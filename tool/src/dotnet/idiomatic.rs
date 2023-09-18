use std::fmt;
use std::fmt::Write as _;

use colored::Colorize as _;
use diplomat_core::{ast, Env};
use heck::{ToLowerCamelCase as _, ToUpperCamelCase as _};

use super::config::BackendConfig;
use super::conversions::{to_idiomatic_object, to_raw_object, SliceParam};
use super::types::{gen_type_name, gen_type_name_to_string};
use super::util::gen_doc_block;
use crate::util::CodeWriter;
use std::collections::{HashMap, HashSet};

pub fn gen(
    custom_type: &ast::CustomType,
    in_path: &ast::Path,
    env: &Env,
    library_config: &BackendConfig,
    docs_url_gen: &ast::DocsUrlGenerator,
    out: &mut CodeWriter,
) -> fmt::Result {
    writeln!(out)?;

    match custom_type {
        ast::CustomType::Opaque(opaque) => {
            gen_doc_block(
                out,
                &opaque
                    .docs
                    .to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
            )?;
            writeln!(
                out,
                "public partial class {}: IDisposable",
                custom_type.name()
            )?;

            out.scope(|out| {
                writeln!(out, "private unsafe Raw.{}* _inner;", opaque.name)?;

                let properties = collect_properties(&opaque.methods, in_path, env, library_config);
                for property in properties {
                    gen_property_for_getters_setters(&property, out)?;
                }

                writeln!(out)?;
                writeln!(out, "/// <summary>")?;
                writeln!(out, "/// Creates a managed <c>{}</c> from a raw handle.", opaque.name)?;
                writeln!(out, "/// </summary>")?;
                writeln!(out, "/// <remarks>")?;
                writeln!(out, "/// Safety: you should not build two managed objects using the same raw handle (may causes use-after-free and double-free).")?;
                writeln!(out, "/// <br/>")?;
                writeln!(out, "/// This constructor assumes the raw struct is allocated on Rust side.")?;
                writeln!(out, "/// If implemented, the custom Drop implementation on Rust side WILL run on destruction.")?;
                writeln!(out, "/// </remarks>")?;
                writeln!(out, "public unsafe {0}(Raw.{0}* handle)", opaque.name)?;
                out.scope(|out| writeln!(out, "_inner = handle;"))?;

                for method in &opaque.methods {
                    gen_method(custom_type, method, in_path, true, env, library_config, docs_url_gen, out)?;
                }

                writeln!(out)?;
                gen_doc_block(out, "Returns the underlying raw handle.")?;
                writeln!(out, "public unsafe Raw.{}* AsFFI()", opaque.name)?;
                out.scope(|out| writeln!(out, "return _inner;"))?;

                writeln!(out)?;
                gen_doc_block(out, "Destroys the underlying object immediately.")?;
                writeln!(out, "public void Dispose()")?;
                out.scope(|out| {
                    writeln!(out, "unsafe")?;
                    out.scope(|out| {
                        writeln!(out, "if (_inner == null)")?;
                        out.scope(|out| writeln!(out, "return;"))?;

                        writeln!(out)?;
                        writeln!(out, "Raw.{}.Destroy(_inner);", opaque.name)?;
                        writeln!(out, "_inner = null;")?;
                        writeln!(out)?;
                        writeln!(out, "GC.SuppressFinalize(this);")
                    })
                })?;

                writeln!(out)?;
                writeln!(out, "~{}()", opaque.name)?;
                out.scope(|out| {
                    writeln!(out, "Dispose();")
                })
            })?;
        }

        ast::CustomType::Struct(strct) => {
            gen_doc_block(
                out,
                &strct
                    .docs
                    .to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
            )?;
            writeln!(out, "public partial class {}", custom_type.name())?;

            out.scope(|out| {
                writeln!(out, "private Raw.{} _inner;", strct.name)?;

                for (name, typ, doc) in strct.fields.iter() {
                    gen_property_for_field(name, doc, typ, in_path, env, docs_url_gen, out)?;
                }

                let properties = collect_properties(&strct.methods, in_path, env, library_config);
                for property in properties {
                    gen_property_for_getters_setters(&property, out)?;
                }

                writeln!(out)?;
                writeln!(out, "/// <summary>")?;
                writeln!(
                    out,
                    "/// Creates a managed <c>{}</c> from the raw representation.",
                    strct.name
                )?;
                writeln!(out, "/// </summary>")?;
                writeln!(out, "public unsafe {0}(Raw.{0} data)", strct.name)?;
                out.scope(|out| writeln!(out, "_inner = data;"))?;

                for method in &strct.methods {
                    gen_method(
                        custom_type,
                        method,
                        in_path,
                        true,
                        env,
                        library_config,
                        docs_url_gen,
                        out,
                    )?;
                }

                writeln!(out)?;
                gen_doc_block(out, "Returns a copy of the underlying raw representation.")?;
                writeln!(out, "public Raw.{} AsFFI()", strct.name)?;
                out.scope(|out| writeln!(out, "return _inner;"))
            })?;
        }

        ast::CustomType::Enum(enm) => {
            gen_doc_block(
                out,
                &enm.docs
                    .to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
            )?;
            writeln!(out, "public enum {}", enm.name)?;
            out.scope(|out| {
                for (name, discriminant, docs, _attrs) in enm.variants.iter() {
                    gen_doc_block(
                        out,
                        &docs.to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
                    )?;
                    writeln!(out, "{name} = {discriminant},")?;
                }

                Ok(())
            })?;
        }
        &_ => unreachable!("unknown AST/HIR variant"),
    }

    Ok(())
}

fn gen_property_for_field(
    name: &ast::Ident,
    docs: &ast::Docs,
    typ: &ast::TypeName,
    in_path: &ast::Path,
    env: &Env,
    docs_url_gen: &ast::DocsUrlGenerator,
    out: &mut CodeWriter,
) -> fmt::Result {
    match typ {
        ast::TypeName::Primitive(_) => {}
        ast::TypeName::Named(path_type) | ast::TypeName::SelfType(path_type) => {
            match path_type.resolve(in_path, env) {
                ast::CustomType::Struct(_) | ast::CustomType::Opaque(_) => {
                    println!(
                        "{} ({name})",
                        "[WARNING] C# properties for non primitive types are not supported yet"
                            .yellow(),
                    );
                    return Ok(());
                }
                ast::CustomType::Enum(_) => {}
                &_ => unreachable!("unknown AST/HIR variant"),
            }
        }
        _ => {
            println!(
                "{} ({name})",
                "[WARNING] C# properties for non primitive types are not supported yet".yellow(),
            );
            return Ok(());
        }
    }

    writeln!(out)?;
    gen_doc_block(
        out,
        &docs.to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
    )?;

    let type_name = gen_type_name_to_string(typ, in_path, env)?;
    let property_name = name.as_str().to_upper_camel_case();
    let var_to_raw = format!("_inner.{name}");

    writeln!(out, "public {type_name} {property_name}")?;
    out.scope(|out| {
        writeln!(out, "get")?;
        out.scope(|out| {
            writeln!(out, "unsafe")?;
            out.scope(|out| {
                write!(out, "return ")?;
                to_idiomatic_object(env, typ, in_path, &var_to_raw, out)?;
                writeln!(out, ";")
            })
        })?;

        writeln!(out, "set")?;
        out.scope(|out| {
            writeln!(out, "unsafe")?;
            out.scope(|out| {
                write!(out, "{var_to_raw} = ")?;
                to_raw_object(env, typ, in_path, "value", out)?;
                writeln!(out, ";")
            })
        })
    })?;

    Ok(())
}

fn gen_property_for_getters_setters(
    property: &(Property, Option<Getter>, Option<Setter>),
    out: &mut CodeWriter,
) -> fmt::Result {
    let (property, getter, setter) = property;
    let property_name = &property.name;

    let return_type = match (getter, setter) {
        (None, None) => unreachable!("unexpected branch reached"),
        (None, Some(setter)) => &setter.param_type,
        (Some(getter), None) => &getter.return_type,
        (Some(getter), Some(setter)) => {
            if setter.param_type != getter.return_type {
                panic!(
                    "Found two different types for same property {property_name} ({} != {})",
                    setter.param_type, getter.return_type
                );
            }
            &getter.return_type
        }
    };

    writeln!(out)?;
    writeln!(out, "public {return_type} {property_name}")?;
    out.scope(|out| {
        if let Some(getter) = getter {
            let getter_name = &getter.name;
            writeln!(out, "get")?;
            out.scope(|out| writeln!(out, "return {getter_name}();"))?;
        }

        if let Some(setter) = setter {
            let setter_name = &setter.name;
            writeln!(out, "set")?;
            out.scope(|out| writeln!(out, "{setter_name}(value);"))?;
        }

        Ok(())
    })
}

#[allow(clippy::too_many_arguments)]
fn gen_method(
    enclosing_type: &ast::CustomType,
    method: &ast::Method,
    in_path: &ast::Path,
    writeable_to_string: bool,
    env: &Env,
    library_config: &BackendConfig,
    docs_url_gen: &ast::DocsUrlGenerator,
    out: &mut CodeWriter,
) -> fmt::Result {
    if method.attrs.skip_if_unsupported
        && matches!(method.return_type, Some(ast::TypeName::Reference(..)))
    {
        // We don't support returning references
        return Ok(());
    }
    // This method should rearrange the writeable
    let rearranged_writeable = method.is_writeable_out() && writeable_to_string;

    if rearranged_writeable {
        // generate the normal method too
        gen_method(
            enclosing_type,
            method,
            in_path,
            false,
            env,
            library_config,
            docs_url_gen,
            out,
        )?;
    }

    writeln!(out)?;

    gen_doc_block(
        out,
        &method
            .docs
            .to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
    )?;

    let result_to_handle: Option<(&ast::TypeName, &ast::TypeName)> = match &method.return_type {
        Some(ast::TypeName::Result(ok_variant, err_variant, _)) => {
            let exception_name = if err_variant.is_zst() {
                "DiplomatOpaqueException".to_owned()
            } else {
                error_type_to_exception_name(env, library_config, err_variant, in_path)?.name
            };
            writeln!(
                out,
                r#"/// <exception cref="{exception_name}"></exception>"#
            )?;
            Some((ok_variant, err_variant))
        }
        _ => None,
    };

    if let Some(ret_ty) = &method.return_type {
        gen_return_type_remark_about_drop(ret_ty, in_path, env, out)?;
    }

    write!(out, "public ")?;
    if method.self_param.is_none() {
        write!(out, "static ")?;
    }
    if rearranged_writeable {
        write!(out, "string ")?;
    } else {
        gen_type_name_return_position(&method.return_type, in_path, env, out)?;
        write!(out, " ")?;
    }
    write!(out, "{}(", method.name.as_str().to_upper_camel_case())?;

    let mut params_to_gen = method.params.clone();
    if rearranged_writeable {
        params_to_gen.remove(params_to_gen.len() - 1);
    }

    let mut params_str_ref = vec![];
    let mut params_slice = vec![];
    let mut params_custom_types = vec![];
    let mut all_params_invocation = vec![];

    if method.self_param.is_some() {
        all_params_invocation.push("_inner".to_owned());
    }

    for (i, param) in params_to_gen.iter().enumerate() {
        if i != 0 {
            write!(out, ", ")?;
        }

        let name = param.name.as_str().to_lower_camel_case();

        if let ast::TypeName::StrReference(..) = param.ty {
            params_str_ref.push(name.clone());
            all_params_invocation.push(format!("{name}BufPtr"));
            all_params_invocation.push(format!("{name}BufLength"));
        } else if let ast::TypeName::PrimitiveSlice(.., prim) = param.ty {
            params_slice.push(SliceParam::new(name.clone(), prim));
            all_params_invocation.push(format!("{name}Ptr"));
            all_params_invocation.push(format!("{name}Length"));
        } else if param.is_writeable() {
            all_params_invocation.push(format!("&{name}"));
        } else if let ast::TypeName::Primitive(_) = param.ty {
            all_params_invocation.push(name.clone());
        } else {
            params_custom_types.push(param.clone());
            all_params_invocation.push(format!("{name}Raw"));
        }

        gen_type_name_decl_position(&param.ty, in_path, env, out)?;
        write!(out, " {name}")?;
    }

    writeln!(out, ")")?;

    out.scope(|out| {
        writeln!(out, "unsafe")?;
        out.scope(|out| {
            if method.self_param.is_some() {
                writeln!(out, "if (_inner == null)")?;
                out.scope(|out| {
                    writeln!(
                        out,
                        r#"throw new ObjectDisposedException("{}");"#,
                        enclosing_type.name()
                    )
                })?;
            }

            for str_ref in params_str_ref {
                let name = format!("{str_ref}Buf");
                writeln!(
                    out,
                    r#"byte[] {name} = DiplomatUtils.StringToUtf8({str_ref});"#
                )?;
                params_slice.push(SliceParam::new(name, ast::PrimitiveType::u8));
            }

            for param in &params_slice {
                writeln!(
                    out,
                    r#"nuint {} = (nuint){}.Length;"#,
                    param.length_var_name, param.array_var_name,
                )?;
            }

            for param in &params_custom_types {
                let param_name = param.name.as_str().to_lower_camel_case();
                let raw_var_name = format!("{param_name}Raw");
                let mut raw_type_name = String::new();
                gen_raw_conversion_type_name_decl_position(
                    &param.ty,
                    in_path,
                    env,
                    &mut raw_type_name,
                )?;

                writeln!(out, "{raw_type_name} {raw_var_name};")?;

                if let ast::TypeName::Option(underlying_ty) = &param.ty {
                    // TODO: support optional primitive types and enums in arguments
                    match underlying_ty.as_ref() {
                        ast::TypeName::Named(path_type) | ast::TypeName::SelfType(path_type) => {
                            if let ast::CustomType::Enum(_) = path_type.resolve(in_path, env) {
                                panic!("Optional enum types as parameters are not supported yet")
                            }
                        }
                        ast::TypeName::Primitive(_) => {
                            panic!("Optional primitive types as paramaters are not supported yet");
                        }
                        _ => {}
                    }

                    writeln!(out, "if ({param_name} == null)")?;
                    out.scope(|out| writeln!(out, "{raw_var_name} = null;"))?;
                    writeln!(out, "else")?;
                    out.scope(|out| {
                        write!(out, "{raw_var_name} = ")?;
                        to_raw_object(env, &param.ty, in_path, &param_name, out)?;
                        writeln!(out, ";")?;
                        insert_null_check(&raw_var_name, &param.ty, in_path, env, out)
                    })?;
                } else {
                    write!(out, "{raw_var_name} = ")?;
                    to_raw_object(env, &param.ty, in_path, &param_name, out)?;
                    writeln!(out, ";")?;
                    insert_null_check(&raw_var_name, &param.ty, in_path, env, out)?;
                }
            }

            for param in &params_slice {
                param.open_fixed_block(out)?;
            }

            if rearranged_writeable {
                writeln!(
                    out,
                    "DiplomatWriteable writeable = new DiplomatWriteable();"
                )?;
                all_params_invocation.push("&writeable".to_owned());
            }

            let ret_typ = match &method.return_type {
                None | Some(ast::TypeName::Unit) => &ast::TypeName::Unit,
                Some(ast::TypeName::Result(ok_variant, ..)) => ok_variant,
                Some(typ) => typ,
            };

            match &method.return_type {
                None | Some(ast::TypeName::Unit) => {}
                Some(typ @ ast::TypeName::Result(..)) => {
                    gen_raw_type_name_decl_position(typ, in_path, env, out)?;
                    write!(out, " result = ")?;
                }
                Some(typ) => {
                    gen_raw_type_name_decl_position(typ, in_path, env, out)?;
                    write!(out, " retVal = ")?;
                }
            }

            write!(
                out,
                "Raw.{}.{}(",
                enclosing_type.name(),
                method.name.as_str().to_upper_camel_case()
            )?;
            for (i, param) in all_params_invocation.into_iter().enumerate() {
                if i != 0 {
                    write!(out, ", ")?;
                }
                write!(out, "{param}")?;
            }
            writeln!(out, ");")?;

            if let Some((_, err_variant)) = result_to_handle {
                writeln!(out, "if (!result.isOk)")?;
                out.scope(|out| {
                    if err_variant.is_zst() {
                        writeln!(out, "throw new DiplomatOpaqueException();")
                    } else {
                        let ExceptionCtx {
                            name: exception_name,
                            ..
                        } = error_type_to_exception_name(
                            env,
                            library_config,
                            err_variant,
                            in_path,
                        )?;
                        write!(out, "throw new {exception_name}(")?;
                        to_idiomatic_object(env, err_variant, in_path, "result.Err", out)?;
                        writeln!(out, ");")
                    }
                })?;

                match ret_typ {
                    ast::TypeName::Unit => {}
                    _ => {
                        gen_raw_type_name_decl_position(ret_typ, in_path, env, out)?;
                        writeln!(out, " retVal = result.Ok;")?;
                    }
                }
            }

            if rearranged_writeable {
                writeln!(out, "string retVal = writeable.ToUnicode();")?;
                writeln!(out, "writeable.Dispose();")?;
                writeln!(out, "return retVal;")?;
            } else {
                match ret_typ {
                    ast::TypeName::Unit => {}
                    ast::TypeName::Option(underlying) => {
                        writeln!(out, "if (retVal == null)")?;
                        out.scope(|out| writeln!(out, "return null;"))?;

                        match underlying.as_ref() {
                            ast::TypeName::Unit => {}
                            typ => {
                                write!(out, "return ")?;
                                to_idiomatic_object(env, typ, in_path, "retVal", out)?;
                                writeln!(out, ";")?
                            }
                        }
                    }
                    _ => {
                        write!(out, "return ")?;
                        to_idiomatic_object(env, ret_typ, in_path, "retVal", out)?;
                        writeln!(out, ";")?
                    }
                }
            }

            for param in params_slice {
                param.close_fixed_block(out)?;
            }

            Ok(())
        })
    })
}

pub struct ExceptionCtx {
    pub error_name: String,
    pub trimmed_error_name: String,
    pub name: String,
}

pub fn error_type_to_exception_name(
    env: &Env,
    library_config: &BackendConfig,
    error_typ: &ast::TypeName,
    in_path: &ast::Path,
) -> Result<ExceptionCtx, fmt::Error> {
    let error_name = gen_type_name_to_string(error_typ, in_path, env)?;
    let trimmed_error_name = error_name
        .trim_end_matches(&library_config.exceptions.trim_suffix)
        .to_upper_camel_case();
    let exception_name = format!("{trimmed_error_name}Exception");

    Ok(ExceptionCtx {
        error_name,
        trimmed_error_name,
        name: exception_name,
    })
}

pub fn gen_exception(
    env: &Env,
    library_config: &BackendConfig,
    error_typ: &ast::TypeName,
    in_path: &ast::Path,
    out: &mut CodeWriter,
) -> fmt::Result {
    let ExceptionCtx {
        error_name,
        trimmed_error_name,
        name: exception_name,
    } = error_type_to_exception_name(env, library_config, error_typ, in_path)?;

    let error_str = format!("{trimmed_error_name} error occurred");

    writeln!(out)?;
    writeln!(out, "public partial class {exception_name} : Exception")?;
    out.scope(|out| {
        writeln!(out, "private {error_name} _inner;")?;

        writeln!(out)?;
        write!(out, "public {exception_name}({error_name} inner) : base(")?;
        if let Some(method) = &library_config.exceptions.error_message_method {
            writeln!(out, "inner.{method}())")?;
        } else {
            writeln!(out, r#""{error_str}")"#)?;
        }
        out.scope(|out| writeln!(out, "_inner = inner;"))?;

        writeln!(out)?;
        writeln!(out, "public {error_name} Inner")?;
        out.scope(|out| {
            writeln!(out, "get")?;
            out.scope(|out| writeln!(out, "return _inner;"))
        })
    })?;

    Ok(())
}

fn gen_type_name_decl_position(
    typ: &ast::TypeName,
    in_path: &ast::Path,
    env: &Env,
    out: &mut dyn fmt::Write,
) -> fmt::Result {
    match typ {
        ast::TypeName::Option(underlying) => {
            gen_type_name(underlying.as_ref(), in_path, env, out)?;
            write!(out, "?")
        }
        _ => gen_type_name(typ, in_path, env, out),
    }
}

fn gen_raw_type_name_decl_position(
    typ: &ast::TypeName,
    in_path: &ast::Path,
    env: &Env,
    out: &mut dyn fmt::Write,
) -> fmt::Result {
    match typ {
        ast::TypeName::Primitive(_) => gen_type_name(typ, in_path, env, out),
        ast::TypeName::Option(opt) => match opt.as_ref() {
            ast::TypeName::Box(ptr) | ast::TypeName::Reference(.., ptr) => {
                gen_raw_type_name_decl_position(ptr.as_ref(), in_path, env, out)?;
                write!(out, "*")
            }
            _ => write!(out, "Options without a pointer type are not yet supported"),
        },
        ast::TypeName::Box(underlying) | ast::TypeName::Reference(.., underlying) => {
            gen_raw_type_name_decl_position(underlying.as_ref(), in_path, env, out)?;
            write!(out, "*")
        }
        ast::TypeName::Unit => panic!("unexpected unit type in parameter"),
        _ => {
            write!(out, "Raw.")?;
            gen_type_name(typ, in_path, env, out)
        }
    }
}

fn gen_raw_conversion_type_name_decl_position(
    typ: &ast::TypeName,
    in_path: &ast::Path,
    env: &Env,
    out: &mut dyn fmt::Write,
) -> fmt::Result {
    match typ {
        ast::TypeName::Named(path_type) | ast::TypeName::SelfType(path_type) => {
            match path_type.resolve(in_path, env) {
                ast::CustomType::Opaque(_) => {
                    write!(out, "Raw.")?;
                    gen_type_name(typ, in_path, env, out)?;
                    write!(out, "*")
                }
                ast::CustomType::Enum(_) | ast::CustomType::Struct(_) => {
                    gen_raw_type_name_decl_position(typ, in_path, env, out)
                }
                &_ => unreachable!("unknown AST/HIR variant"),
            }
        }
        _ => gen_raw_type_name_decl_position(typ, in_path, env, out),
    }
}

fn gen_type_name_return_position<'ast>(
    typ: impl Into<Option<&'ast ast::TypeName>>,
    in_path: &ast::Path,
    env: &Env,
    out: &mut dyn fmt::Write,
) -> fmt::Result {
    match &typ.into() {
        Some(ast::TypeName::Result(ok, _, _)) => gen_type_name(ok, in_path, env, out),
        Some(ast::TypeName::Option(underlying)) => {
            gen_type_name(underlying.as_ref(), in_path, env, out)?;
            write!(out, "?")
        }
        Some(other) => gen_type_name(other, in_path, env, out),
        None => write!(out, "void"),
    }
}

fn gen_return_type_remark_about_drop(
    typ: &ast::TypeName,
    in_path: &ast::Path,
    env: &Env,
    out: &mut CodeWriter,
) -> fmt::Result {
    match typ {
        ast::TypeName::Named(_) | ast::TypeName::SelfType(_) => {
            let type_name = gen_type_name_to_string(typ, in_path, env)?;
            writeln!(out, "/// <returns>")?;
            writeln!(out, "/// A <c>{type_name}</c> allocated on C# side.")?;
            writeln!(out, "/// </returns>")
        }
        ast::TypeName::Box(underlying) | ast::TypeName::Reference(.., underlying) => {
            match underlying.as_ref() {
                ast::TypeName::Named(_) | ast::TypeName::SelfType(_) => {
                    let type_name = gen_type_name_to_string(underlying, in_path, env)?;
                    writeln!(out, "/// <returns>")?;
                    writeln!(out, "/// A <c>{type_name}</c> allocated on Rust side.")?;
                    writeln!(out, "/// </returns>")
                }
                _ => gen_return_type_remark_about_drop(underlying, in_path, env, out),
            }
        }
        ast::TypeName::Result(underlying, _, _) | ast::TypeName::Option(underlying) => {
            gen_return_type_remark_about_drop(underlying, in_path, env, out)
        }
        _ => Ok(()),
    }
}

fn requires_null_check(typ: &ast::TypeName, in_path: &ast::Path, env: &Env) -> bool {
    match typ {
        ast::TypeName::Primitive(_) => false,
        ast::TypeName::Box(boxed) => requires_null_check(boxed.as_ref(), in_path, env),
        ast::TypeName::Reference(.., reference) => {
            requires_null_check(reference.as_ref(), in_path, env)
        }
        ast::TypeName::Option(opt) => requires_null_check(opt.as_ref(), in_path, env),
        _ => match typ {
            ast::TypeName::Named(path_type) | ast::TypeName::SelfType(path_type) => {
                match path_type.resolve(in_path, env) {
                    ast::CustomType::Opaque(_) => true,
                    ast::CustomType::Struct(_) | ast::CustomType::Enum(_) => false,
                    &_ => unreachable!("unknown AST/HIR variant"),
                }
            }
            other => panic!("expected named type name, found `{}`", other),
        },
    }
}

fn insert_null_check(
    var_name: &str,
    typ: &ast::TypeName,
    in_path: &ast::Path,
    env: &Env,
    out: &mut CodeWriter,
) -> fmt::Result {
    if requires_null_check(typ, in_path, env) {
        let type_name = gen_type_name_to_string(typ, in_path, env)?;
        writeln!(out, "if ({var_name} == null)")?;
        out.scope(|out| writeln!(out, r#"throw new ObjectDisposedException("{type_name}");"#))?;
    }

    Ok(())
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct Property {
    name: String,
}

struct Getter {
    name: ast::Ident,
    return_type: String,
}

struct Setter {
    name: ast::Ident,
    param_type: String,
}

fn extract_getter_metadata(
    method: &ast::Method,
    in_path: &ast::Path,
    env: &Env,
    library_config: &BackendConfig,
) -> Option<(Property, Getter)> {
    let prefix = library_config.properties.getters_prefix.as_deref()?;

    let property_name = method
        .name
        .as_str()
        .strip_prefix(prefix)?
        .to_upper_camel_case();

    method.self_param.as_ref()?;

    let return_type = if method.is_writeable_out() {
        if method.params.len() > 1 {
            return None;
        }

        "string".to_owned()
    } else {
        if !method.params.is_empty() {
            return None;
        }

        let mut out = String::new();
        gen_type_name_return_position(&method.return_type, in_path, env, &mut out).ok()?;
        out
    };

    Some((
        Property {
            name: property_name,
        },
        Getter {
            name: ast::Ident::from(method.name.as_str().to_upper_camel_case()),
            return_type,
        },
    ))
}

fn extract_setter_metadata(
    method: &ast::Method,
    in_path: &ast::Path,
    env: &Env,
    library_config: &BackendConfig,
) -> Option<(Property, Setter)> {
    let prefix = library_config.properties.setters_prefix.as_deref()?;

    let property_name = method
        .name
        .as_str()
        .strip_prefix(prefix)?
        .to_upper_camel_case();

    method.self_param.as_ref()?;

    if method.params.len() > 1 {
        return None;
    }

    let first_arg = method.params.first()?;

    let mut param_type = String::new();
    gen_type_name_decl_position(&first_arg.ty, in_path, env, &mut param_type).ok()?;

    Some((
        Property {
            name: property_name,
        },
        Setter {
            name: ast::Ident::from(method.name.as_str().to_upper_camel_case()),
            param_type,
        },
    ))
}

fn collect_properties(
    methods: &[ast::Method],
    in_path: &ast::Path,
    env: &Env,
    library_config: &BackendConfig,
) -> Vec<(Property, Option<Getter>, Option<Setter>)> {
    let mut properties = HashSet::<Property>::new();
    let mut getters = HashMap::<Property, Getter>::new();
    let mut setters = HashMap::<Property, Setter>::new();

    for method in methods {
        if let Some((property, getter)) =
            extract_getter_metadata(method, in_path, env, library_config)
        {
            properties.insert(property.clone());
            getters.insert(property, getter);
        } else if let Some((property, setter)) =
            extract_setter_metadata(method, in_path, env, library_config)
        {
            properties.insert(property.clone());
            setters.insert(property, setter);
        }
    }

    let mut properties: Vec<(Property, Option<Getter>, Option<Setter>)> = properties
        .into_iter()
        .map(|prop| {
            let getter = getters.remove(&prop);
            let setter = setters.remove(&prop);
            (prop, getter, setter)
        })
        .collect();

    properties.sort_by(|a, b| a.0.name.partial_cmp(&b.0.name).unwrap());

    properties
}
