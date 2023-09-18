use std::fmt;

use askama::Template;
use diplomat_core::ast;
use diplomat_core::hir;
use heck::ToLowerCamelCase;
use heck::ToUpperCamelCase;

use crate::dotnet::config::BackendConfig;
use crate::dotnet::formatter::DotnetFormatter;
use crate::dotnet::ResultType;

impl<'tcx> super::DotnetContext<'tcx> {
    pub fn raw_gen_ty(&mut self, id: hir::TypeId, ty: hir::TypeDef<'tcx>) {
        if ty.attrs().disable {
            // Skip type if disabled
            return;
        }

        for method in ty.methods() {
            if method.attrs.disable {
                continue;
            }

            let hir::ReturnType::Fallible(ok, err) = &method.output else {
                continue;
            };

            self.store_result(ok.as_ref(), err.as_ref());

            if let Some(err) = err {
                self.store_error(err)
            }
        }

        let _guard = self
            .backend_errors
            .set_context_ty(ty.name().as_str().into());

        let output = match ty {
            hir::TypeDef::Enum(e) => self.raw_render_enum_def(id, e),
            hir::TypeDef::Opaque(o) => self.raw_render_opaque_def(id, o),
            hir::TypeDef::Struct(s) => self.raw_render_struct_def(id, s),
            hir::TypeDef::OutStruct(s) => self.raw_render_struct_def(id, s),
        };

        let source_path = self.fmt.raw_source_path(id);

        self.files.add_file(source_path, output);

        // for method in ty.methods() {
        //     if method.attrs.disable {
        //         // Skip type if disabled
        //         return;
        //     }
        //     let _guard = self.backend_errors.set_context_method(
        //         self.formatter.fmt_type_name_diagnostics(id),
        //         method.name.as_str().into(),
        //     );
        //     context.gen_method(id, method);
        // }
    }

    pub fn raw_gen_result(&self, name: &str, result_ty: &ResultType<'tcx>) {
        let _guard = self
            .backend_errors
            .set_context_ty(self.fmt.fmt_result_for_diagnostics(ty).into());
        let header_path = self.fmt.fmt_result_header_path(name);
        let mut header = Header::new(header_path.clone());
        let mut dummy_header = Header::new("".to_string());
        let mut context = TyGenContext {
            cx: self,
            // NOTE: Only one header for results
            decl_header: &mut header,
            impl_header: &mut dummy_header,
        };
        context.gen_result(name, ty);
        self.files.add_file(header_path, header.to_string());
    }

    fn store_result(
        &mut self,
        ok: Option<&'tcx hir::SuccessType>,
        err: Option<&'tcx hir::OutType>,
    ) {
        let name = self.fmt.result_name(ok, err);
        let result_type = ResultType { ok, err };
        self.result_store.insert(name, result_type);
    }

    fn store_error(&mut self, err: &'tcx hir::OutType) {
        let name = self.fmt.name_for_type(err);
        self.error_ty_store.insert(name.into_owned(), err);
    }

    fn raw_render_enum_def(&mut self, id: hir::TypeId, def: &'tcx hir::EnumDef) -> String {
        // TODO: support enum extensions in order to add enum methods

        struct Enum {
            name: String,
            variants: Vec<Variant>,
        }

        struct Variant {
            name: String,
            discriminant: String,
            docstring: String,
        }

        #[derive(Template)]
        #[template(path = "dotnet/raw_enum.cs", escape = "none")]
        struct RawEnumTemplate<'a> {
            def: &'a hir::EnumDef,
            fmt: &'a DotnetFormatter<'a>,
            config: &'a BackendConfig,
        }

        RawEnumTemplate {
            def,
            fmt: &self.fmt,
            config: &self.config,
        }
        .render()
        .unwrap()
    }

    fn raw_render_opaque_def(&mut self, id: hir::TypeId, def: &'tcx hir::OpaqueDef) -> String {
        // gen_doc_block(
        //     out,
        //     &opaque
        //         .docs
        //         .to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
        // )?;
        // writeln!(out, "[StructLayout(LayoutKind.Sequential)]")?;
        // writeln!(out, "public partial struct {}", typ.name())?;

        // out.scope(|out| {
        //     writeln!(
        //         out,
        //         "private const string NativeLib = \"{}\";",
        //         library_config.native_lib
        //     )?;

        //     for method in typ.methods() {
        //         gen_method(typ, method, in_path, env, docs_url_gen, out)?;
        //     }

        //     writeln!(out)?;
        //     writeln!(
        //         out,
        //         r#"[DllImport(NativeLib, CallingConvention = CallingConvention.Cdecl, EntryPoint = "{}", ExactSpelling = true)]"#,
        //         typ.dtor_name()
        //     )?;
        //     writeln!(out, "public static unsafe extern void Destroy({}* self);", typ.name())
        // })

        todo!()
    }

    fn raw_render_struct_def<P: hir::TyPosition>(
        &mut self,
        id: hir::TypeId,
        def: &'tcx hir::StructDef<P>,
    ) -> String {
        // for (_, typ, _) in &strct.fields {
        //     collect_results(typ, in_path, env, results);
        //     collect_errors(typ, in_path, env, errors);
        // }

        // gen_doc_block(
        //     out,
        //     &strct
        //         .docs
        //         .to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
        // )?;
        // writeln!(out, "[StructLayout(LayoutKind.Sequential)]")?;
        // writeln!(out, "public partial struct {}", typ.name())?;

        // out.scope(|out| {
        //     writeln!(
        //         out,
        //         "private const string NativeLib = \"{}\";",
        //         library_config.native_lib
        //     )?;

        //     for (name, typ, doc) in strct.fields.iter() {
        //         gen_field(name, doc, typ, in_path, env, docs_url_gen, out)?;
        //     }

        //     for method in typ.methods() {
        //         gen_method(typ, method, in_path, env, docs_url_gen, out)?;
        //     }

        //     Ok(())
        // })

        todo!()
    }
}

fn gen_field(
    name: &ast::Ident,
    docs: &ast::Docs,
    typ: &ast::TypeName,
    in_path: &ast::Path,
    env: &Env,
    docs_url_gen: &ast::DocsUrlGenerator,
    out: &mut CodeWriter,
) -> fmt::Result {
    let mut type_declaration = String::new();
    gen_type_name_decl_position(typ, in_path, env, &mut type_declaration)?;
    let is_unsafe = type_declaration.ends_with('*');

    writeln!(out)?;
    gen_doc_block(
        out,
        &docs.to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
    )?;
    gen_annotations_for_field(typ, out)?;
    write!(out, "public ")?;
    if is_unsafe {
        write!(out, "unsafe ")?;
    }
    writeln!(out, "{type_declaration} {name};")
}

fn gen_method(
    typ: &ast::CustomType,
    method: &ast::Method,
    in_path: &ast::Path,
    env: &Env,
    docs_url_gen: &ast::DocsUrlGenerator,
    out: &mut CodeWriter,
) -> fmt::Result {
    writeln!(out)?;

    gen_doc_block(
        out,
        &method
            .docs
            .to_markdown(docs_url_gen, ast::MarkdownStyle::Normal),
    )?;
    gen_annotations_for_method(method, out)?;
    write!(out, "public static unsafe extern ")?;
    gen_type_name_return_position(method.return_type.as_ref(), in_path, env, out)?;

    write!(
        out,
        " {}(",
        method
            .full_path_name
            .as_str()
            .replace(&format!("{}_", typ.name()), "")
            .to_upper_camel_case()
    )?;

    let mut first = true;

    if let Some(ref self_param) = method.self_param {
        gen_param("self", &self_param.to_typename(), false, in_path, env, out)?;
        first = false;
    }

    for param in method.params.iter() {
        if first {
            first = false;
        } else {
            write!(out, ", ")?;
        }

        let name = param.name.as_str().to_lower_camel_case();
        gen_param(&name, &param.ty, param.is_writeable(), in_path, env, out)?;
    }

    writeln!(out, ");")?;

    Ok(())
}

pub fn gen_result(
    typ: &ast::TypeName,
    in_path: &ast::Path,
    env: &Env,
    out: &mut CodeWriter,
) -> fmt::Result {
    let (ok, err) = if let ast::TypeName::Result(ok, err, _) = typ {
        (ok, err)
    } else {
        panic!("not a result: {:?}", typ);
    };

    writeln!(out)?;
    writeln!(out, "[StructLayout(LayoutKind.Sequential)]")?;
    write!(out, "public partial struct ")?;
    gen_type_name(typ, in_path, env, out)?;
    writeln!(out)?;

    out.scope(|out| {
        // Omit variants or even the entire union if parts are zero-sized.
        // This matches what rustc effectively does with zero-sized union variants
        if !ok.is_zst() || !err.is_zst() {
            writeln!(out, "[StructLayout(LayoutKind.Explicit)]")?;
            writeln!(out, "private unsafe struct InnerUnion")?;

            out.scope(|out| {
                if !ok.is_zst() {
                    writeln!(out, "[FieldOffset(0)]")?;
                    write!(out, "internal ")?;
                    gen_type_name_decl_position(ok, in_path, env, out)?;
                    writeln!(out, " ok;")?;
                }

                if !err.is_zst() {
                    writeln!(out, "[FieldOffset(0)]")?;
                    write!(out, "internal ")?;
                    gen_type_name_decl_position(err, in_path, env, out)?;
                    writeln!(out, " err;")?;
                }

                Ok(())
            })?;

            writeln!(out)?;
            writeln!(out, "private InnerUnion _inner;")?;
            writeln!(out)?;
        }

        writeln!(out, "[MarshalAs(UnmanagedType.U1)]")?;
        writeln!(out, "public bool isOk;")?;

        if !ok.is_zst() {
            writeln!(out)?;
            write!(out, "public unsafe ")?;
            gen_type_name_decl_position(ok, in_path, env, out)?;
            writeln!(out, " Ok")?;
            out.scope(|out| {
                writeln!(out, "get")?;
                out.scope(|out| writeln!(out, "return _inner.ok;"))
            })?;
        }

        if !err.is_zst() {
            writeln!(out)?;
            write!(out, "public unsafe ")?;
            gen_type_name_decl_position(err, in_path, env, out)?;
            writeln!(out, " Err")?;
            out.scope(|out| {
                writeln!(out, "get")?;
                out.scope(|out| writeln!(out, "return _inner.err;"))
            })?;
        }

        Ok(())
    })?;

    Ok(())
}

fn gen_annotations_for_method(method: &ast::Method, out: &mut dyn fmt::Write) -> fmt::Result {
    writeln!(
        out,
        r#"[DllImport(NativeLib, CallingConvention = CallingConvention.Cdecl, EntryPoint = "{}", ExactSpelling = true)]"#,
        method.full_path_name
    )?;
    match &method.return_type {
        Some(ast::TypeName::Primitive(ast::PrimitiveType::bool)) => {
            writeln!(out, "[return: MarshalAs(UnmanagedType.U1)]")
        }
        _ => Ok(()),
    }
}

fn gen_annotations_for_param(typ: &ast::TypeName, out: &mut dyn fmt::Write) -> fmt::Result {
    match typ {
        ast::TypeName::Primitive(ast::PrimitiveType::bool) => {
            write!(out, "[MarshalAs(UnmanagedType.U1)] ")
        }
        _ => Ok(()),
    }
}

fn gen_annotations_for_field(typ: &ast::TypeName, out: &mut dyn fmt::Write) -> fmt::Result {
    match typ {
        ast::TypeName::Primitive(ast::PrimitiveType::bool) => {
            writeln!(out, "[MarshalAs(UnmanagedType.U1)]")
        }
        _ => Ok(()),
    }
}

fn gen_type_name_decl_position(
    typ: &ast::TypeName,
    in_path: &ast::Path,
    env: &Env,
    out: &mut dyn fmt::Write,
) -> fmt::Result {
    match typ {
        ast::TypeName::Option(opt) => match opt.as_ref() {
            ast::TypeName::Box(ptr) | ast::TypeName::Reference(.., ptr) => {
                gen_type_name_decl_position(ptr.as_ref(), in_path, env, out)?;
                write!(out, "*")
            }
            _ => write!(out, "Options without a pointer type are not yet supported"),
        },
        ast::TypeName::Box(underlying) | ast::TypeName::Reference(.., underlying) => {
            gen_type_name_decl_position(underlying.as_ref(), in_path, env, out)?;
            write!(out, "*")
        }
        ast::TypeName::Unit => panic!("unexpected unit type in declaration position"),
        _ => gen_type_name(typ, in_path, env, out),
    }
}

fn gen_type_name_return_position<'ast>(
    typ: impl Into<Option<&'ast ast::TypeName>>,
    in_path: &ast::Path,
    env: &Env,
    out: &mut dyn fmt::Write,
) -> fmt::Result {
    match &typ.into() {
        None | Some(ast::TypeName::Unit) => write!(out, "void"),
        Some(other) => gen_type_name_decl_position(other, in_path, env, out),
    }
}

fn gen_param(
    name: &str,
    typ: &ast::TypeName,
    is_writeable: bool,
    in_path: &ast::Path,
    env: &Env,
    out: &mut dyn fmt::Write,
) -> fmt::Result {
    if is_writeable {
        write!(out, "DiplomatWriteable* {name}")
    } else {
        match typ {
            ast::TypeName::StrReference(_, ast::StringEncoding::UnvalidatedUtf8) => {
                write!(out, "byte* {name}, nuint {name}Sz")
            }
            ast::TypeName::StrReference(..) => {
                write!(out, "ushort* {name}, nuint {name}Sz")
            }
            ast::TypeName::PrimitiveSlice(.., prim) => {
                write!(
                    out,
                    "{}* {name}, nuint {name}Sz",
                    super::types::type_name_for_prim(prim),
                )
            }
            ast::TypeName::Option(opt) => gen_param(name, opt.as_ref(), false, in_path, env, out),
            _ => {
                gen_annotations_for_param(typ, out)?;
                gen_type_name_decl_position(typ, in_path, env, out)?;
                write!(out, " {name}")
            }
        }
    }
}
