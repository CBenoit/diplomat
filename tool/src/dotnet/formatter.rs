use std::borrow::Cow;

use diplomat_core::{
    ast,
    hir::{self, OpaqueOwner as _},
};
use heck::{ToLowerCamelCase as _, ToUpperCamelCase as _};

use crate::c2::CFormatter;

pub struct DotnetFormatter<'tcx> {
    c: CFormatter<'tcx>,
    docs_url_gen: &'tcx diplomat_core::ast::DocsUrlGenerator,
}

impl<'tcx> DotnetFormatter<'tcx> {
    pub fn new(
        tcx: &'tcx hir::TypeContext,
        docs_url_gen: &'tcx diplomat_core::ast::DocsUrlGenerator,
    ) -> Self {
        Self {
            c: CFormatter::new(tcx),
            docs_url_gen,
        }
    }

    /// Generates an identifier that uniquely identifies the given *dotnet* type.
    /// Rust types that map to the same dotnet type will get the same dotnet identifier
    /// (e.g. &mut Foo and Option<&mut Foo> are all the same)
    ///
    /// This is primarily used for generating structs for "raw" result types on dotnet side.
    pub fn identifier_for_type<P: hir::TyPosition>(
        &self,
        ty: &'tcx hir::Type<P>,
    ) -> Cow<'tcx, str> {
        match ty {
            hir::Type::Primitive(p) => self.primitive_as_dotnet(*p).to_upper_camel_case().into(),
            hir::Type::Opaque(o) => {
                let o_name = self.name_for_type_id(o.tcx_id.into());

                let ownership = match o.owner.mutability() {
                    None => "Box",
                    Some(hir::Mutability::Mutable) => "RefMut",
                    Some(hir::Mutability::Immutable) => "Ref",
                };

                Cow::Owned(format!("{ownership}{o_name}"))
            }
            hir::Type::Struct(s) => self.name_for_type_id(P::id_for_path(s)),
            hir::Type::Enum(e) => self.name_for_type_id(e.tcx_id.into()),
            hir::Type::Slice(hir::Slice::Str(_)) => Cow::Borrowed("StrRef"),
            hir::Type::Slice(hir::Slice::Primitive(borrow, p)) => {
                let constness = borrow.mutability.if_mut_else("Mut", "Const");
                let prim = self.primitive_as_dotnet(*p).to_upper_camel_case();
                Cow::Owned(format!("Ref{constness}PrimSlice{prim}"))
            }
        }
    }

    pub fn name_for_type<P: hir::TyPosition>(&self, ty: &'tcx hir::Type<P>) -> Cow<'tcx, str> {
        match ty {
            hir::Type::Primitive(p) => self.primitive_as_dotnet(*p).into(),
            hir::Type::Opaque(o) => self.name_for_type_id(o.tcx_id.into()),
            hir::Type::Struct(s) => self.name_for_type_id(P::id_for_path(s)),
            hir::Type::Enum(e) => self.name_for_type_id(e.tcx_id.into()),
            hir::Type::Slice(hir::Slice::Str(_)) => Cow::Borrowed("string"),
            hir::Type::Slice(hir::Slice::Primitive(_, p)) => {
                Cow::Owned(format!("{}[]", self.primitive_as_dotnet(*p)))
            }
        }
    }

    /// Resolves and formats a named type for use in code
    pub fn name_for_type_id(&self, id: hir::TypeId) -> Cow<'tcx, str> {
        let resolved = self.c.tcx().resolve_type(id);

        if let Some(rename) = resolved.attrs().rename.as_deref() {
            Cow::Borrowed(rename)
        } else {
            Cow::Owned(resolved.name().as_str().to_upper_camel_case())
        }
    }

    pub fn enum_name(&self, def: &'tcx hir::EnumDef) -> Cow<'tcx, str> {
        if let Some(rename) = def.attrs.rename.as_deref() {
            Cow::Borrowed(rename)
        } else {
            Cow::Owned(def.name.as_str().to_upper_camel_case())
        }
    }

    /// Resolve and format a named type for use in diagnostics
    /// (don't apply rename rules and such)
    pub fn type_name_diagnostics(&self, id: hir::TypeId) -> Cow<'tcx, str> {
        self.c.fmt_type_name_diagnostics(id)
    }

    pub fn raw_source_path(&self, id: hir::TypeId) -> String {
        let type_name = self.name_for_type_id(id);
        format!("Raw{type_name}.cs")
    }

    pub fn idiomatic_source_path(&self, id: hir::TypeId) -> String {
        let type_name = self.name_for_type_id(id);
        format!("{type_name}.cs")
    }

    pub fn result_name(
        &self,
        ok: Option<&'tcx hir::SuccessType>,
        err: Option<&'tcx hir::OutType>,
    ) -> String {
        let ok_ty_name = ok
            .and_then(|ty| match ty {
                hir::SuccessType::Writeable => None,
                hir::SuccessType::OutType(out_ty) => Some(self.identifier_for_type(out_ty)),
            })
            .unwrap_or(Cow::Borrowed("Void"));

        let err_ty_name = err
            .map(|out_ty| self.identifier_for_type(out_ty))
            .unwrap_or(Cow::Borrowed("Void"));

        format!("Result{}{}", ok_ty_name, err_ty_name)
    }

    /// Format an enum variant.
    pub fn enum_variant_name(&self, variant: &'tcx hir::EnumVariant) -> &'tcx str {
        if let Some(rename) = variant.attrs.rename.as_deref() {
            rename
        } else {
            variant.name.as_str()
        }
    }

    pub fn ptr(&self, ident: &str) -> String {
        format!("{ident}*")
    }

    pub fn optional(&self, ident: &str) -> String {
        format!("{ident}?")
    }

    /// Format a method
    pub fn method_name<'a>(&self, method: &'a hir::Method) -> Cow<'a, str> {
        if let Some(rename) = method.attrs.rename.as_ref() {
            rename.into()
        } else {
            Cow::Owned(method.name.as_str().to_upper_camel_case())
        }
    }

    pub fn param_name(&self, ident: &str) -> String {
        ident.to_lower_camel_case()
    }

    pub fn instance_variable_name(&self, ident: &str) -> String {
        ident.to_lower_camel_case()
    }

    pub fn property_name(&self, ident: &str) -> String {
        ident.to_upper_camel_case()
    }

    pub fn c_method_name<'a>(&self, ty: hir::TypeId, method: &'a hir::Method) -> String {
        self.c.fmt_method_name(ty, method)
    }

    /// Get the primitive type as a dotnet type
    pub fn primitive_as_dotnet(&self, prim: hir::PrimitiveType) -> &'static str {
        use diplomat_core::hir::{FloatType, Int128Type, IntSizeType, IntType, PrimitiveType};

        match prim {
            PrimitiveType::Bool => "bool",
            PrimitiveType::Char => "uint",
            PrimitiveType::Int(IntType::I8) => "sbyte",
            PrimitiveType::Int(IntType::U8) => "byte",
            PrimitiveType::Int(IntType::I16) => "short",
            PrimitiveType::Int(IntType::U16) => "ushort",
            PrimitiveType::Int(IntType::I32) => "int",
            PrimitiveType::Int(IntType::U32) => "uint",
            PrimitiveType::Int(IntType::I64) => "long",
            PrimitiveType::Int(IntType::U64) => "ulong",
            PrimitiveType::Int128(Int128Type::I128) => "Int128",
            PrimitiveType::Int128(Int128Type::U128) => "UInt128",
            PrimitiveType::IntSize(IntSizeType::Isize) => "nint",
            PrimitiveType::IntSize(IntSizeType::Usize) => "nuint",
            PrimitiveType::Float(FloatType::F32) => "float",
            PrimitiveType::Float(FloatType::F64) => "double",
        }
    }

    pub fn docstring(&self, docs: &ast::Docs) -> String {
        let mut out = String::new();

        gen_doc_block(
            &mut out,
            &docs.to_markdown(&self.docs_url_gen, ast::MarkdownStyle::Normal),
        )
        .unwrap();

        out
    }
}

fn gen_doc_block(out: &mut String, comment: &str) -> core::fmt::Result {
    use core::fmt::Write as _;

    if !comment.is_empty() {
        let mut summary_is_open = true;
        writeln!(out, "/// <summary>")?;
        for line in comment.lines() {
            if line.is_empty() {
                if summary_is_open {
                    writeln!(out, "/// </summary>")?;
                    writeln!(out, "/// <remarks>")?;
                    summary_is_open = false;
                } else {
                    writeln!(out, "/// <br/>")?;
                }
            } else {
                writeln!(out, "/// {line}")?;
            }
        }

        if summary_is_open {
            writeln!(out, "/// </summary>")?;
        } else {
            writeln!(out, "/// </remarks>")?;
        }
    }
    Ok(())
}
