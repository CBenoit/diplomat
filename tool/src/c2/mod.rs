mod formatter;
mod header;
mod ty;

pub(crate) use self::formatter::CFormatter;
pub(crate) use self::formatter::CAPI_NAMESPACE;
pub(crate) use self::header::Header;
pub(crate) use self::ty::TyGenContext;

use crate::common::{ErrorStore, FileMap};
use askama::Template;
use diplomat_core::hir;

pub fn gen_runtime(is_for_cpp: bool) -> String {
    #[derive(Template)]
    #[template(path = "c2/runtime.h.jinja", escape = "none")]
    struct RuntimeTemplate {
        is_for_cpp: bool,
    }
    let mut runtime = String::new();
    RuntimeTemplate { is_for_cpp }
        .render_into(&mut runtime)
        .unwrap();
    runtime
}

pub fn run(tcx: &hir::TypeContext) -> (FileMap, ErrorStore<String>) {
    let files = FileMap::default();
    let formatter = CFormatter::new(tcx, false);
    let errors = ErrorStore::default();

    files.add_file("diplomat_runtime.h".into(), gen_runtime(false));

    for (id, ty) in tcx.all_types() {
        if ty.attrs().disable {
            // Skip type if disabled
            continue;
        }
        if let hir::TypeDef::Struct(s) = ty {
            if s.fields.is_empty() {
                // Skip ZST
                continue;
            }
        }
        if let hir::TypeDef::OutStruct(s) = ty {
            if s.fields.is_empty() {
                // Skip ZST
                continue;
            }
        }

        let decl_header_path = formatter.fmt_decl_header_path(id);
        let impl_header_path = formatter.fmt_impl_header_path(id);

        let _guard = errors.set_context_ty(ty.name().as_str().into());
        let context = TyGenContext {
            tcx,
            formatter: &formatter,
            errors: &errors,
            is_for_cpp: false,
            id,
            decl_header_path: &decl_header_path,
            impl_header_path: &impl_header_path,
        };

        let decl_header = match ty {
            hir::TypeDef::Enum(e) => context.gen_enum_def(e),
            hir::TypeDef::Opaque(o) => context.gen_opaque_def(o),
            hir::TypeDef::Struct(s) => context.gen_struct_def(s),
            hir::TypeDef::OutStruct(s) => context.gen_struct_def(s),
            _ => unreachable!("unknown AST/HIR variant"),
        };

        let impl_header = context.gen_impl(ty);

        files.add_file(decl_header_path, decl_header.to_string());
        files.add_file(impl_header_path, impl_header.to_string());
    }

    (files, errors)
}
