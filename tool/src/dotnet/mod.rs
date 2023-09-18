use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

use colored::*;
use diplomat_core::hir;

use self::config::BackendConfig;
use self::formatter::DotnetFormatter;
use crate::common::FileMap;

mod config;
// mod conversions;
mod formatter;
// mod idiomatic;
mod raw;
mod runtime;

pub struct TypeStore<T> {
    keys: BTreeSet<String>,
    values: Vec<T>,
}

impl<T> Default for TypeStore<T> {
    fn default() -> Self {
        Self {
            keys: BTreeSet::default(),
            values: Vec::default(),
        }
    }
}

impl<T> TypeStore<T> {
    pub fn insert(&mut self, name: String, value: T) {
        if self.keys.insert(name) {
            self.values.push(value);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &T)> {
        self.keys.iter().map(|s| s.as_str()).zip(self.values.iter())
    }
}

#[derive(Clone)]
pub struct ResultType<'tcx> {
    pub ok: Option<&'tcx hir::SuccessType>,
    pub err: Option<&'tcx hir::OutType>,
}

pub struct DotnetContext<'tcx> {
    pub files: FileMap,
    pub backend_errors: crate::common::ErrorStore<'tcx, String>,

    tcx: &'tcx hir::TypeContext,
    fmt: DotnetFormatter<'tcx>,
    config: BackendConfig,
    result_store: TypeStore<ResultType<'tcx>>,
    error_ty_store: TypeStore<&'tcx hir::OutType>,
}

impl<'tcx> DotnetContext<'tcx> {
    pub fn new(
        tcx: &'tcx hir::TypeContext,
        docs_url_gen: &'tcx diplomat_core::ast::DocsUrlGenerator,
        files: FileMap,
        library_config_path: Option<&Path>,
    ) -> Self {
        let config = if let Some(path) = library_config_path {
            // Should be fine, we've already verified the path
            let contents = fs::read_to_string(path).unwrap();

            match toml::from_str(&contents) {
                Ok(config) => config,
                Err(err) => {
                    eprintln!(
                        "{} Unable to parse library configuration file: {path:?}\n{err}",
                        "Error:".red().bold(),
                    );
                    std::process::exit(1);
                }
            }
        } else {
            BackendConfig::default()
        };

        Self {
            files,
            backend_errors: crate::common::ErrorStore::default(),

            tcx,
            fmt: DotnetFormatter::new(tcx, docs_url_gen),
            config,
            result_store: TypeStore::default(),
            error_ty_store: TypeStore::default(),
        }
    }

    pub fn run(&mut self) {
        self.gen_runtime();

        // Raw API generation pass

        for (id, ty) in self.tcx.all_types() {
            self.raw_gen_ty(id, ty);
        }

        for (name, result) in self.result_store.iter() {
            self.raw_gen_result(name, result);

            // for (in_path, typ) in results {
            //     let result_name = types::gen_type_name_to_string(typ, &in_path, env)?;
            //     let mut out_buf = String::new();
            //     let mut out =
            //         CodeWriter::new(&mut out_buf, INDENTATION, SCOPE_OPENING, SCOPE_CLOSING);
            //     raw::gen_result(typ, &in_path, env, &mut out)?;
            //     outs.insert(format!("Raw{result_name}.cs"), out_buf)
            //         .and_then::<String, _>(|_| panic!("file created twice: Raw{}.cs", result_name));
            // }
        }

        // Idiomatic API generation pass

        for (id, ty) in self.tcx.all_types() {
            // TODO: self.idiomatic_gen_ty(id, ty);

            // let mut out_buf = String::new();
            // let mut out = CodeWriter::new(&mut out_buf, INDENTATION, SCOPE_OPENING, SCOPE_CLOSING);
            // idiomatic::gen(typ, in_path, env, &library_config, docs_url_gen, &mut out)?;
            // outs.insert(format!("{}.cs", typ.name()), out_buf)
            //     .and_then::<String, _>(|_| panic!("file created twice: {}.cs", typ.name()));
        }

        for (name, error_ty) in self.error_ty_store.iter() {
            // TODO: self.idiomatic_gen_exception(name, error_ty);

            // let idiomatic::ExceptionCtx { name, .. } =
            //     idiomatic::error_type_to_exception_name(env, &library_config, typ, &in_path)?;
            // let mut out_buf = String::new();
            // let mut out = CodeWriter::new(&mut out_buf, INDENTATION, SCOPE_OPENING, SCOPE_CLOSING);
            // idiomatic::gen_exception(env, &library_config, typ, &in_path, &mut out)?;
            // outs.insert(format!("{name}.cs"), out_buf)
            //     .and_then::<String, _>(|_| panic!("file created twice: {}.cs", name));
        }
    }
}
