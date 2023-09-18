use askama::Template;

use crate::dotnet::config::BackendConfig;

use super::DotnetContext;

impl DotnetContext<'_> {
    pub(super) fn gen_runtime(&self) {
        #[derive(Template)]
        #[template(path = "dotnet/runtime.cs", escape = "none")]
        struct RuntimeTemplate<'a> {
            config: &'a BackendConfig,
        }

        self.files.add_file(
            "DiplomatRuntime.cs".to_owned(),
            RuntimeTemplate {
                config: &self.config,
            }
            .render()
            .unwrap(),
        );
    }

    // further methods can be found in ty.rs and formatter.rs
}
