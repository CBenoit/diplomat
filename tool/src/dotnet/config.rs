use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct BackendConfig {
    pub namespace: String,
    pub native_lib: String,
    #[serde(default)]
    pub usings: Vec<String>,
    #[serde(default)]
    pub exceptions: ExceptionsConfig,
    #[serde(default)]
    pub properties: PropertiesConfig,
}

impl BackendConfig {
    pub fn default() -> BackendConfig {
        BackendConfig {
            namespace: "Interop".to_owned(),
            native_lib: "rust".to_owned(),
            usings: Vec::new(),
            exceptions: ExceptionsConfig::default(),
            properties: PropertiesConfig::default(),
        }
    }
}

#[derive(Debug, Deserialize, Default)]
pub struct PropertiesConfig {
    #[serde(default)]
    pub setters_prefix: Option<String>,
    #[serde(default)]
    pub getters_prefix: Option<String>,
}

#[derive(Debug, Deserialize, Default)]
pub struct ExceptionsConfig {
    #[serde(default)]
    pub trim_suffix: String,
    #[serde(default)]
    pub error_message_method: Option<String>,
}
