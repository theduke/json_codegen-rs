use std::collections::HashMap;
use std::path::Path;
use std::fs;
use std::io::Read;

use failure::Error;
use regex::Regex;
use url_serde::SerdeUrl;

/// Configuration regarding the http client.
/// Only relevant during the fetching phase.
///
/// Mainly useful for adding a Authorization header.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ClientConfig {
    pub headers: Option<HashMap<String, String>>,
}

/// Configuration for a single ednpoint that should be analysed.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct EndpointConfig {
    pub type_name: Option<String>,
    pub pattern: String,
}

/// Configuration for the API generator.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Config {
    /// Target directory where client code should be generated.
    pub target_dir: Option<String>,
    /// If false or null, the user will be prompted for additional information/
    /// settings when necessary.
    pub non_interactive: Option<bool>,

    /// HTTP client configuration.
    pub client: Option<ClientConfig>,
    /// Endpoints to be analysed.
    pub endpoints: HashMap<String, EndpointConfig>,
    /// The starting URLs that should be checked.
    /// They will be matched against the patterns in EndpointConfig items.
    pub urls: Vec<SerdeUrl>,
}

impl Config {
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Config, Error> {
        let mut file = fs::File::open(path)?;
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;

        let config = ::toml::from_slice(&data)?;
        Ok(config)
    }
}
