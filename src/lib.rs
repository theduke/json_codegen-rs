extern crate failure;
#[macro_use]
extern crate log;
extern crate ordermap;
#[macro_use]
extern crate quote;
extern crate regex;
extern crate reqwest;
extern crate rustfmt;
extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde_json;
extern crate syn;
extern crate toml;
extern crate url;
extern crate url_serde;

pub mod config;
pub mod types;
mod generator;

pub use generator::ApiGenerator;
