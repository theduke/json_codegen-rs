use std::path;
use std::collections::{BTreeMap, VecDeque};
use std::cell::RefCell;

use reqwest::Client;
use failure::Error;
use serde_json::Value;
use quote::Tokens;
use url::Url;
use regex::Regex;

use config::{Config, EndpointConfig};
use types::{analyse_value, JsonType, ObjectType, TypeRegistry};

/// Convert the first char of the string to upper case.
fn upper_case_first(value: &str) -> String {
    let first = value.chars().next().unwrap().to_uppercase();
    format!("{}{}", first, &value[1..])
}

/// A single API endpoint.
pub struct Endpoint {
    name: String,
    type_name: String,
    regex: Regex,
    request_counter: u64,
}

/// ApiGenerator loads API endpoints and anlyses them according to the config.
/// It then generates type declarations and an API client as Rust code.
pub struct ApiGenerator {
    config: Config,

    client: Client,
    endpoints: BTreeMap<String, Endpoint>,
    types: RefCell<TypeRegistry>,

    url_queue: VecDeque<Url>,
}

impl ApiGenerator {
    /// Build a ApiGenerator from a config file path.
    pub fn from_config_file<P: AsRef<path::Path>>(path: P) -> Result<Self, Error> {
        let config = Config::from_file(path)?;
        Ok(Self::new(config))
    }

    /// Create a new ApiGenerator based on a config.
    pub fn new(config: Config) -> Self {
        let endpoints = config
            .endpoints
            .iter()
            .map(|(name, conf)| {
                let type_name = conf.type_name
                    .clone()
                    .unwrap_or(format!("{}Response", upper_case_first(&name)));
                let regex = Regex::new(&conf.pattern).unwrap();

                (
                    name.clone(),
                    Endpoint {
                        name: name.clone(),
                        type_name,
                        regex,
                        request_counter: 0,
                    },
                )
            })
            .collect();

        let url_queue = config.urls.iter().map(|x| x.0.clone()).collect();

        // Build the client.

        let mut headers = ::reqwest::header::Headers::new();
        // Add default headers, if specified in config.
        config
            .client
            .as_ref()
            .and_then(|c| c.headers.as_ref())
            .and_then(|h| {
                for (key, val) in h {
                    headers.set_raw(key.to_string(), val.to_string());
                }
                Some(true)
            });

        let client = ::reqwest::ClientBuilder::new()
            .default_headers(headers)
            .build()
            .unwrap();

        ApiGenerator {
            config,
            client,
            endpoints,
            types: RefCell::new(TypeRegistry::new()),
            url_queue,
        }
    }

    /// Analyze the json structure of the data returned by a URL.
    fn analyze_url(&self, url: &Url) -> Result<JsonType, Error> {
        debug!("Fetching url: {}", url);
        let value: Value = self.client
            .get(&url.to_string())
            .send()?
            .error_for_status()?
            .json()?;
        Ok(analyse_value(&value))
    }

    /// Normalize a JsonType be replacing anonymous objects with type references
    /// and add the types to the registry.
    fn add_normalize_type(&self, typ: JsonType, path: Vec<String>) -> JsonType {
        use std::iter::FromIterator;
        use self::JsonType as T;
        debug_assert!(path.len() > 0);

        let typ = match typ {
            T::Array(t) => T::array(self.add_normalize_type(*t, path)),
            T::Optional(t) => T::array(self.add_normalize_type(*t, path)),
            T::Object(obj) => {
                let obj_type = T::object(ObjectType::from_iter(obj.fields.into_iter().map(
                    |(name, field_type)| {
                        let mut path = path.clone();
                        path.push(name.clone());
                        let field_type = self.add_normalize_type(field_type, path);
                        (name, field_type)
                    },
                )));

                let name = path.iter().last().unwrap().to_string();
                let name = upper_case_first(&name);

                let mut borrow = self.types.borrow_mut();
                borrow.add(name.clone(), obj_type, true);

                T::ObjectReference(name)
            }
            x => x,
        };
        typ
    }

    /// Load a URL and refine the type definitions based on the result.
    fn analyse_url(&self, endpoint: &Endpoint, url: Url) -> Result<JsonType, Error> {
        info!("Analyzing endpoint {} url: {}", endpoint.name, url);

        let mut typ = self.analyze_url(&url)?;
        let typ = self.add_normalize_type(typ, vec![endpoint.type_name.clone()]);

        Ok(typ)
    }

    /// Build Rust code that represents a JSON type.
    /// TODO: move to types.rs.
    fn render_type(typ: &JsonType) -> ::quote::Tokens {
        use self::JsonType as T;
        use syn::Ident;

        match typ {
            &T::Null => quote!(Option<::serde_json::Value>),
            &T::Bool => quote!(bool),
            &T::Number => quote!(f32),
            &T::String | &T::Url => quote!(String),
            &T::Array(ref t) => {
                let item_type = Self::render_type(&*t);
                quote!(Vec<#item_type>)
            }
            &T::Optional(ref t) => {
                let item_type = Self::render_type(&*t);
                quote!(Option<#item_type>)
            }
            &T::ObjectReference(ref name) => {
                let ident = Ident::from(name.as_str());
                quote!(#ident)
            }
            &T::Object(_) => {
                panic!("Internal error: can't render object type, types must be normalized first");
            }
            &T::Multi(_) => {
                unimplemented!();
            }
        }
    }

    /// Generate Rust code for all types in the registry.
    /// TODO: move to type registry in types.rs
    fn render_types(&self) -> Tokens {
        use syn::Ident;

        let borrow = self.types.borrow();

        let types = borrow.types.iter().filter_map(|(name, typ)| {
            typ.as_object().map(|obj| {
                let type_name = Ident::from(name.as_ref());

                let fields = obj.fields.iter().map(|&(ref name, ref typ)| {
                    let field_ident = Ident::from(name.as_str());
                    let type_tok = Self::render_type(&typ);
                    quote!(pub #field_ident: #type_tok,)
                });

                quote!(
                        #[derive(Serialize, Deserialize, Clone, Debug)]
                        pub struct #type_name {
                            #(#fields)*
                        }

                    )
            })
        });

        let output = quote!(#(#types)*);
        output
    }

    /// Format Rust tokens using rustfmt.
    fn format_code(code: ::quote::Tokens) -> String {
        use rustfmt::{self, format_input, Input};
        let input = Input::Text(code.to_string());
        let mut output = Vec::new();
        let mut config = rustfmt::config::Config::default();
        config.set().write_mode(rustfmt::config::WriteMode::Plain);
        let (summary, map, report) = format_input(input, &config, Some(&mut output)).unwrap();
        ::std::str::from_utf8(&output).unwrap().to_string()
    }

    /// Execute the generator.
    pub fn run(&mut self) -> Result<(), Error> {
        loop {
            if let Some(url) = self.url_queue.pop_front() {
                let endpoint_match = self.endpoints
                    .iter()
                    .find(|&(_, ref endpoint)| endpoint.regex.is_match(url.as_str()))
                    .map(|(_, e)| e);

                if let Some(endpoint) = endpoint_match {
                    self.analyse_url(endpoint, url)?;
                } else {
                    warn!("URL does not match any endpoint pattern: {}", url);
                }
            } else {
                break;
            }
        }

        let type_tokens = self.render_types();
        let type_code = Self::format_code(type_tokens);
        println!("\ntype_code: \n{}", type_code);

        Ok(())
    }
}
