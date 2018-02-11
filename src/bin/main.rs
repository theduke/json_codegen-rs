extern crate json_codegen;
extern crate pretty_env_logger;

use json_codegen::ApiGenerator;

fn main() {
    pretty_env_logger::init();
    let mut gen = ApiGenerator::from_config_file("./json_api_gen.toml").unwrap();
    gen.run().unwrap();
}
