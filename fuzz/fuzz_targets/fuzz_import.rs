#![no_main]

use std::sync::Arc;

use all_is_cubes::util::YieldProgress;
use all_is_cubes_port as port;

libfuzzer_sys::fuzz_target!(|input: &[u8]| test(input));

fn test(input: &[u8]) {
    let input = input.to_vec();
    let _result = pollster::block_on(port::load_universe_from_file(
        YieldProgress::noop(),
        Arc::new(port::file::NonDiskFile::from_name_and_data_source(
            String::new(),
            move || Ok(input.clone()),
        )),
    ));
}
