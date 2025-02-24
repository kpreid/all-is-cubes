//! `main()` for a server that serves the All is Cubes client as well as being a game
//! server.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

use std::net::SocketAddr;

use clap::builder::{PossibleValuesParser, TypedValueParser};

use all_is_cubes_server::{AicClientSource, start_server};

#[derive(Debug, clap::Parser)]
struct Args {
    #[arg(long, short = 'v')]
    verbose: bool,

    /// TCP hostand port to bind.
    ///
    /// If unspecified, will use `127.0.0.1:0`
    /// If port is 0, an arbitrary free port will be chosen.
    #[arg(long, default_value_t = std::net::SocketAddr::from(([127, 0, 0, 1], 0)))]
    addr: SocketAddr,

    #[arg(
        long,
        value_parser = PossibleValuesParser::new(
            [
                #[cfg(feature = "embed")]
                "embedded",
                "workspace"
            ],
        ).map(|string| match string.as_str() {
            #[cfg(feature = "embed")]
            "embedded" => AicClientSource::Embedded,
            "workspace" => AicClientSource::Workspace,
            _ => unreachable!(),
        })
    )]
    client_source: AicClientSource,
}

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
    let Args {
        addr,
        verbose,
        client_source,
    } = <Args as clap::Parser>::parse();

    // Note: Something like this log configuration also appears in other binaries.
    // Unclear how to deduplicate since we don't want to have a library-level dep on
    // simplelog. For now, just remember to consider updating other instances.
    use simplelog::LevelFilter::{Debug, Off, Trace};
    simplelog::WriteLogger::init(
        match verbose {
            false => Debug,
            true => Trace,
        },
        simplelog::ConfigBuilder::new()
            .set_target_level(Trace)
            .set_location_level(Off)
            .add_filter_ignore_str("tracing") // noisy
            .add_filter_ignore_str("hyper") // noisy
            .add_filter_ignore_str("mio") // uninteresting
            .build(),
        std::io::stderr(),
    )?;

    let (url, finished) = start_server(addr, &client_source).await?;
    println!("{url}"); // note: printed *to stdout* for the use of tests

    finished.await?;
    Ok(())
}
