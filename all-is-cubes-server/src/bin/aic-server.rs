//! main() for a server that serves the All is Cubes client as well as being a game
//! server.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

use clap::builder::{PossibleValuesParser, TypedValueParser};

use all_is_cubes_server::{start_server, AicClientSource};

#[derive(Debug, clap::Parser)]
struct Args {
    #[arg(long, short = 'v')]
    verbose: bool,

    /// TCP port to bind.
    ///
    /// If not specified, an arbitrary port will be chosen.
    #[arg(long)]
    port: Option<u16>,

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
        port,
        verbose,
        client_source,
    } = <Args as clap::Parser>::parse();

    // Note: Something like this log configuration also appears in other binaries.
    // Unclear how to deduplicate since we don't want to have a library-level dep on
    // simplelog. For now, just remember to consider updating other instances.
    use simplelog::LevelFilter::{Debug, Off, Trace};
    simplelog::TermLogger::init(
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
        simplelog::TerminalMode::Stderr,
        simplelog::ColorChoice::Auto,
    )?;

    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], port.unwrap_or(0)));

    let (url, finished) = start_server(addr, &client_source).await?;
    println!("{url}"); // note: printed *to stdout* for the use of tests

    finished.await?;
    Ok(())
}
