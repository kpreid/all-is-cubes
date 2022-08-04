//! main() for a server that serves the All is Cubes client as well as being a game
//! server.

use all_is_cubes_server::webserver::start_server;

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
    // Note: Something like this log configuration also appears in other binaries.
    // Unclear how to deduplicate since we don't want to have a library-level dep on
    // simplelog. For now, just remember to consider updating other instances.
    use simplelog::LevelFilter::{Info, Off};
    simplelog::TermLogger::init(
        Info,
        simplelog::ConfigBuilder::new()
            .set_target_level(Off)
            .set_location_level(Off)
            .build(),
        simplelog::TerminalMode::Stderr,
        simplelog::ColorChoice::Auto,
    )?;

    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], 0));

    let (url, finished) = start_server(addr)?;
    println!("{url}"); // note: printed *to stdout* for the use of tests

    finished.await?;
    Ok(())
}
