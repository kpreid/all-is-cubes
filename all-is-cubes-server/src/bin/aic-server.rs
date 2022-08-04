//! main() for a server that serves the All is Cubes client as well as being a game
//! server.

use all_is_cubes_server::webserver::server_main;

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

    server_main().await?;
    Ok(())
}
