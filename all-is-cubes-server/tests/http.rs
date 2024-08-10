//! Test starting and contacting the web server, externally.

use std::process::Stdio;

use reqwest::header::HeaderValue;
use reqwest::Url;
use tokio::io::AsyncBufReadExt;

async fn with_server<F: AsyncFnOnce(Url)>(client_source: &'static str, f: F) {
    let mut server = tokio::process::Command::new(env!("CARGO_BIN_EXE_aic-server"))
        .arg("--client-source")
        .arg(client_source)
        .kill_on_drop(true)
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    // Read listening address from the server
    let mut stdout = tokio::io::BufReader::new(server.stdout.take().unwrap());
    let mut server_url_line = String::new();
    // TODO: add a timeout on this read_line
    stdout.read_line(&mut server_url_line).await.unwrap();
    assert!(
        server_url_line.starts_with("http"),
        "Expected URL: {server_url_line:?}"
    );
    let url = Url::parse(server_url_line.trim()).expect("parsing server URL failed");

    f(url).await;

    server.kill().await.unwrap()
}

/// This test will fail if the static file serving is not working.
/// (Or if we change the URL layout.)
async fn client_smoke_test(root_url: Url) {
    let resp = reqwest::get(root_url.join("style.css").unwrap())
        .await
        .unwrap()
        .error_for_status()
        .unwrap();
    assert_eq!(
        resp.headers().get("Content-Type"),
        // TODO: this should be ;charset=utf-8 but that will require more hand-built server code
        Some(&HeaderValue::from_static("text/css"))
    );

    // TODO: also test that the wasm exists and is loadable (currently difficult because
    // the address is variable)
}
#[cfg(feature = "embed")]
#[tokio::test]
async fn client_smoke_test_embedded() {
    with_server("embedded", client_smoke_test).await
}
#[tokio::test]
async fn client_smoke_test_workspace() {
    with_server("workspace", client_smoke_test).await
}
