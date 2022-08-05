//! Test starting and contacting the web server, externally.

use std::process::Stdio;

use async_fn_traits::AsyncFnOnce1;
use reqwest::header::HeaderValue;
use tokio::io::AsyncBufReadExt;

async fn with_server<F: AsyncFnOnce1<String, Output = ()>>(f: F) {
    let mut server = tokio::process::Command::new(env!("CARGO_BIN_EXE_aic-server"))
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

    f(server_url_line.trim().to_owned()).await;

    server.kill().await.unwrap()
}

/// This test will fail if the static file serving is not working.
/// (Or if we change the URL layout.)
#[tokio::test]
async fn server_static_files_smoke_test() {
    with_server(|root_url| async move {
        let resp = reqwest::get(format!("{root_url}/style.css")) // TODO: use a url resolver library
            .await
            .unwrap()
            .error_for_status()
            .unwrap();
        assert_eq!(
            resp.headers().get("Content-Type"),
            // TODO: this should be ;charset=utf-8 but that will require more hand-built server code
            Some(&HeaderValue::from_static("text/css"))
        );
    })
    .await
}
