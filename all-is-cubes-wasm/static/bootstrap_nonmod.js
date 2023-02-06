// This file is the non-module entry point which will be loaded from HTML.

document.getElementById('loading-log').innerText = 'Loading code...';
import("./bootstrap.js").then(module => {
  module.bootstrap();
}).catch(error => {
  document.getElementById('loading-log').innerText +=
    '\nError during initial loading! Check console for details.';
  if (String(error) !== 'RuntimeError: unreachable') {
    // Only log errors that aren't Rust's panics because those are logged separately.
    console.error(error);
  }
});
