// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

document.getElementById('scene-info-text').innerText = 'Loading code...';
import("../pkg/index.js").then(module => {
  // Global variable for debug use only
  window.aic_mod = module;
  console.log("Module: ", module);
  
  module.start_game();
}).catch(error => {
  document.getElementById('scene-info-text').innerText +=
    '\nError during initial loading! Check console for details.';
  if (String(error) !== 'RuntimeError: unreachable') {
    // Only log errors that aren't Rust's panics because those are logged separately.
    console.error(error);
  }
});
