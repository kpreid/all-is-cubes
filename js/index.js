// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

document.getElementById('scene-info-text').innerText = 'Loading code...';
import("../pkg/index.js").then(module => {
  // Global variable for debug use only
  console.log("Module: ", module);
  window.aic_mod = module;
}).catch(error => {
  document.getElementById('scene-info-text').innerText +=
    'Error during initial loading! Check console for details.';
  console.error(error);
});
