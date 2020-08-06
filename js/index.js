// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

import("../pkg/index.js").then(module => {
  // Global variable for debug use only
  console.log("Module: ", module);
  window.aic_mod = module;
}).catch(console.error);
