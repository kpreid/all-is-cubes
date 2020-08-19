// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

// HTML GUI code, including management of the WebGL canvas outside of
// actually drawing.
//
// This is in JS because writing DOM-based GUI code from Rust raises all sorts
// lifetime and reference management issues.
// The Rust bindings for this file may be found in src/js_bindings.js.

// Construct all GUI helper objects that refer to elements defined in the HTML
// rather than created dynamically.
export function makeAllStaticGuiHelpers(window, document) {
  return {
    canvasHelper: new CanvasHelper(document.getElementById('view-canvas')),
  };
}

// Manages a <canvas> element; particularly, resizing it to have 1:1 pixel resolution.
export class CanvasHelper {
  // TODO: unit test this class

  constructor(canvas) {
    if (!canvas) throw new Error('canvas missing');

    this.canvas = canvas;

    const canvasComputedStyle = window.getComputedStyle(canvas, null);
    let layoutPixelWidth;
    let layoutPixelHeight;

    const updateViewport = () => {
      // allow canvas to flex-shrink, then get actual size
      canvas.width = 1;
      canvas.height = 1;
      layoutPixelWidth = parseInt(canvasComputedStyle.width, 10);
      layoutPixelHeight = parseInt(canvasComputedStyle.height, 10);

      canvas.width = layoutPixelWidth * window.devicePixelRatio;
      canvas.height = layoutPixelHeight * window.devicePixelRatio;

      this.viewportPx = Object.freeze([layoutPixelWidth, layoutPixelHeight]);
      this.viewportDev = Object.freeze([canvas.width, canvas.height]);
    };
    updateViewport();

    const resizeObserver = new ResizeObserver(_entries => {
      updateViewport();
      // TODO once we have the _option_ of not rendering if nothing changed,
      // we'll need to tell the renderer to render because the aspect ratio changed.
    });
    resizeObserver.observe(canvas);
  }

  // property .canvas

  // property .viewportPx

  // Get the ID of the canvas element, assigning one if needed.
  id() {
    let id = this.canvas.id;
    if (typeof id !== 'string') {
      console.warn('canvas has no id; assigning a random one');
      this.canvas.id = id = 'canvas' + Math.random();
    }
    return id;
  }
}