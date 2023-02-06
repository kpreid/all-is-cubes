// HTML GUI code, including management of the WebGL canvas outside of
// actually drawing.
//
// This is in JS because writing DOM-based GUI code from Rust raises all sorts
// lifetime and reference management issues.
// The Rust bindings for this file may be found in src/js_bindings.js.

// Construct all GUI helper objects that refer to elements defined in the HTML
// rather than created dynamically.
export function makeAllStaticGuiHelpers(window, document) {
  const result = {
    canvasHelper: new CanvasHelper(document.getElementById('view-canvas')),
  };

  // Set up initial focus
  result.canvasHelper.canvas.focus();

  return result;
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
    this.updateViewport = updateViewport; // allow explicit calls

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

  // Given pixel X and Y as used in mouse events, return GL-style normalized
  // device coordinates.
  normalizePosition(x, y) {
    return [
      x / this.viewportPx[0] * 2 - 1,
      y / this.viewportPx[1] * -2 + 1,
    ];
  }

  // Attempt to enter or leave fullscreen mode.
  // 
  // TODO: This and its companion isFullscreen() make document.body fullscreen, but for
  // more flexibility we should have an explicit canvas-and-everything container element.
  setFullscreen(shouldBeFullscreen) {
    const document = this.canvas.ownerDocument;

    if (shouldBeFullscreen) {
      if (document.body.requestFullscreen) {
        document.body.requestFullscreen();
      } else if (document.body.webkitRequestFullscreen) {
        // Safari still does not have unprefixed fullscreen API as of version 16.1
        document.body.webkitRequestFullscreen();
      } else {
        // TODO: instead we should have noticed this from the start and not enabled
        // fullscreen control
        alert("Full screen not supported, apparently.");
      }
    } else {
      if (document.exitFullscreen) {
        document.exitFullscreen();
      } else if (document.webkitExitFullscreen) {
        document.webkitExitFullscreen();
      }
    }
  }

  isFullscreen() {
    const document = this.canvas.ownerDocument;

    // Note: this test must be consistent with what we _make_ fullscreen.
    console.log("fse", document.webkitFullscreenElement);
    return document.fullscreenElement === document.body
      || document.webkitFullscreenElement === document.body;
  }
}