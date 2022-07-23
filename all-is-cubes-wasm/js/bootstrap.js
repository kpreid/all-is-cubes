// This is the "root" _module_ which is loaded by js/index.js and imports all others.

import { start_game } from '../pkg/index.js';  // Rust WASM module is named this for some reason
import { makeAllStaticGuiHelpers } from './gui';

export function bootstrap() {
  start_game(makeAllStaticGuiHelpers(window, document));
}