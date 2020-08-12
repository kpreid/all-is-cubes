// Derived from the template
// https://github.com/rustwasm/rust-webpack-template/blob/24f3af83206b52e0241d95ee10cebf930ec8bf08/template/webpack.config.js

const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const dist = path.resolve(__dirname, "dist");

module.exports = {
  mode: "production",
  entry: {
    index: "./js/index.js"
  },
  output: {
    path: dist,
    filename: "[name].js"
  },
  devServer: {
    contentBase: dist,
  },
  plugins: [
    new CopyPlugin({
      patterns: [
        path.resolve(__dirname, "static")
      ]
    }),

    new WasmPackPlugin({
      crateDirectory: __dirname,
    }),
  ]
};
