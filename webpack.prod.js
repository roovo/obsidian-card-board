const path          = require("path");
const { merge }     = require("webpack-merge");
const common        = require("./webpack.common.js");
const TerserPlugin  = require("terser-webpack-plugin");

module.exports = merge(common, {
  mode: "production",
  optimization: {
    'minimize': true,
    minimizer: [new TerserPlugin({
      terserOptions: {
        compress: {
          pure_funcs: [
            'console.info',
            'console.debug',
            'console.warn',
            "F2",
            "F3",
            "F4",
            "F5",
            "F6",
            "F7",
            "F8",
            "F9",
            "A2",
            "A3",
            "A4",
            "A5",
            "A6",
            "A7",
            "A8",
            "A9"

          ]
        }
      }
    })],
  },
  module: {
    rules: [
      {
        test: [/\.elm$/],
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: "elm-webpack-loader",
            options: {}
          }
        ]
      },
    ]
  },
});
