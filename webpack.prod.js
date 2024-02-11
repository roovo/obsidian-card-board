const path = require("path");

const { merge }     = require("webpack-merge");
const common        = require("./webpack.common.js");
const TerserPlugin  = require("terser-webpack-plugin");

const terserOptions = {
    mangle: false,
    compress: {
        pure_funcs: [
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
        ],
        pure_getters: true,
        keep_fargs: false,
        unsafe_comps: true,
        unsafe: true
    }
};
module.exports = merge(common, {
  mode: "production",
  optimization: {
    minimize: true,
    minimizer: [
      new TerserPlugin({
        minify: TerserPlugin.esbuildMinify
        // terserOptions: terserOptions
      }),
    ],
  },
  module: {
    rules: [
      {
        test: [/\.elm$/],
        exclude: [/elm-stuff/, /node_modules/],
          use: [
            {
              loader: "elm-webpack-loader",
              options: {
                optimize: true
              }
            }
          ]
      },
    ]
  },
});

