const path          = require('path');

const { merge } = require("webpack-merge");
const common    = require("./webpack.common.js");

module.exports = merge(common, {
  mode: "development",
  module: {
    rules: [
      {
        test: [/\.elm$/],
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          { loader: "elm-reloader" },
          {
            loader: "elm-webpack-loader",
            options: {}
          }
        ]
      },
    ]
  },
});
