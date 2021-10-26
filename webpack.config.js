const path = require('path');

module.exports = {
  entry: './typescript/main.ts',
  devtool: false,
  output: {
    path: path.resolve(__dirname, '.'),
    libraryTarget: 'commonjs',
    filename: 'main.js'
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        use: 'ts-loader',
      },
      {
        test: [/\.elm$/],
        exclude: [/elm-stuff/, /node_modules/],
          use: [
            { loader: "elm-hot-webpack-loader" },
            {
              loader: "elm-webpack-loader",
              options: {
                optimize: false,
                files: [
                  path.resolve(__dirname, "src/Main.elm"),
                  path.resolve(__dirname, "src/Worker.elm")
                ]
              }
            }
          ]
      },
    ]
  },
  externals: {
    obsidian: 'obsidian'
  },
  resolve: {
    extensions: ['.js', '.ts', '.elm']
  }
};
