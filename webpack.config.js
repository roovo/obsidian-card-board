const path = require('path');

module.exports = {
  entry: './typescript/main.ts',
  performance: {
    hints: false
  },
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
            { loader: "elm-reloader" },
            {
              loader: "elm-webpack-loader",
              options: {}
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
