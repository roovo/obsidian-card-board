const path = require('path');

module.exports = {
  entry: './typescript/main.ts',
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
      }
    ]
  },
  externals: {
    obsidian: 'obsidian'
  },
  resolve: {
    extensions: ['.js', '.ts']
  }
};
