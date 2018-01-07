const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
    entry: [
      "./src/index.tsx",
      "./css/acquire.css",
      "./css/font-awesome.css"
    ],
    output: {
        path: path.join(__dirname, "build"),
        filename: "[hash].bundle.js"
    },
    plugins: [
      new webpack.optimize.UglifyJsPlugin(),
        new HtmlWebpackPlugin({
            template: "./index.html"
        }),
        new webpack.DefinePlugin({
            "process.env": {
                IS_IN_WEBPACK: true,
                NODE_ENV: '"production"'
            }
        })
    ],
  module: {
    rules: [
      { test: /\.tsx?$/, exclude: /node_modules/, use: [{loader: "ts-loader"}] },
      { test: /\.css$/, use: [ { loader: "style-loader" }, { loader: "css-loader" } ] },
      { test: /\.(svg|woff2?|eot|ttf)$/, use: [ { loader: "url-loader" } ] }
    ]
  },
  resolve: {
    extensions: [".tsx", ".ts", ".js"]
  }
};
