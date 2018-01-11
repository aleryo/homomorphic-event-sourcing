const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const localPort = "3000";
const proxiedServer = "http://localhost:9090";

module.exports = {
    entry: [
        "./src/index.tsx",
        //"./img/"
        //"./css/acquire.css",
        //"./css/font-awesome.css"
    ],
    devServer: {
        contentBase: "./build",
        lazy: false,     // always compile immediately to save time
        compress: false, // do not spend time on this
        host: "0.0.0.0", // server is also available externally
        overlay: {       // overlay for compiler issues
          warnings: true,
          errors: true
        },
        port: localPort,
        hot: true,       // hot module replacement
        historyApiFallback: true,
        proxy: { "/": proxiedServer }
    },
    devtool: "inline-source-map",
    output: {
        path: path.join(__dirname, "dist"),
        filename: "[hash].bundle.js"
    },
    plugins: [
        new webpack.HotModuleReplacementPlugin(),
        new HtmlWebpackPlugin({
            template: "./index.html"
        }),
        new webpack.DefinePlugin({
            "process.env": {
                IS_IN_WEBPACK: true,
                NODE_ENV: '"development"'
            }
        })
    ],
    module: {
        rules: [
            { test: /\.tsx?$/, exclude: /node_modules/, use: [{loader: "ts-loader"}]},
            { test: /\.css$/, use: [ { loader: "style-loader" }, { loader: "css-loader" } ] },
            { test: /\.(svg|woff2?|eot|ttf)$/, use: [ { loader: "url-loader" } ] },
            { test: /\.(jpg|png|svg)$/, use: [ {loader: "file-loader", options: {}} ] }
        ]
    },
    resolve: {
        extensions: [".tsx", ".ts", ".js"]
    }
};
