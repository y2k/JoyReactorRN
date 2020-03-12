var path = require("path");
var webpack = require("webpack");

module.exports = {
    mode: "development",
    entry: "./src/web.fsproj",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        contentBase: "./public",
        port: 8080,
        hot: true,
        inline: true
    },
    devtool: "source-map",
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    },
    plugins: [new webpack.HotModuleReplacementPlugin()]
}
