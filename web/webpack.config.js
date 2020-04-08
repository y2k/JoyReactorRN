var path = require("path");
var webpack = require("webpack");
var OfflinePlugin = require('offline-plugin');

module.exports = {
    mode: "development",
    entry: "./ui/ui.fsproj",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        contentBase: "./public",
        port: 8080,
        hot: true,
        inline: true,
        proxy: [{
            path: '/parse/',
            target: 'http://localhost:8090'
        }, {
            path: '/form',
            target: 'http://localhost:8090'
        }],
    },
    devtool: "source-map",
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    },
    plugins: [
        new webpack.HotModuleReplacementPlugin(),
        new OfflinePlugin({
            autoUpdate: true,
            externals: [
                '/'
            ]
        })]
}
