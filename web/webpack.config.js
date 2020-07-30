var path = require("path");
var webpack = require("webpack");
var OfflinePlugin = require('offline-plugin');
const PnpWebpackPlugin = require(`pnp-webpack-plugin`);

var config = {
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
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    }
};

module.exports = (env, argv) => {
    if (argv.mode === 'production') {
        config.plugins = [
            new OfflinePlugin({
                autoUpdate: true,
                externals: ['/']
            })
        ]
    } else {
        config.devtool = 'source-map';
        config.plugins = [
            new webpack.HotModuleReplacementPlugin(),
        ]
    }
    config.resolve = { plugins: [PnpWebpackPlugin] }
    config.resolveLoader = { plugins: [PnpWebpackPlugin.moduleLoader(module)] }
    return config;
};
