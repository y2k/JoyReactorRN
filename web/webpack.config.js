var path = require("path");
var webpack = require("webpack");
const PnpWebpackPlugin = require(`pnp-webpack-plugin`);
const TerserPlugin = require("terser-webpack-plugin");

var config = {
    mode: "development",
    entry: "./ui/Library.fs.js",
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
            target: 'http://127.0.0.1:8090'
        }, {
            path: '/form',
            target: 'http://127.0.0.1:8090'
        }],
    },
    module: {
    }
};

module.exports = (env, argv) => {
    if (argv.mode === 'production') {
        config.optimization = {
            minimize: true,
            minimizer: [new TerserPlugin()],
        }
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
