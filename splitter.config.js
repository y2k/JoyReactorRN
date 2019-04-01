var path = require('path')

function resolve(filePath) {
    return path.resolve(__dirname, filePath)
}

var define = [];
var idx = process.argv.indexOf("--define");

if (idx > -1) {
    define = [process.argv[idx + 1]];
}

console.log("Bundling for " + define + "...");

module.exports = {
    entry: resolve('src/joyreact.fsproj'),
    outDir: resolve("out"),
    babel: {
        presets: ["module:metro-react-native-babel-preset"]
    },
    fable: {
        define: define
    }
};