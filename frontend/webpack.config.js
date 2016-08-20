module.exports = {
    entry: './entry.js',
    output: {
        path: __dirname + '/built',
        filename: "bundle.js"
    },
    module: {
        loaders: [{
            test: /\.js$/,
            exclude: /(node_modules|bower_components)/,
            loader: 'babel',
            query: {
              presets: ['es2015']
            }
        }, {
          test: /\.json$/,
          loader: 'json'
        }]
    }
};
