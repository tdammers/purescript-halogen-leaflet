var padTo = function (str, l, s) {
    var n = l - str.length
    for (var i = 0; i < n; ++i) {
        str = s + str
    }
    return str
}

exports.formatGeo = function (posIndicator) {
    return function (negIndicator) {
        return function(gc) {
            var indicator = posIndicator
            if (gc < 0) {
                gc = -gc
                indicator = negIndicator
            }
            var degrees = Math.floor(gc)
            var minutesRaw = (gc - degrees) * 60
            var minutes = Math.floor(minutesRaw)
            var seconds = (minutesRaw - minutes) * 60

            return padTo(degrees.toFixed(0), 3, ' ') + "°" +
                padTo(minutes.toFixed(0), 2, '0') + "'" +
                padTo(seconds.toFixed(4), 7, '0') + '"' + indicator
        }
    }
}
