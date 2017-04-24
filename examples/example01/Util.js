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
            var seconds = minutesRaw - minutes

            return degrees.toFixed(0) + "°" + minutes.toFixed(0) + "'" + seconds.toFixed(4) + '"' + indicator
        }
    }
}
