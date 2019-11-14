exports.setGradientStrokeStyle = function(ctx) {
    return function(gradient) {
        return function() {
            ctx.strokeStyle = gradient;
        };
    };
};
