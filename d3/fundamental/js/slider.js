fundamental.trueMeanSlider = $("#fundamental-slider-distribution-mean").ionRangeSlider({
    type: "single",
    min: -50,
    max: 50,
    step: 1,
    grid: true,

    onFinish: function(data) {
        fundamental.updatePlot(data.input.val());
    }
});
