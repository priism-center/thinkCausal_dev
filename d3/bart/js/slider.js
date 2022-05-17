
bart.overlap.slider = $("#bart-slider-overlap").ionRangeSlider({
    type: "single",
    min: 1,
    max: 10,
    value: 5,
    step: 1,
    grid: false,

    onChange: function(data) {
        bart.overlap.updateSliderLabel()
    }
});

bart.overlap.updateSliderLabel = function(){
    $("#bart-slider-overlap-container .irs-min").text("Good overlap")
    $("#bart-slider-overlap-container .irs-single").html("&nbsp;")
    $("#bart-slider-overlap-container .irs-max").text("Poor overlap")
}
