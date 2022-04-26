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

fundamental.sliderEfficiency = $("#fundamental-slider-efficiency").ionRangeSlider({
    type: "single",
    from: -1,
    min: -1,
    max: 1,
    step: 0.00001,
    grid: false,
    // hide_min_max: true,
    hide_from_to: true,

    onFinish: function(data) {
        // fundamental.updatePlot(data.input.val());
        fundamental.updateSliderLabel();
        fundamental.killSliderAnimations();
    }
});

fundamental.sliderBias = $("#fundamental-slider-bias").ionRangeSlider({
    type: "single",
    from: -1,
    min: -1,
    max: 1,
    step: 0.00001,
    grid: false,
    // hide_min_max: true,
    hide_from_to: true,

    onUpdate: function(data){
        // console.log(data)
        $("#temp").text(data.from)
    },
    onFinish: function(data) {
        // fundamental.updatePlot(data.input.val());
        fundamental.updateSliderLabel()
        $("#temp").text(data.from)
        fundamental.killSliderAnimations();
    }
});

fundamental.updateSliderLabel = function(){
    $("#fundamental-slider-efficiency-container .irs-min").text("Low efficiency")
    $("#fundamental-slider-efficiency-container .irs-max").text("High efficiency")
    $("#fundamental-slider-bias-container .irs-min").text("Low bias")
    $("#fundamental-slider-bias-container .irs-max").text("High bias")
}

fundamental.animateSlider = function(){
    fundamental.sliderDuration = 2000
    for (let i = 0; i < fundamental.sliderDuration; i++){
        let newVal = (i - 1000) / 1000 
        fundamental.sliderTimeouts.push(
            setTimeout(() => {
                fundamental.sliderBias.data("ionRangeSlider").update({from: newVal});
                fundamental.sliderEfficiency.data("ionRangeSlider").update({from: newVal});
                fundamental.updateSliderLabel();
            }, i)
        )
    }
}


fundamental.isInViewport = function(element) {
    const rect = element.getBoundingClientRect();
    return (
        rect.top >= 0 &&
        rect.left >= 0 &&
        rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
        rect.right <= (window.innerWidth || document.documentElement.clientWidth)
    );
}


// trigger slider animation when slider is in view
fundamental.sliderTriggered = false
document.addEventListener('scroll', function(){
    let isInView = fundamental.isInViewport($('#fundamental-slider-efficiency-container')[0])
    if (!fundamental.sliderTriggered && isInView){
        fundamental.buildEfficiencyPlot();
        fundamental.buildBiasPlot();
        fundamental.animateSlider();
        fundamental.sliderTriggered = true
    }
});