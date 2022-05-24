
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

bart.overlap.getScales = function(data, config) {
    const { bodyWidth, bodyHeight, container } = config;
    const maxX = d3.max(data.observations, d => +d.caloriesConsumed);
    const minX = d3.min(data.observations, d => +d.caloriesConsumed);
    const maxY = d3.max(data.observations, d => +d.runningTime);
    const minY = d3.min(data.observations, d => +d.runningTime);
    const padding = (maxX - minX) * 0.06

    const xScale = d3.scaleLinear()
      .domain([-400, 1400])
      .range([0, bodyWidth])
    const yScale = d3.scaleLinear()
      .domain([minY - padding, maxY + padding]) 
      .range([bodyHeight, 0])
  
    const colorScale = d3.scaleOrdinal()
      .domain(["0", "1", 'true'])
      .range(["#21918c", "#9D8420", '#303030'])
  
   return { xScale, yScale, colorScale }
  }

bart.overlap.drawPlot = function(data, scales, config){
    let { container, selector } = config;
    const { xScale, yScale } = scales
  
    // draw base plot
    bart.drawPlot(data, scales, config);
  
    // remove fitted lines besides BART
    let selectors = ".bart-lines-bartFit0, .bart-lines-bartFit1"
    // container.selectAll(`.bart-lines-group > :not(${selectors})`).remove()
    container.selectAll(selectors).style('display', null)

    // show posterior lines
    container.selectAll('.bart-distribution-line')
        .style('display', null)
        .style('opacity', 0.6)

    // de-emphasize points
    container.selectAll('.bart-observations').transition().style('opacity', 0.2)

    // add title
    container.select('.bart-title').text('BART response surface')

    // add subtitle
    container.select('.bart-subtitle').text('Bayesian Additive Regression Trees (BART)')

}

// TODO need to calculate posterior for 10 states
// read in as json; overwrite data when slider changes
// update true response surface?
