
// controls the delay on the rugplot animation
bart.delayFn = function(index){ return ((((((index+1)**0.001)-1) * 5000000)) / 1.3) + 100 } // accelerating curve

// store timeouts for later clearing
// bart.timeouts = []
// bart.killkdeAnimations = function(){
//     bart.timeouts.map(clearTimeout)
//     bart.timeouts = []
// }

bart.roundNumber = function(num, dec){
  // rounds a number to a certain decimal place and always maintains a decimal point
  let rounded = Math.round(num * Math.pow(10, dec)) / Math.pow(10, dec)
  return rounded.toFixed(dec)
}

bart.clamp = function(x, min, max){
  return Math.min(Math.max(x, min), max)
}

// generates the data from the functional form user input
// data is linearly interpolated between the points and then rnorm noise is added
bart.generateData = function(){
  let data = bart.functional.movedPoints
  let newData = JSON.parse(JSON.stringify(data)) // deep copy
  let n = bart.data.observations.filter(d => d.z == '1').length
  let rangeX = bart.functional.scales.xScale.domain()

  // generate noise
  let sdX = 10
  let sdY = 50 // same as generate-data.R
  let noiseX = d3.range(n).map(function(i){
    return jStat.normal.sample(0, sdX);
  })
  let noiseY = d3.range(n).map(function(i){
    return jStat.normal.sample(0, sdY);
  })

  // interpolate points
  let newX = newData.map(d => +d.caloriesConsumed )
  let newY = newData.map(d => +d.runningTime )
  newX = bart.interpolateArray(newX, n)
  newY = bart.interpolateArray(newY, n)

  // convert back to object
  let interpData = d3.zip(newX, newY)
  interpData.map((d, i) => d.caloriesConsumed = newX[i] )
  interpData.map((d, i) => d.runningTime = newY[i] )

  // add noise
  interpData.map( (d,i) => bart.clamp( d.caloriesConsumed = +d.caloriesConsumed + noiseX[i], rangeX[0], rangeX[1] ))
  interpData.map( (d,i) => d.runningTime = +d.runningTime + noiseY[i])

  // add treatment id
  interpData.map( (d,i) => d.z = '1' )

  return interpData;
}

// http://jsfiddle.net/sbv2jj9m/4/
bart.interpolateArray = function(data, fitCount) {
    
  var linearInterpolate = function (before, after, atPoint) {
      return before + (after - before) * atPoint;
  };
  
  var newData = new Array();
  var springFactor = new Number((data.length - 1) / (fitCount - 1));
  newData[0] = data[0]; // for new allocation
  for ( var i = 1; i < fitCount - 1; i++) {
    var tmp = i * springFactor;
    var before = new Number(Math.floor(tmp)).toFixed();
    var after = new Number(Math.ceil(tmp)).toFixed();
    var atPoint = tmp - before;
    newData[i] = linearInterpolate(data[before], data[after], atPoint);
    }
  newData[fitCount - 1] = data[data.length - 1]; // for new allocation
  return newData;
};

bart.emphasizeText = function(selectors){
  // de-emphasize this text
  d3.selectAll(".bart-text-along-d3 > p, .bart-text-along-d3 > h2")
      .style('filter', 'opacity(0.2)')
  // emphasize this text
  d3.selectAll(selectors)
      .style('filter', null)
}

// bart.changeStudyText = function(blank){
//   let ATE = fundamental.roundNumber(fundamental.data.studyLine, 2)
//   if (blank) ATE = "____"
//   // console.log(runner.length)
//   let newText = "Your study had an ATE of " + ATE
//   d3.select("#fundamental-study-text").text(newText)
// }

// add lines
bart.addLines = function(container, data, y0, y1, scales){
  const {xScale, yScale, colorScale} = scales
  const class0 = "bart-lines bart-lines-" + y0
  const class1 = "bart-lines bart-lines-" + y1

  // add y0 line
  container.append('path')
    .datum(data.fits)
    .attr('d', d3.line()
      .x(d => xScale(d.caloriesConsumed))
      .y(d => yScale(d[y0]))
    )
    .style("stroke", colorScale('0'))
    .style('fill', 'none')
    .style('display', 'none')
    .attr('class', class0)

  // add y1 line
  container.append('path')
    .datum(data.fits)
    .attr('d', d3.line()
      .x(d => xScale(d.caloriesConsumed))
      .y(d => yScale(d[y1]))
    )
    .style("stroke", colorScale('1'))
    .style('fill', 'none')
    .style('display', 'none')
    .attr('class', class1)
}

bart.addTitle = function(container){
  let containerTitle = container.append('g').attr('class', 'bart-title-group')
  // add title
  containerTitle
    .append('text')
    .attr('x', 0)
    .attr('y', -35)
    .text('Heterogeneous treatment effects')
    .attr('class', 'bart-title')
  
  // add subtitle
  containerTitle
    .append('text')
    .attr('x', 0)
    .attr('y', -15)
    .attr('class', 'bart-subtitle')
}

bart.addLegend = function(container, scales, config){
  const {xScale, yScale, colorScale} = scales
  const {bodyWidth, bodyHeight, margin} = config
  
  let legend = container
    .append('g')
    .attr("class", "bart-legend")
    .attr("transform", 
          "translate(" + bodyWidth*7/9 + "," + (0 - (margin.bottom)) + ")")

  legend.append("circle")
    .attr("cx", 0)
    .attr("cy", 10)
    .attr("r", 5)
    .style("fill", colorScale('0'))
  legend.append("circle")
    .attr("cx", 0)
    .attr("cy", 30)
    .attr("r", 5)
    .style("fill", colorScale('1'))
  legend.append("text")
    .attr("x", 10)
    .attr("y", 10)
    .text("Control")
    .attr("dominant-baseline", "middle")
    .attr('class', 'bart-legend-text')
  legend.append("text")
    .attr("x", 10)
    .attr("y", 30)
    .text("Treatment")
    .attr("dominant-baseline", "middle")
    .attr('class', 'bart-legend-text')
}

// reset the pointer events for each scrollytell state
bart.updatePointerOnScroll = function(container, dataSelector){
    
  // enable pointer events
  container.select('.bart-hoverRect').attr('pointer-events', 'all')

  // turn off line and tooltip (briefly, so it resets)
  bart.verticalLine.style('display', 'none')
  bart.tooltip.style('display', 'none')
  
  // adjust data for vertical line
  d3.map(bart.data.fits, x => { x.scroll0 = x[dataSelector + "Fit0"] } )
  d3.map(bart.data.fits, x => { x.scroll1 = x[dataSelector + "Fit1"] } )
}

// mouse event for main plot
bart.onMouseMove = function() {
  const { xScale, yScale } = bart.scales;

  // get mouse location and limit to range
  let mouseX = d3.mouse(this)[0];
  const mouseRange = [
    xScale(d3.min(bart.data.observations, d => d.caloriesConsumed)),
    xScale(d3.max(bart.data.observations, d => d.caloriesConsumed))
  ]
  mouseX = bart.clamp(mouseX, mouseRange[0], mouseRange[1]); 

  // recover coordinate from the fitted line
  const data = Array.from(bart.data.fits, d => d.caloriesConsumed);
  const x0 = xScale.invert(mouseX);
  const bisect = d3.bisector(function(d) { return d; }).left
  const i = bisect(data, x0, 1);

  // get the closest data point
  const selectedData = bart.data.fits[i-1]
  
  // update vertical line position
  bart.verticalLine
    .attr('x1', mouseX)
    .attr('x2', mouseX)
    .attr('y1', yScale(selectedData.scroll0))
    .attr('y2', yScale(selectedData.scroll1))
    .style('display', null)

  // update tooltip
  const ICATE = bart.roundNumber(selectedData.scroll1 - selectedData.scroll0, 1)
  let ATE = d3.mean(bart.data.fits, d => d.scroll1) - d3.mean(bart.data.fits, d => d.scroll0)
  ATE = bart.roundNumber(ATE, 1)
  bart.tooltip
    .style('display', null)
    .html(`<p style='font-weight: 500'>Average treatment effect: ${ATE}<br>Conditional average treatment effect: ${ICATE}`)
    .style("left", (d3.event.layerX + 20) + "px")
    .style("top", (d3.event.layerY + 5) + "px")
}

// reset the posterior plot
bart.resetPosteriorPlot = function(){
  const data = bart.data

  // replace button
  let newButton = $('<button id="bart-trigger" onclick="bart.triggerPosteriorAnimation()">Animate</button>')
  $('#bart-reset').after(newButton)
  $('#bart-reset').remove()

  // remove existing
  d3.select('#bart-plot-posterior1 > svg').remove()

  // build posterior plot
  const configPosterior = bart.getConfig("#bart-plot-posterior1");
  bart.posterior1.config = configPosterior;
  const scalesPosterior = bart.getScales(data, configPosterior);
  bart.posterior1.scales = scalesPosterior;
  bart.drawPosteriorPlot(data, scalesPosterior, configPosterior);
}
