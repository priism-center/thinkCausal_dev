
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
  // add title
  container
    .append('text')
    .attr('x', 0)
    .attr('y', -35)
    .text('Heterogeneous treatment effects')
    .attr('class', 'bart-title')
  
  // add subtitle
  container
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
            "translate(" + bodyWidth*2.5/9 + " ," + (0 - (margin.bottom*3/5)) + ")")

  const width = 500;
  legend.append("circle")
    .attr("cx", width*0.48)
    .attr("cy", 10)
    .attr("r", 5)
    .style("fill", colorScale('0'))
  legend.append("circle")
    .attr("cx", width*0.48)
    .attr("cy", 30)
    .attr("r", 5)
    .style("fill", colorScale('1'))
  legend.append("text")
    .attr("x", width*0.5)
    .attr("y", 12)
    .text("Control")
    .attr("alignment-baseline", "middle")
    .attr('class', 'bart-legend-text')
  legend.append("text")
    .attr("x", width*0.5)
    .attr("y", 32)
    .text("Treatment")
    .attr("alignment-baseline","middle")
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
  mouseX = Math.min(Math.max(parseInt(mouseX), mouseRange[0]), mouseRange[1]); 

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
    .html(`<p style='font-weight: 500'>Average treatment effect: ${ATE}<br>Individual conditional average treatment effect: ${ICATE}`)
    .style("left", (d3.event.layerX + 20) + "px")
    .style("top", (d3.event.layerY + 5) + "px")
}

// animate the posterior plot
bart.triggerPosteriorAnimation = function(){
  let container = bart.posterior.config.container

  // replace button
  let newButton = $('<button id="bart-reset" onclick="bart.resetPosteriorPlot()">Reset animation</button>')
  $('#bart-trigger').after(newButton)
  $('#bart-trigger').remove()

  //// animate first point
  let firstPointIndex = 12 // set in bart.drawPosteriorPlot()
  // let newCoords = 

  // temp
  let xCoordAxis = 50
  let xCoord = 150
  let yCoord = 400
  let quantiles = {x_5: 100, x_20: 125, x_80: 175, x_95: 200}

  // move line down
  container.select('.bart-verticalLine')
      .transition()
      .duration(2000)
      .delay(200)
      .attr('x1', xCoordAxis)
      .attr('x2', xCoord)
      .attr('y1', yCoord)
      .attr('y2', yCoord)
      .style('display', null)
  
  // create subsgroup
  // let containerAnim = container.append('g').attr('class', 'bart-animated-plot')
  
  // convert to circle
  container.append("circle")
    .style('opacity', 0)
    .transition()
    .duration(2000)
    .delay(3000)
    .attr("cx", xCoord)
    .attr("cy", yCoord)
    .attr("r", 5.5)
    .attr('class', 'bart-animated-plot')
    .style('stroke-width', 0.8)
    .style('fill', 'black')
    .style('stroke', "#fff")
    .transition()
    .duration(500)
    .style('opacity', 0.8);
  container.select('.bart-verticalLine')
    .transition()
    .delay(6000)
    .duration(500)
    .style('opacity', 0)
    .remove()
  
  // add distribution
  container.append('line')
    .attr('class', 'bart-posterior-distribution')
    .attr('x1', quantiles.x_5)
    .attr('x2', quantiles.x_95)
    .attr('y1', yCoord)
    .attr('y2', yCoord)
    .attr('class', 'bart-animated-plot')
    .style('stroke', 'black')
    .style('opacity', 0)
    .transition()
    .duration(1000)
    .delay(8250)
    .style('opacity', 1)
    container.append('line')
    .attr('class', 'bart-posterior-distribution')
    .attr('x1', quantiles.x_20)
    .attr('x2', quantiles.x_80)
    .attr('y1', yCoord)
    .attr('y2', yCoord)
    .attr('class', 'bart-animated-plot')
    .style('stroke', 'black')
    .style('stroke-width', 3)
    .style('opacity', 0)
    .transition()
    .duration(1000)
    .delay(6750)
    .style('opacity', 1)

  // add labels
  container.append('text')
    .attr("x", xCoord - 29)
    .attr("y", yCoord + 25)
    .text("|← 80th →|")
    .style('font-size', "12px")
    .attr("alignment-baseline", "middle")
    .style('opacity', 0)
    .transition()
    .duration(1000)
    .delay(7000)
    .style('opacity', 1)
  container.append('text')
    .attr("x", xCoord - 47)
    .attr("y", yCoord + 45)
    .html("|← &nbsp&nbsp&nbsp&nbsp&nbsp 95th &nbsp&nbsp&nbsp&nbsp&nbsp →|")
    .style('font-size', "12px")
    .attr("alignment-baseline", "middle")
    .style('opacity', 0)
    .transition()
    .duration(1000)
    .delay(8250)
    .style('opacity', 1)

  // remove original plot
  let itemsToRemove = ['g', 'path', 'text']
  container.selectAll(itemsToRemove)
    .transition()
    .duration(1000)
    .delay(11000)
    .style('opacity', 0)
    .transition()
    .delay(12000)
    .remove()
  
  // add axis

  // add points
}

// reset the posterior plot
bart.resetPosteriorPlot = function(){
  const data = bart.data

  // replace button
  let newButton = $('<button id="bart-trigger" onclick="bart.triggerPosteriorAnimation()">Animate</button>')
  $('#bart-reset').after(newButton)
  $('#bart-reset').remove()

  // remove existing
  d3.select('#bart-plot-posterior > svg').remove()

  // build posterior plot
  const configPosterior = bart.getConfig("#bart-plot-posterior");
  bart.posterior.config = configPosterior;
  const scalesPosterior = bart.getScales(data, configPosterior);
  bart.posterior.scales = scalesPosterior;
  bart.drawPosteriorPlot(data, scalesPosterior, configPosterior);
}