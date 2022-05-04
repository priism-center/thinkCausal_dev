
// controls the delay on the rugplot animation
bart.delayFn = function(index){ return ((((((index+1)**0.001)-1) * 5000000)) / 1.3) + 100 } // accelerating curve

// store timeouts for later clearing
bart.timeouts = []
bart.killkdeAnimations = function(){
    bart.timeouts.map(clearTimeout)
    bart.timeouts = []
}

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
  let {xScale, yScale, colorScale} = scales
  let class0 = "bart-lines bart-lines-" + y0
  let class1 = "bart-lines bart-lines-" + y1

  // add y0 line
  container.append('path')
    .datum(data.fits)
    .attr('d', d3.line()
      .x(d => xScale(d.caloriesConsumed))
      .y(d => yScale(d[y0]))
    )
    .attr('treatment', '0')
    .style("stroke", colorScale('0'))
    .style('fill', 'none')
    .attr('class', class0)

  // add y1 line
  container.append('path')
    .datum(data.fits)
    .attr('d', d3.line()
      .x(d => xScale(d.caloriesConsumed))
      .y(d => yScale(d[y1]))
    )
    .attr('treatment', '0')
    .style("stroke", colorScale('1'))
    .style('fill', 'none')
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
  let {xScale, yScale, colorScale} = scales
  let {bodyWidth, bodyHeight, margin} = config
  
  let legend = container
    .append('g')
    .attr("class", "legend")
    .attr("transform",
            "translate(" + bodyWidth*2.5/9 + " ," + (0 - (margin.bottom*3/5)) + ")")

  let width = 500;
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