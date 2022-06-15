
overlap.roundNumber = function(num, dec){
  // rounds a number to a certain decimal place and always maintains a decimal point
  let rounded = Math.round(num * Math.pow(10, dec)) / Math.pow(10, dec)
  return rounded.toFixed(dec)
}

overlap.clamp = function(x, min, max){
  return Math.min(Math.max(x, min), max)
}

overlap.emphasizeText = function(selectors){
  // de-emphasize this text
  d3.selectAll(".overlap-text-along-d3 > p, .overlap-text-along-d3 > h2")
      .style('filter', 'opacity(0.2)')
  // emphasize this text
  d3.selectAll(selectors)
      .style('filter', null)
}

overlap.addTitle = function(container){
  let containerTitle = container.append('g').attr('class', 'overlap-title-group')
  // add title
  containerTitle
    .append('text')
    .attr('x', 0)
    .attr('y', -40)
    .text('My title')
    .attr('class', 'overlap-title')
  
  // add subtitle
  containerTitle
    .append('text')
    .attr('x', 0)
    .attr('y', -20)
    .text('My subtitle')
    .attr('class', 'overlap-subtitle')
}

overlap.addLegend = function(container, scales, config){
  const {xScale, yScale, colorScale} = scales
  const {bodyWidth, bodyHeight, margin, selector} = config
  
  let legend = container
    .append('g')
    .attr("class", "overlap-legend")
    .attr("transform", 
          "translate(" + bodyWidth*0.8 + "," + (0 - (margin.bottom)) + ")")

  legend.append("circle")
    .attr("cx", 0)
    .attr("cy", 0)
    .attr("r", 5)
    .style("fill", colorScale('0'))
  legend.append("circle")
    .attr("cx", 0)
    .attr("cy", 18)
    .attr("r", 5)
    .style("fill", colorScale('1'))
  // legend.append("circle")
  //   .attr("cx", 0)
  //   .attr("cy", 36)
  //   .attr("r", 5)
  //   .style("fill", colorScale('true'))
  legend.append("text")
    .attr("x", 10)
    .attr("y", 0)
    .text("Control")
    .attr("dominant-baseline", "middle")
    .attr('class', 'overlap-legend-text')
  legend.append("text")
    .attr("x", 10)
    .attr("y", 18)
    .text("Treatment")
    .attr("dominant-baseline", "middle")
    .attr('class', 'overlap-legend-text')
  legend.append("text")
    .attr("x", 10)
    .attr("y", 36)
    .text("Show true surface")
    .attr("dominant-baseline", "middle")
    .attr('class', 'overlap-legend-text overlap-legend-text-true')
}
