// namespace for posterior distribution plot
bart.animation = {}

bart.animation.getScales = function(data, config) {
  const { bodyWidth, bodyHeight } = config;
  const maxX = 0 //d3.max(data.credibleIntervals, d => +d.q_95);
  const minX = d3.min(data.credibleIntervals, d => +d.q_250);
  const maxY = 1
  const minY = data.credibleIntervals.length
  const paddingX = 10 //(maxX - minX) * 0.075
  const paddingY = -2.5 //(maxY - minY) * 0.075

  const xScale = d3.scaleLinear()
    .domain([minX - paddingX, maxX + paddingX])
    .range([0, bodyWidth])
  const yScale = d3.scaleLinear()
    .domain([minY - paddingY, maxY + paddingY]) 
    .range([bodyHeight, 0])

  const colorScale = d3.scaleOrdinal()
    .domain(["0", "1"])
    .range(["#21918c", "#440154"])

  return {xScale, yScale, colorScale}
}

bart.animation.drawDistributionPlot = function(data, scales, config){
  let { margin, container, bodyHeight, bodyWidth } = config;
  const { xScale, yScale } = scales

  // add a group for this new plot
  container = container.append('g').attr('class', 'bart-distributionPlot')

  // add X axis
  const xAxis = d3.axisBottom(xScale).tickSize(-bodyHeight)
  container.append("g")
    .attr('class', "bart-axis bart-xAxis")
    .attr("transform", "translate(0," + bodyHeight + ")")
    .call(xAxis);
  container.append('text')
    .attr('class', 'bart-axisLabel')
    .attr("x", bodyWidth/2)
    .attr('y', bodyHeight + margin.bottom/2)
    .attr('text-anchor', 'middle')
    .text("Individual conditional average treatment effect (running time)")

  // add Y axis
  const yAxis = d3.axisLeft(yScale).tickSize(-bodyWidth)
  container.append("g")
    .attr('class', "bart-axis bart-yAxis")
    .call(yAxis);
  container.append('text')
    .attr('class', 'bart-axisLabel')
    .attr('x', -margin.left-10)
    .attr('y', bodyHeight/2)
    .attr('text-anchor', 'middle')
    .attr("transform", "rotate(-90,-" + (margin.left-10) + "," + bodyHeight/2 + ")")
    .text("Observations ?");
  
  // add the dots
  container.append('g')
    .selectAll(".bart-distribution-circles")
    .data(data.credibleIntervals)
    .enter()
    .append("circle")
      .attr("cx", d => xScale(d.x_mean))
      .attr("cy", d => yScale(d.index))
      .attr("r", 3.5)
      // .style('opacity', 0.8)
      .style('fill', '#696969')
      .style('stroke', "#fff")
      .style('stroke-width', 0.8)
      .attr('class', 'bart-distribution-circles')

  // add the interval distributions
  container.append('g')
    .selectAll("bart-distribution-line-90")
    .data(data.credibleIntervals)
    .enter()
    .append('line')
      .attr('x1', d => xScale(+d.q_250))
      .attr('x2', d => xScale(+d.q_975))
      .attr('y1', d => yScale(+d.index))
      .attr('y2', d => yScale(+d.index))
      .attr('class', 'bart-animated-plot')
      .style('stroke', '#696969')
      .style('stroke-width', '1px')
      .attr('class', 'bart-distribution-line-90')
  container.append('g')
    .selectAll("bart-distribution-line-80")
    .data(data.credibleIntervals)
    .enter()
    .append('line')
      .attr('x1', d => xScale(+d.q_10))
      .attr('x2', d => xScale(+d.q_90))
      .attr('y1', d => yScale(+d.index))
      .attr('y2', d => yScale(+d.index))
      .attr('class', 'bart-animated-plot')
      .style('stroke', '#696969')
      .style('stroke-width', '2px')
      .attr('class', 'bart-distribution-line-80')

   // add title
   bart.addTitle(container);
   container.select('.bart-title').text('Posterior distribution')
   container.select('.bart-subtitle').text('Bayesian Additive Regression Trees (BART)')

  // disable this plot on start
  container.attr('display', 'none')
}
  
// animate the posterior plot
bart.triggerPosteriorAnimation = function(){
  let container = bart.posterior.config.container

  // scales for the interval distribution plot
  const { xScale, yScale } = bart.animation.scales

  // replace button
  let newButton = $('<button id="bart-reset" onclick="bart.resetPosteriorPlot()">Reset animation</button>')
  $('#bart-trigger').after(newButton)
  $('#bart-trigger').remove()

  //// animate first point
  let firstPointIndex = bart.animation.startIndex

  // coords for the first point animation
  let xCoordAxis = xScale(0) //50
  let xCoord = xScale(bart.data.credibleIntervals[firstPointIndex].x_mean) //150
  let yCoord = yScale(bart.data.credibleIntervals[firstPointIndex].index) //400
  let quantiles = {
    x_250: xScale(bart.data.credibleIntervals[firstPointIndex].q_250), 
    x_10: xScale(bart.data.credibleIntervals[firstPointIndex].q_10), 
    x_90: xScale(bart.data.credibleIntervals[firstPointIndex].q_90), 
    x_975: xScale(bart.data.credibleIntervals[firstPointIndex].q_975)
  }

  // animate the line down
  container.select('.bart-verticalLine')
      .transition()
      .duration(2000)
      .delay(200)
      .attr('x1', xCoordAxis)
      .attr('x2', xCoord)
      .attr('y1', yCoord)
      .attr('y2', yCoord)
      .style('display', null)
  
  // convert to circle
  container.append("circle")
    .style('opacity', 0)
    .transition()
    .duration(2000)
    .delay(1500)
    .attr("cx", xCoord)
    .attr("cy", yCoord)
    .attr("r", 3.5)
    .attr('class', 'bart-animated-plot')
    .style('stroke-width', 0.8)
    .style('fill', '#696969')
    .style('stroke', "#fff")
    .transition()
    .duration(500)
    .style('opacity', null);
  container.select('.bart-verticalLine')
    .transition()
    .delay(6000)
    .duration(500)
    .style('opacity', 0)
    .remove()
  
  // add interval distribution
  container.append('line')
    .attr('class', 'bart-posterior-distribution')
    .attr('x1', quantiles.x_250)
    .attr('x2', quantiles.x_975)
    .attr('y1', yCoord)
    .attr('y2', yCoord)
    .attr('class', 'bart-animated-plot')
    .style('stroke', '#696969')
    .style('stroke-width', "1px")
    .style('opacity', 0)
    .transition()
    .duration(1000)
    .delay(8250)
    .style('opacity', 1)
  container.append('line')
    .attr('class', 'bart-posterior-distribution')
    .attr('x1', quantiles.x_10)
    .attr('x2', quantiles.x_90)
    .attr('y1', yCoord)
    .attr('y2', yCoord)
    .attr('class', 'bart-animated-plot')
    .style('stroke', '#696969')
    .style('stroke-width', "2px")
    .style('opacity', 0)
    .transition()
    .duration(1000)
    .delay(6750)
    .style('opacity', 1)

  // add labels
  container.append('text')
    .attr("x", xCoord - 37)
    .attr("y", yCoord + 25)
    .html("|← &nbsp&nbsp 80th &nbsp&nbsp →|")
    .style('font-size', "12px")
    .attr("alignment-baseline", "middle")
    .attr('class', 'bart-dist-label')
    .style('opacity', 0)
    .transition()
    .duration(1000)
    .delay(7000)
    .style('opacity', 1)
  container.append('text')
    .attr("x", xCoord - 57)
    .attr("y", yCoord + 45)
    .html("|← &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp 95th &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp →|")
    .style('font-size', "12px")
    .attr("alignment-baseline", "middle")
    .attr('class', 'bart-dist-label')
    .style('opacity', 0)
    .transition()
    .duration(1000)
    .delay(8250)
    .style('opacity', 1)

  // remove original plot
  container.selectAll('.bart-basePlot, .bart-dist-label')
    .transition()
    .duration(1000)
    .delay(11000)
    .style('opacity', 0)
    .transition()
    .delay(12000)
    .remove()
  
  // fade in full interval plot
  d3.selectAll('.bart-distributionPlot')
    .style('opacity', 0)
    .attr('display', null)
    .transition()
    .duration(3000)
    .delay(12500)
    .style('opacity', 1)
}
