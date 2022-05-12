
bart.getConfig = function(selector) {
  const width = 540; //900px is width of learning article * 0.6
  const height = 400;
  const margin = {
      top: 50,
      bottom: 80,
      left: 60,
      right: 20
  }

  // the body is the area that will be occupied by the plot
  const bodyHeight = height - margin.top - margin.bottom;
  const bodyWidth = width - margin.left - margin.right;

  // the container is the SVG where we will draw the plot
  const container = d3.select(selector)
    .append("svg")
      .attr("class", "plot")
      .attr("preserveAspectRatio", "xMinYMin meet")
      .attr("viewBox", "0 0 " + width + " " + height)
    .append("g")
      .attr("transform",
            `translate(${margin.left},${margin.top})`)

  return {width, height, margin, bodyHeight, bodyWidth, container, selector}
}

bart.getScales = function(data, config) {
  const { bodyWidth, bodyHeight, container } = config;
  const maxX = d3.max(data.observations, d => +d.caloriesConsumed);
  const minX = d3.min(data.observations, d => +d.caloriesConsumed);
  const maxY = d3.max(data.observations, d => +d.runningTime);
  const minY = d3.min(data.observations, d => +d.runningTime);
  const padding = (maxX - minX) * 0.075

  const xScale = d3.scaleLinear()
    .domain([minX - padding, maxX + padding])
    .range([0, bodyWidth])
  const yScale = d3.scaleLinear()
    .domain([minY - padding, maxY + padding]) 
    .range([bodyHeight, 0])

  const colorScale = d3.scaleOrdinal()
    .domain(["0", "1"])
    .range(["#21918c", "#440154"])

 return {xScale, yScale, colorScale}
}

bart.drawPlot = function(data, scales, config){
  let {width, height, margin, container, bodyHeight, bodyWidth} = config;
  const {xScale, yScale, colorScale} = scales
  // console.log('Data into bart.drawPlot():', data)

  // add all plot elements as a group for ease of later removal
  container = container.append('g').attr('class', 'bart-basePlot')

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
    .text("Calories consumed")

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
    .text("Running time (seconds)");

  // draw observations scatter
  container.append('g')
    .selectAll("bart-observations")
    .data(data.observations)
    .enter()
    .append("circle")
      .attr("cx", d => xScale(d.caloriesConsumed))
      .attr("cy", d => yScale(d.runningTime))
      .attr("r", 5.5)
      .style('opacity', 0.8)
      .style('fill', d => colorScale(d.z))
      .style('stroke', "#fff")
      .style('stroke-width', 0.8)
      .attr('class', 'bart-observations')

  // add fitted lines
  let containerLines = container.append('g')
  bart.addLines(containerLines, data, "diffFit0", "diffFit1", scales)
  bart.addLines(containerLines, data, "lmFit0", "lmFit1", scales)
  bart.addLines(containerLines, data, "bartFit0", "bartFit1", scales)
  
  // add title
  bart.addTitle(container);

  // add legend
  bart.addLegend(container, scales, config)
}

bart.drawScrollyPlot = function(data, scales, config){
  const { width, height, margin, container, bodyHeight, bodyWidth, selector} = config;

  // draw base plot
  bart.drawPlot(data, scales, config);

  // add vertical line
  bart.verticalLine = config.container
    .append('line')
    .attr('class', 'bart-verticalLine')  

  // add tooltip
  bart.tooltip = d3.select(selector)
    .append("div")
    .style("display", 'none')
    .attr("class", "bart-tooltip")

  // add rect to hold mouse event that updates the vertical line and tooltip
  container
    .append('rect')
    .style("fill", "none")
    .attr("pointer-events", "all")
    .style("z-index", "2000")
    .attr('width', bodyWidth)
    .attr('height', bodyHeight)
    .on("mousemove", bart.onMouseMove)
    .on('mouseleave', function(){
      bart.verticalLine.style('display', 'none')
      bart.tooltip.style('display', 'none')
    })
    .attr('class', 'bart-hoverRect');
}

bart.drawPosteriorPlot = function(data, scales, config){
  const { container, selector } = config;
  const { xScale, yScale } = scales

  // draw base plot
  bart.drawPlot(data, scales, config);

  // remove everything but bart
  const itemsToRemove = '.bart-lines-diffFit0, .bart-lines-diffFit1, .bart-lines-lmFit0, .bart-lines-lmFit1'
  container.selectAll(itemsToRemove).remove()
  container.selectAll('.bart-lines-bartFit0, .bart-lines-bartFit1').style('display', null)

  // de-emphasize points
  container.selectAll('.bart-observations').style('opacity', 0.2)

  // add subtitle
  container.select('.bart-subtitle').text('Bayesian Additive Regression Trees (BART)')

  // add static vertical line
  const index = 12 // chosen subjectively
  bart.animation.startIndex = index
  const selectedObsData = bart.data.observations[index]
  const selectedFitsData = bart.data.fits.filter(d => {
    const val = bart.roundNumber(d.caloriesConsumed, 0)
    const match = bart.roundNumber(selectedObsData.caloriesConsumed, 0)
    return val == match
  })[0]
  container.append('line')
    .attr('class', 'bart-verticalLine')
    .attr('x1', xScale(selectedFitsData.caloriesConsumed))
    .attr('x2', xScale(selectedFitsData.caloriesConsumed))
    .attr('y1', yScale(selectedFitsData.bartFit0))
    .attr('y2', yScale(selectedFitsData.bartFit1))
    .style('display', null)

  // extend viewbox
  const newHeight = 600
  const newWidth = 540
  d3.selectAll(`${selector} > svg`)
      .attr('viewBox', '0 0 ' + newWidth + ' ' + newHeight)
  
  // draw distribution plot
  config.bodyHeight = newHeight - config.margin.top - config.margin.bottom;
  let scalesDistribution = bart.animation.getScales(data, config)
  bart.animation.config = config
  bart.animation.scales = scalesDistribution
  bart.animation.drawDistributionPlot(data, scalesDistribution, config)
}

// initialize plot
bart.showData = function(data) {

  // build main plot
  const config = bart.getConfig("#bart-plot");
  bart.config = config;
  const scales = bart.getScales(data, config);
  bart.scales = scales;
  bart.drawScrollyPlot(data, scales, config);
  config.container.selectAll('.bart-observations').style('opacity', 0)

  // build posterior plot
  bart.posterior = {}
  const configPosterior = bart.getConfig("#bart-plot-posterior");
  bart.posterior.config = configPosterior;
  const scalesPosterior = bart.getScales(data, configPosterior);
  bart.posterior.scales = scalesPosterior;
  bart.drawPosteriorPlot(data, scalesPosterior, configPosterior);
}
