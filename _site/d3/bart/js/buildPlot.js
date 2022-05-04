
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

  return {width, height, margin, bodyHeight, bodyWidth, container}
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
  const {width, height, margin, container, bodyHeight, bodyWidth} = config;
  const {xScale, yScale, colorScale} = scales
  console.log('Data into bart.drawPlot():', data)


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
  const pointOpacity = 0.8
  const pointRadius = 5.5
  const strokeColor = '#fff'
  const strokeWidth = 0.8
  container.append('g')
    .selectAll("bart-observations")
    .data(data.observations)
    .enter()
    .append("circle")
      .attr("cx", d => xScale(d.caloriesConsumed))
      .attr("cy", d => yScale(d.runningTime))
      .attr("r", pointRadius)
      .style('opacity', pointOpacity)
      .style('fill', d => colorScale(d.z))
      .style('stroke', strokeColor)
      .style('stroke-width', strokeWidth)
      .attr('class', 'bart-observations')


  // add fitted lines
  bart.addLines(container, data, "diffFit0", "diffFit1", scales)
  bart.addLines(container, data, "lmFit0", "lmFit1", scales)
  bart.addLines(container, data, "bartFit0", "bartFit1", scales)
  
  // add title
  bart.addTitle(container);

  // add legend
  bart.addLegend(container, scales, config)

  // add vertical line
  bart.verticalLine = container
    .append('line')
    .attr('class', 'bart-verticalLine')
  
  // add tooltip
  bart.tooltip = d3.select('body') //select("#bart-plot") //container
    .append("div")
    .style("display", 'none')
    .attr("class", "bart-tooltip")

  // add rect to hold mouse event that updates the vertical line
  container
    .append('rect')
    .style("fill", "none")
    .attr("pointer-events", "all")
    .style("z-index", "200")
    .attr('width', bodyWidth)
    .attr('height', bodyHeight)
    .on("mousemove", bart.onMouseMove)
    // .on("mouseover", bart.onMouseMove)
    .on('mouseleave', function(){
      bart.verticalLine.style('display', 'none')
      bart.tooltip.style('display', 'none')
    })
    .attr('class', 'bart-hoverRect');
}

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
  // console.log(selectedData)

  // get pixel locations
  const closestYBottomPx = yScale(selectedData.scroll1)
  const closestYTopPx = yScale(selectedData.scroll0)
  
  // update vertical line position
  bart.verticalLine
    .attr('x1', mouseX)
    .attr('x2', mouseX)
    .attr('y1', closestYBottomPx)
    .attr('y2', closestYTopPx)
    .style('display', null)

  // tooltip (TODO: issue with positioning; convert to d3 shape instead of div?)
  const ICE = bart.roundNumber(selectedData.scroll0 - selectedData.scroll1, 1)
  let ATE = d3.mean(bart.data.fits, d => d.scroll0) - d3.mean(bart.data.fits, d => d.scroll1)
  ATE = bart.roundNumber(ATE, 1)
  bart.tooltip
    .style('display', null)
    .html(`<p style='font-weight: 500'>Average treatment effect: ${ATE}<br>Individual causal effect: ${ICE}`)
    .style("left", (d3.event.pageX + 20) + "px")
    .style("top", (d3.event.pageY + 5) + "px")
}

// initialize plot
bart.showData = function(data) {
  const config = bart.getConfig("#bart-plot");
  bart.config = config;
  const scales = bart.getScales(data, config);
  bart.scales = scales;
  bart.drawPlot(data, scales, config);
}
