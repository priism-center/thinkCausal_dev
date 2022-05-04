

bart.getConfig = function(selector) {
  let width = 540; //900px is width of learning article * 0.6
  let height = 400;
  let margin = {
      top: 50,
      bottom: 80,
      left: 60,
      right: 20
  }

  // the body is the area that will be occupied by the plot
  let bodyHeight = height - margin.top - margin.bottom;
  let bodyWidth = width - margin.left - margin.right;

  // the container is the SVG where we will draw the plot
  let container = d3.select(selector)
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
 let { bodyWidth, bodyHeight, container } = config;
 let maxX = d3.max(data.observations, d => +d.caloriesConsumed);
 let minX = d3.min(data.observations, d => +d.caloriesConsumed);
 let maxY = d3.max(data.observations, d => +d.runningTime);
 let minY = d3.min(data.observations, d => +d.runningTime);
 let padding = (maxX - minX) * 0.075

 let xScale = d3.scaleLinear()
    .domain([minX - padding, maxX + padding])
    .range([0, bodyWidth])
 let yScale = d3.scaleLinear()
    .domain([minY - padding, maxY + padding]) 
    .range([bodyHeight, 0])

let colorScale = d3.scaleOrdinal()
  .domain(["0", "1"])
  .range(["#21918c", "#440154"])

 return {xScale, yScale, colorScale}
}

bart.drawPlot = function(data, scales, config){
  let {width, height, margin, container, bodyHeight, bodyWidth} = config;
  let {xScale, yScale, colorScale} = scales
  console.log('Data into bart.drawPlot():', data)


  // add X axis
  let xAxis = d3.axisBottom(xScale).tickSize(-bodyHeight)
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
  let yAxis = d3.axisLeft(yScale).tickSize(-bodyWidth)
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
  let pointOpacity = 0.8
  let pointRadius = 5.5
  let strokeColor = '#fff'
  let strokeWidth = 0.8
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
  // TODO: change this from div to slider?
  bart.verticalLine = d3.select('#bart-plot')
    .append("div")
    .style("position", "absolute")
    .style("z-index", "19")
    .style("width", "2px")    
    .style("background", "#696969" )
    .attr('class', 'bart-verticalLine')

  // add rect to hold mouse event that updates the vertical line
  d3.select('#bart-plot > svg > g')
    .append('rect')
    .style("fill", "none")
    .style("pointer-events", "all")
    .style("z-index", "200")
    .attr('width', bodyWidth)
    .attr('height', bodyHeight)
    .on("mousemove", onMouseMove)
    // .on("mouseover", onMouseMove)
}

// intialize plot
bart.showData = function(data) {

    // initialize plot
    let config = bart.getConfig("#bart-plot");
    bart.config = config;
    let scales = bart.getScales(data, config);
    bart.scales = scales;
    bart.drawPlot(data, scales, config);
}


function onMouseMove() {
  const { xScale, yScale } = bart.scales;
  let { margin, height, bodyHeight } = bart.config;

  // get mouse location and limit to range
  let mouseX = d3.mouse(this)[0];
  mouseX = Math.min(Math.max(parseInt(mouseX), 30), 440); // not sure where these values come from

  // recover coordinate we need
  const data = Array.from(bart.data.fits, d => d.caloriesConsumed);
  const x0 = xScale.invert(mouseX);
  const bisect = d3.bisector(function(d) { return d; }).left
  const i = bisect(data, x0, 1);

  // get the closest data point
  const selectedData = bart.data.fits[i-1]
  // console.log(selectedData)

  // get pixel locations
  // TODO: make this reactive to scroll
  // const closestYBottomPx = yScale(selectedData.bartFit1)
  // const closestYTopPx = yScale(selectedData.bartFit0)
  const closestYBottomPx = yScale(selectedData.scroll1)
  const closestYTopPx = yScale(selectedData.scroll0)
  
  // update vertical line position
  bart.verticalLine.style("left", mouseX + margin.left + "px" )
  bart.verticalLine.style("bottom", (height - closestYBottomPx - margin.top - 4) + "px" )
  bart.verticalLine.style("top", (closestYTopPx + margin.top + 21) + "px" )
}
