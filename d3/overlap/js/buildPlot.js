
overlap.getConfig = function(selector) {
  const width = 540; //900px is width of learning article * 0.6
  const height = 400;
  const margin = {
      top: 60,
      bottom: 50,
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

  return { width, height, margin, bodyHeight, bodyWidth, container, selector }
}

overlap.getScales = function(data, config) {
  const { bodyWidth, bodyHeight, container } = config;
  const maxX = d3.max(data.observations, d => +d.caloriesConsumed);
  const minX = d3.min(data.observations, d => +d.caloriesConsumed);
  const maxY = d3.max(data.observations, d => +d.runningTime);
  const minY = d3.min(data.observations, d => +d.runningTime);
  const padding = (maxX - minX) * 0.06

  const xScale = d3.scaleLinear()
    .domain([minX - padding, maxX + padding])
    .range([0, bodyWidth])
  const yScale = d3.scaleLinear()
    .domain([minY - padding, maxY + padding]) 
    .range([bodyHeight, 0])

  const colorScale = d3.scaleOrdinal()
    .domain(["0", "1", 'true'])
    .range(["#21918c", "#9D8420", '#303030'])

 return { xScale, yScale, colorScale }
}

overlap.drawPlot = function(data, scales, config){
  let { width, height, margin, container, bodyHeight, bodyWidth } = config;
  const { xScale, yScale, colorScale } = scales
  // console.log('Data into overlap.drawPlot():', data)

  // add all plot elements as a group for ease of later removal
  container = container.append('g').attr('class', 'overlap-basePlot')

  // add X axis
  const xAxis = d3.axisBottom(xScale).tickSize(-bodyHeight)
  container.append("g")
    .attr('class', "overlap-axis overlap-xAxis")
    .attr("transform", "translate(0," + bodyHeight + ")")
    .call(xAxis);
  container.append('text')
    .attr('class', 'overlap-axisLabel')
    .attr("x", bodyWidth/2)
    .attr('y', bodyHeight + margin.bottom*3/4)
    .attr('text-anchor', 'middle')
    .text("Calories consumed")

  // add Y axis
  const yAxis = d3.axisLeft(yScale).tickSize(-bodyWidth)
  container.append("g")
    .attr('class', "overlap-axis overlap-yAxis")
    .call(yAxis);
  container.append('text')
    .attr('class', 'overlap-axisLabel')
    .attr('x', -margin.left-10)
    .attr('y', bodyHeight/2)
    .attr('text-anchor', 'middle')
    .attr("transform", "rotate(-90,-" + (margin.left-10) + "," + bodyHeight/2 + ")")
    .text("Running time (seconds)");

  // draw observations scatter
  container.append('g')
    .attr('class', 'overlap-observations-group')
    .selectAll(".overlap-observations")
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
      .attr('treatment', d => d.z)
      .attr('class', 'overlap-observations')


  
  // add title
  overlap.addTitle(container);

  // add legend
  overlap.addLegend(container, scales, config);
}


// initialize plot
overlap.showData = function(data) {

  // build main plot
  const config = overlap.getConfig("#overlap-plot");
  overlap.config = config;
  const scales = overlap.getScales(data, config);
  overlap.scales = scales;
  overlap.drawScrollyPlot(data, scales, config);
  config.container.selectAll('.overlap-observations').style('opacity', 0)

}
