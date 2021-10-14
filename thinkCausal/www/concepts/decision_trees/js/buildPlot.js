function getConfig(){
  let width = 600;
  let height = 550;
  let margin = {
      top: 30,
      bottom: 80,
      left: 60,
      right: 20
  }

  //The body is the area that will be occupied by the bars.
  let bodyHeight = height - margin.top - margin.bottom;
  let bodyWidth = width - margin.left - margin.right;

  //The container is the SVG where we will draw the chart
  let container = d3.select("#plotRank")
    .append("svg")
      .attr("class", "plot")
      .attr("preserveAspectRatio", "xMinYMin meet")
      .attr("viewBox", "0 0 " + width + " " + height)
    .append("g")
      .attr("transform",
            "translate(" + margin.left + "," + margin.top + ")")

  return {width, height, margin, bodyHeight, bodyWidth, container}
}

function getScales(data, config) {
 let { bodyWidth, bodyHeight } = config;
 let maxX = d3.max(data, d => +d.rating_delta);
 let minX = d3.min(data, d => +d.rating_delta);
 let maxY = d3.max(data, d => +d.rating);
 let minY = d3.min(data, d => +d.rating);

 let xScale = d3.scaleLinear()
     .domain([minX, maxX])
     .range([0, bodyWidth])

 let yScale = d3.scaleLinear()
     .domain([minY, maxY])
     .range([bodyHeight, 0])

 let colorScale = d3.scaleOrdinal()
      .domain(["Eastern", "Western"])
      .range(["#183b32", "#80b0a4"])

 return {xScale, yScale, colorScale}
}

function drawData(data, config, scales){
  let {margin, container, bodyHeight, bodyWidth, width, height} = config;
  let {xScale, yScale, colorScale} = scales;
  console.log('Data into drawData():', data)

  let meanY = d3.mean(data, d => +d.rating);
  let minY = d3.min(data, d => +d.rating);
  let maxY = d3.max(data, d => +d.rating);
  let minX = d3.min(data, d => +d.rating_delta)
  let maxX = d3.max(data, d => +d.rating_delta)

  // group the data for drawing the historical lines
  let grouped = d3.nest()
    .key(d => d.team)
    .entries(data)
  console.log('Grouped data:', grouped)

  // filter the data for the current data
  latestData = data.filter(d => {return d.latest == 1})

  // add X axis
  container.append("g")
    .attr('class', "axis xAxis")
    .attr("transform", "translate(0," + bodyHeight + ")")
    .call(d3.axisBottom(xScale));
  container.append('text')
    .attr('class', 'axisLabel')
    .attr("x", xScale(0))
    .attr('y', bodyHeight + margin.bottom/2)
    .attr('text-anchor', 'middle')
    .text("Rating trend over last 10 games")

  // Add Y axis
  container.append("g")
    .attr('class', "axis yAxis")
    .call(d3.axisLeft(yScale));
  container.append('text')
    .attr('class', 'axisLabel')
    .attr('x', -margin.left-10)
    .attr('y', yScale(meanY))
    .attr('text-anchor', 'middle')
    .attr("transform", "rotate(-90,-" + (margin.left-10) + "," + yScale(meanY) + ")")
    .text("Current rating")

  // create a tooltip
  let tooltip = d3.select("#plotRank")
    .append("div")
    .style("opacity", 0)
    .attr("class", "tooltip")

  function mouseover(d){
    tooltip
      .style('opacity', 1)

    // de-emphasize other points
    d3.selectAll('.currentPoints')
      .style('opacity', 0.2)

    // emphasize this current point tho
    d3.select(this)
      .style('opacity', 1)
      .style('fill', '#394E48')

    // get team name and change the stroke with all values with this team name
    let name = d3.select(this).attr('team')
    d3.selectAll("[team=" + name + "_path]")
      .transition()
        .duration(150)
        .style('stroke-opacity', 1)
    d3.selectAll("[team=" + name + "_circle]")
      .transition()
        .duration(150)
        .style('opacity', 1)

    // highlight row on table
    d3.selectAll('table').select('[team=' + name + ']')
      .style("background-color", '#e4ebe7')
  }

  function mousemove(d){
    tooltip
      .html("<p style='font-weight: 700'>#" + d.rank + ": " + d.team + "</p><p style='font-weight: 400; font-size: 0.9em'>Historical ratings shown</p>")
      .style("left", (d3.event.pageX + 20) + "px")
      .style("top", (d3.event.pageY + 20) + "px")
  }

  function mouseleave(d){
    tooltip
      .style('opacity', 0)

    // re-emphasize other points
    d3.selectAll('.currentPoints')
      .style('opacity', 0.8)
      //.style('fill', '#71807b')
      .style('fill', d => colorScale(d.conference))

    // get team name and change the stroke with all values with this team name
    let name = d3.select(this).attr('team')
    d3.selectAll("[team=" + name + "_path]")
      .transition()
        .duration(100)
        .style('stroke-opacity', 0)
    d3.selectAll("[team=" + name + "_circle]")
      .transition()
        .duration(100)
        .style('opacity', 0)

    // de-highlight row on table
    d3.selectAll('table').select('[team=' + name + ']')
      .style("background-color", '#fff')
  }

  // add quadrant identifiers
  container.append("text")
    .attr("class", "quadrantText")
    .attr("x", xScale(minX/2))
    .attr("y", yScale(meanY + (maxY-meanY)/2))
    .style("text-anchor", "middle")
    .html("HIGH BUT DECLINING")
 container.append("text")
    .attr("class", "quadrantText")
    .attr("x", xScale(maxX/2))
    .attr("y", yScale(meanY + (maxY-meanY)/2))
    .style("text-anchor", "middle")
    .html("HIGH AND INCREASING")
  container.append("text")
    .attr("class", "quadrantText")
    .attr("x", xScale(minX/2))
    .attr("y", yScale(meanY - (meanY-minY)/2) + 10)
    .style("text-anchor", "middle")
    .html("LOW AND DECLINING")
 container.append("text")
    .attr("class", "quadrantText")
    .attr("x", xScale(maxX/2))
    .attr("y", yScale(meanY - (meanY-minY)/2) + 10)
    .style("text-anchor", "middle")
    .html("LOW BUT INCREASING")

  // add vertical line
  container.append("line")
    .attr("x1", xScale(0))
    .attr('y1', 0)
    .attr('x2', xScale(0))
    .attr('y2', bodyHeight)
    .style("stroke-width", 1.5)
    .style("stroke-dasharray", ("3, 3"))
    .style("stroke", "#c4c4c4")
    .style("fill", "none");

  // add horizontal line
  container.append("line")
    .attr("x1", 0)
    .attr('y1', yScale(meanY))
    .attr('x2', bodyWidth)
    .attr('y2', yScale(meanY))
    .style("stroke-width", 1.5)
    .style("stroke-dasharray", ("3, 3"))
    .style("stroke", "#c4c4c4")
    .style("fill", "none");

  // add historical lines
  let historicalLine = d3.line()
      .curve(d3.curveCatmullRom.alpha(1))
      .x(d => xScale(d.rating_delta))
      .y(d => yScale(d.rating))
  container.append('g')
      .selectAll('lines')
      .data(grouped)
      .enter()
      .append("path")
        .attr('d', d => historicalLine(d.values))
        .style('stroke-width', 1.5)
        .attr('stroke', '#394E48')
        //.attr('stroke', d => colorScale(d.conference))
        .style('fill', 'none')
        .style('stroke-opacity', 0)
        .attr('team', d => d.key + "_path")

  // add historical points
  container.append('g')
    .selectAll('dot')
    .data(data)
    .enter()
    .append("circle")
      .attr("cx", d => xScale(d.rating_delta))
      .attr("cy", d => yScale(d.rating))
      .attr("r", 3)
      .style("fill", "#394E48")
      //.style('fill', d => colorScale(d.conference))
      .style("stroke", '#fff')
      .style('opacity', 0)
      .attr('team', d => d.team + "_circle")

  // add current points
  container.append('g')
    .selectAll("dot")
    .data(latestData)
    .enter()
    .append("circle")
      .attr("cx", d => xScale(d.rating_delta))
      .attr("cy", d => yScale(d.rating))
      .attr("r", 8)
      .style('opacity', 0.8)
      //.style("fill", "#adadad")
//      .style('fill', '#71807b')
      .style('fill', d => colorScale(d.conference)) //394E48
      .style('stroke', '#fff')
      .attr('class', 'currentPoints')
      .attr('team', d => d.team)
      .attr('teamShape', d => d.team + "_currentCircle")
      .on('mouseover', mouseover)
      .on('mousemove', mousemove)
      .on('mouseleave', mouseleave)

  // add subtitle
  container
    .append('text')
    .attr('class', 'subtitle')
    .attr('x', -55)
    .attr('y', -20)
    .text('Hover over points to see team history')

  // add legend
  let legend = container
    .append('g')
    .attr("class", "legend")
    .attr("transform",
            "translate(" + bodyWidth*2.5/9 + " ," + (bodyHeight + (margin.bottom*2.5/5)) + ")")
  legend.append("circle").attr("cx",10).attr("cy",25).attr("r", 6).style("fill", "#183b32")
  legend.append("circle").attr("cx",100).attr("cy",25).attr("r", 6).style("fill", "#80b0a4")
  legend.append("text").attr("x", 25).attr("y", 30).text("Eastern").attr("alignment-baseline","middle")
  legend.append("text").attr("x", 115).attr("y", 30).text("Western Conference").attr("alignment-baseline","middle")
}

function buildPlot(data){
  config = getConfig()
  scales = getScales(data, config)
  drawData(data, config, scales)
}
