function getConfigPlot(){
  let width = 500;
  let height = 400;
  let margin = {
      top: 30,
      bottom: 80,
      left: 60,
      right: 20
  }

  // the body is the area that will be occupied by the points
  let bodyHeight = height - margin.top - margin.bottom;
  let bodyWidth = width - margin.left - margin.right;

  // the container is the SVG where we will draw the plot
  let container = d3.select("#plot")
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
 let maxX = d3.max(data, d => +d.x);
 let minX = d3.min(data, d => +d.x);
 let maxY = d3.max(data, d => +d.y);
 let minY = d3.min(data, d => +d.y);

 let xScale = d3.scaleLinear()
     .domain([minX, maxX])
     .range([0, bodyWidth])

 let yScale = d3.scaleLinear()
     .domain([minY, maxY])
     .range([bodyHeight, 0])

 let colorScale = d3.scaleOrdinal()
      .domain(["positive", "negative"])
      .range(["#76758f", "#823329"])

 return {xScale, yScale, colorScale}
}

// main function to draws the points on the plot
function drawData(data, config, scales){
  let {margin, container, bodyHeight, bodyWidth, width, height} = config;
  let {xScale, yScale, colorScale} = scales;
  console.log('Data into drawData():', data)

  // calculate basic stats for later use
  let meanY = d3.mean(data, d => +d.y);
  let meanX = d3.mean(data, d => +d.y);
  let minY = d3.min(data, d => +d.y);
  let maxY = d3.max(data, d => +d.y);
  let minX = d3.min(data, d => +d.x)
  let maxX = d3.max(data, d => +d.x)

  // add X axis
  container.append("g")
    .attr('class', "axis xAxis")
    .attr("transform", "translate(0," + bodyHeight + ")")
    .call(d3.axisBottom(xScale));
  container.append('text')
    .attr('class', 'axisLabel')
    .attr("x", xScale(meanY))
    .attr('y', bodyHeight + margin.bottom/2)
    .attr('text-anchor', 'middle')
    .text("X")

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
    .text("Y")

  // TODO: add 'Click to draw line' when hovering over the plot?
  // create a tooltip
  // let tooltip = d3.select("#plot")
  //   .append("div")
  //   .style("opacity", 0)
  //   .attr("class", "tooltip")

  // function mouseover(d){
  //   tooltip
  //     .style('opacity', 1)
  //
  //   // de-emphasize other points
  //   d3.selectAll('.currentPoints')
  //     .style('opacity', 0.2)
  //
  //   // emphasize this current point tho
  //   d3.select(this)
  //     .style('opacity', 1)
  //     .style('fill', '#394E48')
  //
  //   // get team name and change the stroke with all values with this team name
  //   let name = d3.select(this).attr('team')
  //   d3.selectAll("[team=" + name + "_path]")
  //     .transition()
  //       .duration(150)
  //       .style('stroke-opacity', 1)
  //   d3.selectAll("[team=" + name + "_circle]")
  //     .transition()
  //       .duration(150)
  //       .style('opacity', 1)
  //
  //   // highlight row on table
  //   d3.selectAll('table').select('[team=' + name + ']')
  //     .style("background-color", '#e4ebe7')
  // }

  // function mousemove(d){
  //   tooltip
  //     .html("<p style='font-weight: 700'>#" + d.rank + ": " + d.team + "</p><p style='font-weight: 400; font-size: 0.9em'>Historical ratings shown</p>")
  //     .style("left", (d3.event.pageX + 20) + "px")
  //     .style("top", (d3.event.pageY + 20) + "px")
  // }

  // function mouseleave(d){
  //   tooltip
  //     .style('opacity', 0)
  //
  //   // re-emphasize other points
  //   d3.selectAll('.currentPoints')
  //     .style('opacity', 0.8)
  //     //.style('fill', '#71807b')
  //     .style('fill', d => colorScale(d.conference))
  //
  //   // get team name and change the stroke with all values with this team name
  //   let name = d3.select(this).attr('team')
  //   d3.selectAll("[team=" + name + "_path]")
  //     .transition()
  //       .duration(100)
  //       .style('stroke-opacity', 0)
  //   d3.selectAll("[team=" + name + "_circle]")
  //     .transition()
  //       .duration(100)
  //       .style('opacity', 0)
  //
  //   // de-highlight row on table
  //   d3.selectAll('table').select('[team=' + name + ']')
  //     .style("background-color", '#fff')
  // }

  // add historical lines
  // let historicalLine = d3.line()
  //     .curve(d3.curveCatmullRom.alpha(1))
  //     .x(d => xScale(d.rating_delta))
  //     .y(d => yScale(d.rating))
  // container.append('g')
  //     .selectAll('lines')
  //     .data(grouped)
  //     .enter()
  //     .append("path")
  //       .attr('d', d => historicalLine(d.values))
  //       .style('stroke-width', 1.5)
  //       .attr('stroke', '#394E48')
  //       //.attr('stroke', d => colorScale(d.conference))
  //       .style('fill', 'none')
  //       .style('stroke-opacity', 0)
  //       .attr('team', d => d.key + "_path")
  //
  // // add historical points
  // container.append('g')
  //   .selectAll('dot')
  //   .data(data)
  //   .enter()
  //   .append("circle")
  //     .attr("cx", d => xScale(d.rating_delta))
  //     .attr("cy", d => yScale(d.rating))
  //     .attr("r", 3)
  //     .style("fill", "#394E48")
  //     //.style('fill', d => colorScale(d.conference))
  //     .style("stroke", '#fff')
  //     .style('opacity', 0)
  //     .attr('team', d => d.team + "_circle")

  // define functions to handle line drawing
  // TODO only registers when clicking on circle; does not draw line; could have issues with translate and scale
  //https://stackoverflow.com/questions/18273884/live-drawing-of-a-line-in-d3-js
  var line;
  function mousedown(){
    var m = d3.mouse(this);
    line = container.append('line')
      .attr('x1', m[0])
      .attr('y1', m[1])
      .attr('x2', m[0])
      .attr('y2', m[1])

    console.log(xScale(m[0]));
    console.log(yScale(m[1]));

    container.on('mousemove', mousemove);
  }
  function mousemove() {
      var m = d3.mouse(this);
      line
        .attr("x2", m[0])
        .attr("y2", m[1]);
  }
  function mouseup() {
    container.on("mousemove", null);
  }

  container
    .on("mousedown", mousedown)
    .on("mouseup", mouseup);

  // add current points
  container.append('g')
    .selectAll("dot")
    .data(data)
    .enter()
    .append("circle")
      .attr("cx", d => xScale(d.x))
      .attr("cy", d => yScale(d.y))
      .attr("r", 6)
      .style('opacity', 0.8)
      .style('fill', d => colorScale(d.label))
      .style('stroke', '#fff')
      .attr('class', 'currentPoints')
      // .attr('team', d => d.team)
      // .attr('teamShape', d => d.team + "_currentCircle")
      // .on('mouseover', mouseover)
      // .on('mousemove', mousemove)
      // .on('mouseleave', mouseleave)
    .exit()

  // add subtitle
  // container
  //   .append('text')
  //   .attr('class', 'subtitle')
  //   .attr('x', -55)
  //   .attr('y', -20)
  //   .text('Hover over points to see team history')

  // add vertical line
  container.append("line")
    .attr("x1", xScale(30))
    .attr('y1', 0)
    .attr('x2', xScale(30))
    .attr('y2', bodyHeight)
    .style("stroke-width", 2.5)
    .style("stroke-dasharray", ("5, 5"))
    .style("stroke", "black")
    .style("fill", "none");

  // add horizontal line
  container.append("line")
    .attr("x1", xScale(30))
    .attr('y1', yScale(50))
    .attr('x2', bodyWidth)
    .attr('y2', yScale(50))
    .style("stroke-width", 2.5)
    .style("stroke-dasharray", ("5, 5"))
    .style("stroke", "black")
    .style("fill", "none");

  // add vertical line
  container.append("line")
    .attr("x1", xScale(65))
    .attr('y1', yScale(50))
    .attr('x2', xScale(65))
    .attr('y2', bodyHeight)
    .style("stroke-width", 2.5)
    .style("stroke-dasharray", ("5, 5"))
    .style("stroke", "black")
    .style("fill", "none");

  // add legend
  let legend = container
    .append('g')
    .attr("class", "legend")
    .attr("transform",
            "translate(" + bodyWidth*2.5/9 + " ," + (bodyHeight + (margin.bottom*2.5/5)) + ")")
  legend.append("circle").attr("cx",55).attr("cy",25).attr("r", 6).style("fill", "#76758f")
  legend.append("circle").attr("cx",145).attr("cy",25).attr("r", 6).style("fill", "#823329")
  legend.append("text").attr("x", 70).attr("y", 30).text("Positive").attr("alignment-baseline","middle")
  legend.append("text").attr("x", 160).attr("y", 30).text("Negative").attr("alignment-baseline","middle")
}

function buildScatter(data){
  config = getConfigPlot()
  scales = getScales(data, config)
  drawData(data, config, scales)
}
