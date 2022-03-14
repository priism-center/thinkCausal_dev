// https://setosa.io/ev/principal-component-analysis/
// https://setosa.io/ev/principal-component-analysis/script.js
// https://octoperf.com/blog/2018/04/18/d3-js-drag-and-drop-tutorial/#the-d3-drag-library

function getConfig(){
  let width = 600;
  let height = 550;
  let margin = {
      top: 30,
      bottom: 80,
      left: 60,
      right: 20
  }

  // the body is the area that will be occupied by the plot
  let bodyHeight = height - margin.top - margin.bottom;
  let bodyWidth = width - margin.left - margin.right;

  // the container is the SVG where we will draw the plot
  let container = d3.select("#plot-scatter")
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
 data = data.scatter
 let { bodyWidth, bodyHeight } = config;
 let maxX = d3.max(data, d => +d.xName);
 let minX = d3.min(data, d => +d.xName);
 let maxY = d3.max(data, d => +d.yName);
 let minY = d3.min(data, d => +d.yName);

 let xScale = d3.scaleLinear()
     .domain([minX, maxX])
     .range([0, bodyWidth])

 let yScale = d3.scaleLinear()
     // .domain([minY, maxY])
     .domain([0, maxY])
     .range([bodyHeight/2, 0])

 let colorScale = d3.scaleOrdinal()
      .domain(["Eastern", "Western"])
      .range(["#183b32", "#D7837F"])

 return {xScale, yScale, colorScale}
}

function drawData(data, config, scales){
  let {margin, container, bodyHeight, bodyWidth, width, height} = config;
  let {xScale, yScale, colorScale} = scales;
  console.log('Data into drawData():', data)

  let dataLine = data.line
  data = data.scatter

  let meanY = d3.mean(data, d => +d.yName);
  let meanX = d3.mean(data, d => +d.xName);
  let minY = d3.min(data, d => +d.yName);
  let maxY = d3.max(data, d => +d.yName);
  let minX = d3.min(data, d => +d.xName);
  let maxX = d3.max(data, d => +d.xName);

  // add X axis
  container.append("g")
    .attr('class', "axis xAxis")
    .attr("transform", "translate(0," + bodyHeight/2 + ")")
    .call(d3.axisBottom(xScale));
  container.append('text')
    .attr('class', 'axisLabel')
    .attr("x", xScale(meanX))
    .attr('y', bodyHeight/2 + margin.bottom/2)
    .attr('text-anchor', 'middle')
    .text("My x axis label")

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
    .text("My y axis label")

  // draw scatter
  let pointOpacity = 0.8
  container.append('g')
    .selectAll("myCircles")
    .data(data)
    .enter()
    .append("circle")
      .attr("cx", d => xScale(d.xName))
      .attr("cy", d => yScale(d.yName))
      .attr("r", 4)
      .style('opacity', pointOpacity)
      .style('fill', d => colorScale(d.treatment))
      .style('stroke', '#fff')
      .style('stroke-width', 0.7)
      .on('mouseover', mouseover)
      .on('mousemove', mousemove)
      .on('mouseleave', mouseleave)
      .attr('class', 'currentPoints')

    // histogram
    // add X axis
    container.append("g")
      .attr('class', "axis xAxis")
      .attr("transform", "translate(0," + bodyHeight + ")")
      .call(d3.axisBottom(xScale));
    container.append('text')
      .attr('class', 'axisLabel')
      .attr("x", xScale(meanX))
      .attr('y', bodyHeight + margin.bottom/2)
      .attr('text-anchor', 'middle')
      .text("Histogram: My x axis label")

    // Add Y axis
    container.append('text')
      .attr('class', 'axisLabel')
      .attr('x', (-margin.left-10) * 4)
      .attr('y', yScale(meanY))
      .attr('text-anchor', 'middle')
      .attr("transform", "rotate(-90,-" + (margin.left-10) + "," + yScale(meanY) + ")")
      .text("Count")

  // create a tooltip
  let tooltip = d3.select("#plot-scatter")
    .append("div")
    .style("opacity", 0)
    .attr("class", "tooltip")

  function mouseover(d){
    tooltip
      .style('opacity', 1)
      .style('display', 'block')

    // de-emphasize other points
    d3.selectAll('.currentPoints')
      .style('opacity', 0.4)

    // emphasize this current point tho
    d3.select(this)
      .style('opacity', 1)
      .style('fill', '#394E48')
  }

  function mousemove(d){
    tooltip
      .html("<p style='font-weight: 700'>Treatment: " + d.treatment)
      .style("left", (d3.event.pageX + 20) + "px")
      .style("top", (d3.event.pageY + 20) + "px")
  }

  function mouseleave(d){
    tooltip
      .style('opacity', 0)
      .style('display', 'none')

    // re-emphasize other points
    d3.selectAll('.currentPoints')
      .style('opacity', pointOpacity)
      .style('fill', d => colorScale(d.treatment))
  }

  // // add subtitle
  // container
  //   .append('text')
  //   .attr('class', 'subtitle')
  //   .attr('x', -55)
  //   .attr('y', -20)
  //   .text('Hover over points to see team history')
  //
  // // add legend
  // let legend = container
  //   .append('g')
  //   .attr("class", "legend")
  //   .attr("transform",
  //           "translate(" + bodyWidth*2.5/9 + " ," + (bodyHeight + (margin.bottom*2.5/5)) + ")")
  // legend.append("circle").attr("cx",10).attr("cy",25).attr("r", 6).style("fill", "#183b32")
  // legend.append("circle").attr("cx",100).attr("cy",25).attr("r", 6).style("fill", "#80b0a4")
  // legend.append("text").attr("x", 25).attr("y", 30).text("Eastern").attr("alignment-baseline","middle")
  // legend.append("text").attr("x", 115).attr("y", 30).text("Western Conference").attr("alignment-baseline","middle")
}

function triggerAnimation(data, scales){
  let {xScale, yScale, colorScale} = scales;
  binData(data)

  d3.selectAll('circle')
    .transition()
    .duration(2500)
    .attr("cx", d => xScale(d.xBin))
    .attr("cy", d => yScale(d.yCount)*2)
    .delay(d => Math.random() * 300)
    // .delay(d => d.xName * 200) // controls left-to-right delay
    .ease(d3.easeBounceOut) //https://github.com/d3/d3-ease
    // .ease(d3.easeCubicOut)
    // .ease(d3.easeBackIn)
}

function binData(data){
  // calculate new x values
  d3.map(data, d => d.xBin = Math.round(d.xName * 3) / 3) // modify bin count here

  // calculate new y values
  // for each bin, add sequential count for each obsevation in a bin
  let bins = d3.nest().key(d => d.xBin).entries(store.scatter)
  d3.map(bins, function(d) {
    d.values.forEach(function(dv, i){ dv.yCount = i + 1})
  })

  return(data)
}


function buildPlot(data){
  config = getConfig()
  scales = getScales(data, config)
  drawData(data, config, scales)
}

function resetPlot(){
  d3.select('#plot-scatter svg').remove()
  buildPlot(store)
}