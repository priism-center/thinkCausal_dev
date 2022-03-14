// https://www.d3-graph-gallery.com/graph/interactivity_transition.html

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
      .domain(["0", "1"])
      .range(["#21918c", "#440154"])

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
      .style('stroke-width', 0.5)
      .on('mouseover', mouseover)
      .on('mousemove', mousemove)
      .on('mouseleave', mouseleave)
      .attr('class', 'scatter scatterPoints')
      .attr('treatment', d => d.treatment)
  // add same points again that will not fall
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
      .style('stroke-width', 0.5)
      .on('mouseover', mouseover)
      .on('mousemove', mousemove)
      .on('mouseleave', mouseleave)
      .attr('class', 'scatterPoints-fixed')
      .attr('treatment', d => d.treatment)
      .style('display', 'none')

  // draw fitted lines
  container.append('path')
    .datum(dataLine)
    .attr('class', 'line')
    .attr('treatment', '0')
    .style('stroke', '#183b32')
    .attr('d', d3.line()
      .x(d => xScale(d.x0Name))
      .y(d => yScale(d.y0Name))
    )
    container.append('path')
      .datum(dataLine)
      .attr('class', 'line')
      .attr('treatment', '1')
      .style('stroke', '#D7837F')
      .attr('d', d3.line()
        .x(d => xScale(d.x1Name))
        .y(d => yScale(d.y1Name))
      )

    // histogram
    // add X axis
    container.append("g")
      .attr('class', "axis xAxis")
      .attr("transform", "translate(0," + bodyHeight + ")")
      .call(d3.axisBottom(xScale))
      .style('display', 'none')
    container.append('text')
      .attr('class', 'axisLabel')
      .attr("x", xScale(meanX))
      .attr('y', bodyHeight + margin.bottom/2)
      .attr('text-anchor', 'middle')
      .text("Histogram: My x axis label")
      .style('display', 'none')

    // Add Y axis
    container.append('text')
      .attr('class', 'axisLabel')
      .attr('x', (-margin.left-10) * 4)
      .attr('y', yScale(meanY))
      .attr('text-anchor', 'middle')
      .attr("transform", "rotate(-90,-" + (margin.left-10) + "," + yScale(meanY) + ")")
      .text("Count")
      .style('display', 'none')

  // create a tooltip
  let tooltip = d3.select("#plot-scatter")
    .append("div")
    .style("opacity", 0)
    .attr("class", "tooltip")

  function mouseover(d){
    tooltip
      .style('opacity', 1)
      .style('display', null)

    // de-emphasize points not in treatment gorup
    let treatment = d3.select(this).attr('treatment')
    let other_treatment = Math.abs(+treatment - 1)
    d3.selectAll("circle[treatment='" + other_treatment + "'], path[treatment='" + other_treatment + "']")
      .style('opacity', 0.2)
    d3.selectAll("circle[treatment='" + treatment + "'], path[treatment='" + treatment + "']")
      .style('opacity', 1)

    // emphasize legend
    d3.selectAll("text[treatment='" + treatment + "']")
      .style('font-weight', 700)
    d3.selectAll("text[treatment='" + other_treatment + "']")
      .style('fill', '#d9d9d9')

    // emphasize this current point tho
    d3.select(this)
      .style('opacity', 1)
      .style('filter', 'brightness(0.8)')
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

    // remove font weight from legend
    d3.selectAll("text")
      .style('font-weight', null)
      .style('fill', null)

    // re-emphasize other points
    d3.selectAll('circle, path')
      .style('opacity', pointOpacity)
      .style('filter', null)
  }

  // add subtitle
  container
    .append('text')
    .attr('class', 'subtitle')
    .attr('x', 0)
    .attr('y', -10)
    .text('My title')

  // add legend
  let legend = container
    .append('g')
    .attr("class", "legend")
    .attr("transform",
            "translate(" + bodyWidth*2.5/9 + " ," + (0 - (margin.bottom*3/5)) + ")")
  legend.append("circle")
    .attr("cx", width*0.48)
    .attr("cy", 35)
    .attr("r", 5)
    .style("fill", colorScale('0'))
    .attr('treatment', '0')
    .on('mouseover', mouseover)
    .on('mousemove', d => tooltip.style('display', 'none'))
    .on('mouseleave', mouseleave)
  legend.append("circle")
    .attr("cx", width*0.48)
    .attr("cy", 55)
    .attr("r", 5)
    .style("fill", colorScale('1'))
    .attr('treatment', '1')
    .on('mouseover', mouseover)
    .on('mousemove', d => tooltip.style('display', 'none'))
    .on('mouseleave', mouseleave)
  legend.append("text")
    .attr("x", width*0.5)
    .attr("y", 40)
    .text("Control")
    .attr("alignment-baseline","middle")
    .attr('treatment', '0')
    .on('mouseover', mouseover)
    .on('mousemove', d => tooltip.style('display', 'none'))
    .on('mouseleave', mouseleave)
  legend.append("text")
    .attr("x", width*0.5)
    .attr("y", 60)
    .text("Treatment")
    .attr("alignment-baseline","middle")
    .attr('treatment', '1')
    .on('mouseover', mouseover)
    .on('mousemove', d => tooltip.style('display', 'none'))
    .on('mouseleave', mouseleave)
}

function triggerAnimation(data, scales){
  let {xScale, yScale, colorScale} = scales;
  binData(data)

  let xOffset = 0.3
  let ySqueeze = 0.6

  // show axis and labels
  d3.selectAll('.axis, .axisLabel')
    .transition()
    .delay(900)
    .style('display', null)

  // make points fall
  d3.selectAll('.scatterPoints')
    .transition()
    .duration(2500)
    .attr("cx", d => xScale(d.xBin + ((d.treatment == 1) * xOffset/2) - ((d.treatment == 0) * xOffset/2)))
    .attr("cy", d => yScale(d.yCount * ySqueeze) * 2)
    .delay(d => Math.random() * 400)
    // .delay(d => d.xName * 200) // controls left-to-right delay
    .ease(d3.easeBounceOut) //https://github.com/d3/d3-ease
    // .ease(d3.easeCubicOut)
    // .ease(d3.easeBackIn)

  // add points back
  d3.selectAll('.scatterPoints-fixed')
    .style('opacity', 0)
    .transition()
    .duration(2000)
    .delay(4000)
    .style('display', null)
    .style('opacity', null)
}

function binData(data){
  // calculate new x values
  let binsPerInteger = 1
  d3.map(data, d => d.xBin = Math.round(d.xName * binsPerInteger) / binsPerInteger) // modify bin count here

  // calculate new y values
  // for each bin, add sequential count for each obsevation in a bin
  let bins = d3.nest().key(d => [d.xBin, d.treatment]).entries(store.scatter)
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

