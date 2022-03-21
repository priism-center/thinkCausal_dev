// https://www.d3-graph-gallery.com/graph/interactivity_transition.html

estimands.getConfig = function(){
  let width = 540; //900px is width of learning article * 0.6
  let height = 400;
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

estimands.getScales = function(data, config) {
  data = data.scatter
  let { bodyWidth, bodyHeight } = config;
  let maxX = d3.max(data, d => +d.xName);
  let minX = d3.min(data, d => +d.xName);
  let maxY = d3.max(data, d => +d.yName);
  let minY = d3.min(data, d => +d.yName);

  let xAxisBuffer = 0.2
  let yAxisBuffer = 0.5

  let xScale = d3.scaleLinear()
     .domain([minX - xAxisBuffer, maxX + xAxisBuffer])
     .range([0, bodyWidth])

  let yScale = d3.scaleLinear()
    //  .domain([minY, maxY])
     .domain([0, maxY + yAxisBuffer])
     .range([bodyHeight, 0])

  let colorScale = d3.scaleOrdinal()
      .domain(["0", "1"])
      .range(["#fff", "grey"])

  let strokeScale = d3.scaleOrdinal()
      .domain(["0", "1"])
      .range(["#21918c", "#440154"])

  return {xScale, yScale, colorScale, strokeScale}
}

estimands.drawData = function(data, config, scales){
  var {margin, container, bodyHeight, bodyWidth, width, height} = config;
  let {xScale, yScale, colorScale, strokeScale} = scales;
  console.log('Data into drawData():', data)

  let meanY = d3.mean(data.scatter, d => +d.yName);
  let meanX = d3.mean(data.scatter, d => +d.xName);
  let minY = d3.min(data.scatter, d => +d.yName);
  let maxY = d3.max(data.scatter, d => +d.yName);
  let minX = d3.min(data.scatter, d => +d.xName);
  let maxX = d3.max(data.scatter, d => +d.xName);

  let strokeColor = '#6e6e6e'
  let strokeWidth = 2.5
  let pointOpacity = 0.8
  let pointRadius = 7



  // add X axis
  let xAxis = d3.axisBottom(xScale)
  xAxis.ticks(3);
  xAxis.tickValues([0, 1]);
  let tickLabels = ['y0', 'y1']
  xAxis.tickFormat((d, i) => tickLabels[i])
  container.append("g")
    .attr('class', "axis xAxis")
    .attr("transform", "translate(0," + bodyHeight + ")")
    .call(xAxis);

  // add Y axis
  container.append("g")
    .attr('class', "axis yAxis")
    .call(d3.axisLeft(yScale));
  container.append('text')
    .attr('class', 'axisLabel')
    .attr('x', -margin.left-10)
    .attr('y', yScale(meanY))
    .attr('text-anchor', 'middle')
    .attr("transform", "rotate(-90,-" + (margin.left-10) + "," + yScale(meanY) + ")")
    .text("Running time")



  // draw ICE lines connecting the points
  container.append('g')
    .selectAll('line')
    .data(data.line)
    .enter()
    .append('line')
      .attr('x1', d => xScale(d.xName_y0))
      .attr('y1', d => yScale(d.yName_y0))
      .attr('x2', d => xScale(meanX))
      .attr('y2', d => yScale((d.yName_y0)))
      .style('stroke', "#21918c")
      .style('stroke-width', strokeWidth * 2/3)
      .style('display', 'none')
      .attr('pairID', d => d.pair_id)
      .attr('class', 'showOnHover')
  container.append('g')
    .selectAll('line')
    .data(data.line)
    .enter()
    .append('line')
      .attr('x1', d => xScale(meanX))
      .attr('y1', d => yScale(d.yName_y0))
      .attr('x2', d => xScale(meanX))
      .attr('y2', d => yScale((d.yName_y1)))
      .style('stroke', strokeColor)
      .style('stroke-width', strokeWidth * 2/3)
      .style('display', 'none')
      .attr('pairID', d => d.pair_id)
      .attr('class', 'line-dashed showOnHover')
  container.append('g')
    .selectAll('line')
    .data(data.line)
    .enter()
    .append('line')
      .attr('x1', d => xScale(meanX))
      .attr('y1', d => yScale(d.yName_y1))
      .attr('x2', d => xScale(d.xName_y1))
      .attr('y2', d => yScale((d.yName_y1)))
      .style('stroke', '#440154')
      .style('stroke-width', strokeWidth * 2/3)
      .style('display', 'none')
      .attr('pairID', d => d.pair_id)
      .attr('class', 'showOnHover')

  // add endpoints to ICE lines
  container.append('g')
    .selectAll('endCircles')
    .data(data.line)
    .enter()
    .append('circle')
      .attr("cx", d => xScale(meanX))
      .attr("cy", d => yScale(d.yName_y0))
      .attr("r", pointRadius * 0.7)
      .attr('fill', function(d) {
        if (d.treatment == "0") return '#21918c'
        return "#fff"
      })
      .attr('stroke', "#21918c")
      .attr('stroke-width', 2)
      .style('display', 'none')
      .attr('class', 'endCircle showOnHover')
      .attr('pairID', d => d.pair_id)
  container.append('g')
    .selectAll('endCircle')
    .data(data.line)
    .enter()
    .append('circle')
      .attr("cx", d => xScale(meanX))
      .attr("cy", d => yScale(d.yName_y1))
      .attr("r", pointRadius * 0.7)
      .attr('fill', function(d) {
        if (d.treatment == "1") return '#440154'
        return "#fff"
      })
      .attr('stroke', "#440154")
      .attr('stroke-width', 2)
      .style('display', 'none')
      .attr('class', 'endCircle showOnHover')
      .attr('pairID', d => d.pair_id)

  // add ICE label
  container.append('g')
    .selectAll('rect')
    .data(data.line)
    .enter()
    .append('rect')
      .attr('width', 50)
      .attr('height', 27)
      .attr('x', d => xScale(meanX * 0.97))
      .attr('y', d => yScale(((+d.yName_y0 + +d.yName_y1) / 2) + 0.2))
      .style('fill', '#fff')
      .style('display', 'none')
      .attr('pairID', d => d.pair_id)
      .attr('class', 'ICElabel showOnHover')
  container.append('g')
    .selectAll('text')
    .data(data.line)
    .enter()
    .append('text')
      .attr('x', d => xScale(meanX * 0.75))
      .attr('y', d => yScale((+d.yName_y0 + +d.yName_y1) / 2))
      .text(function(d) {
        let diff = d.yName_y1 - d.yName_y0
        let label = "Runner " + d.pair_id + " ICE: " + Math.round(diff * 100) / 100
        return label
      })
      .style('display', 'none')
      .attr('pairID', d => d.pair_id)
      .attr('class', 'ICElabel showOnHover')



  // draw scatter
  container.append('g')
    .selectAll("myCircles")
    .data(data.scatter)
    .enter()
    .append("circle")
      .attr("cx", d => xScale(d.xName))
      .attr("cy", d => yScale(d.yName))
      .attr("r", pointRadius)
      .style('opacity', pointOpacity)
      .style('fill', function(d) {
        if (d.factual === '0') return '#fff'
        if (d.treatment === '0') return "#21918c"
        return "#440154"
      })
      .style('stroke', d => strokeScale(d.y))
      .style('stroke-width', strokeWidth)
      .on('mouseover', mouseover)
      .on('mousemove', mousemove)
      .on('mouseleave', mouseleave)
      .attr('class', 'scatter scatterPoints')
      .attr('factual', function(d) {
        if (d.factual === '1') return 'factual'
        return 'counterfactual'
      })
      .attr('pairID', d => d.pair_id)
      .attr("pointer-events", "none")
  // hide counterfactual points
  d3.selectAll(".scatterPoints[factual='counterfactual']")
    .style('display', 'none')

  // show mean lines
  let meanYy0 = d3.mean(data.line, d => +d.yName_y0);
  let meanYy1 = d3.mean(data.line, d => +d.yName_y1);
  container.append('g')
    .append('line')
      .attr('x1', xScale(-0.1))
      .attr('y1', yScale(meanYy0))
      .attr('x2', xScale(0.1))
      .attr('y2', yScale(meanYy0))
      .style('stroke', "#333333")
      .style('stroke-width', 5)
      .style('display', 'none')
      .attr('class', 'meanLines')
  container.append('g')
    .append('line')
      .attr('x1', xScale(0.9))
      .attr('y1', yScale(meanYy1))
      .attr('x2', xScale(1.1))
      .attr('y2', yScale(meanYy1))
      .style('stroke', '#333333')
      .style('stroke-width', 5)
      .style('display', 'none')
      .attr('class', 'meanLines')
  
  // add ICE ATE line
  let ATE = d3.mean(data.line, d => d.yName_y0 - d.yName_y1)
  console.log(ATE)
  container.append('g')
    .append('line')
      .attr('x1', xScale(-0.1))
      .attr('y1', yScale(ATE))
      .attr('x2', xScale(1.1))
      .attr('y2', yScale(ATE))
      .style('stroke', "#333333")
      .style('stroke-width', 5)
      .style('display', 'none')
      .attr('class', 'ICEATEline')
    


  // create a tooltip
  let tooltip = d3.select("#plot-scatter")
    .append("div")
    .style("opacity", 0)
    .attr("class", "tooltip")

  function mouseover(d){
    // tooltip
    //   .style('opacity', 1)
    //   .style('display', null)

    // make sure all other points are not shown
    d3.selectAll('.showOnHover')
      .style('display', 'none')

    // get the pair ID for this highlighted point
    let pairID = d3.select(this).attr('pairID')

    // de-emphasize points not in pairing
    d3.selectAll(".scatterPoints")
      .style('opacity', 0.2)
    d3.selectAll(".scatterPoints[pairID='" + pairID + "']")
      .style('opacity', 1)
      .attr('r', pointRadius*1.2)
      .style('filter', 'brightness(0.9)')

    // emphasize lines
    d3.selectAll(".showOnHover[pairID='" + pairID + "']")
      .style('display', null)

    // emphasize legend
    // d3.selectAll("text[treatment='" + treatment + "']")
    //   .style('font-weight', 700)
    // d3.selectAll("text[treatment='" + other_treatment + "']")
    //   .style('fill', '#d9d9d9')
  }

  function mousemove(d){
    // tooltip
    //   .html("<p style='font-weight: 700'>Treatment: " + d.treatment)
    //   .style("left", (d3.event.pageX + 20) + "px")
    //   .style("top", (d3.event.pageY + 20) + "px")
  }

  function mouseleave(d){
    // tooltip
    //   .style('opacity', 0)
    //   .style('display', 'none')

    // remove font weight from legend
    // d3.selectAll("text")
    //   .style('font-weight', null)
    //   .style('fill', null)

    // re-emphasize other points
    d3.selectAll('.scatterPoints')
      .style('opacity', pointOpacity)
      .style('filter', null)
      .attr('r', pointRadius)

    d3.selectAll('.showOnHover')
      .style('display', 'none')
  }

  // add subtitle
  container
    .append('text')
    .attr('class', 'subtitle')
    .attr('x', 0)
    .attr('y', -10)
    .text('Calculated average running times - lower is better')

  // add legend
  let legend = container
    .append('g')
    .attr("class", "legend")
    .attr("transform",
            "translate(" + -bodyWidth*1/2.3 + " ," + (0 + (bodyHeight * 1.17)) + ")")
  legend.append("circle")
    .attr("cx", width*0.48)
    .attr("cy", -5)
    .attr("r", 5)
    .style("fill", strokeScale('0'))
  legend.append("circle")
    .attr("cx", width*0.70)
    .attr("cy", -5)
    .attr("r", 5)
    .style("fill", strokeScale('1'))
  legend.append("text")
    .attr("x", width*0.5)
    .attr("y", 0)
    .text("Standard shoes (y0)")
    .attr("alignment-baseline","middle")
    .attr('treatment', '0')
  legend.append("text")
    .attr("x", width*0.72)
    .attr("y", 0)
    .text("HyperShoes (y1)")
    .attr("alignment-baseline","middle")
    .attr('treatment', '1')
  legend.append("circle")
      .attr("cx", width*0.92)
      .attr("cy", -5)
      .attr("r", 5)
      .style("fill", '#fff')
      .style("stroke", 'grey')
  legend.append("circle")
    .attr("cx", width*1.1)
    .attr("cy", -5)
    .attr("r", 5)
    .style("fill", 'grey')
  legend.append("text")
    .attr("x", width*0.94)
    .attr("y", 0)
    .text("Counterfactual")
    .attr("alignment-baseline","middle")
    .attr('treatment', '0')
  legend.append("text")
    .attr("x", width*1.12)
    .attr("y", 0)
    .text("Factual")
    .attr("alignment-baseline","middle")
    .attr('treatment', '1')
}

estimands.buildPlot = function(data){
  config = estimands.getConfig()
  scales = estimands.getScales(data, config)
  estimands.drawData(data, config, scales)
}

// function resetPlot(){
//   // replace button
//   let newButton = $('<button id="trigger" onclick="triggerAnimation(estimands.data.scatter, scales)">Build histogram</button>')
//   $('#reset').after(newButton)
//   $('#reset').remove()

//   d3.select('#plot-scatter svg').remove()
//   buildPlot(estimands.data)
// }
