// https://www.d3-graph-gallery.com/graph/interactivity_transition.html

estimands.getConfig = function(selector){
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
            "translate(" + margin.left + "," + margin.top + ")")
  
  // store the config for later animation
  estimands.config = {}
  estimands.config = {width, height, margin, bodyHeight, bodyWidth, container}
  estimands.config.heightTall = 700;
  
  return {width, height, margin, bodyHeight, bodyWidth, container, selector}
}

estimands.getScales = function(data, config) {
  data = data.scatter
  let { bodyWidth, bodyHeight } = config;
  let maxX = d3.max(data, d => +d.xName);
  let minX = d3.min(data, d => +d.xName);
  let maxY = d3.max(data, d => +d.yName);
  let minY = d3.min(data, d => +d.yName);
  let diffs = estimands.data.line.map(d => +d.yName_y1 - +d.yName_y0)
  let minDiff = Math.min(...diffs)
  let maxDiff = Math.max(...diffs)
  let pxGap = 65

  let xAxisBuffer = 0.2
  let yAxisBuffer = 3

  let xScale = d3.scaleLinear()
     .domain([minX - xAxisBuffer, maxX + xAxisBuffer])
     .range([0, bodyWidth])

  let yScale = d3.scaleLinear()
    //  .domain([minY, maxY])
     .domain([minY - yAxisBuffer, maxY + yAxisBuffer])
     .range([bodyHeight, 0])
  let yScaleBottomPlot = d3.scaleLinear()
    .domain([minDiff - 1, maxDiff + 1])
    .range([bodyHeight*2, bodyHeight + pxGap])

  let colorScale = d3.scaleOrdinal()
      .domain(["0", "1"])
      .range(["#fff", "grey"])

  let strokeScale = d3.scaleOrdinal()
      .domain(["0", "1"])
      .range(["#21918c", "#440154"])
  
  // store the scales
  estimands.scales = { xScale, yScale, colorScale, strokeScale, yScaleBottomPlot }

  return { xScale, yScale, colorScale, strokeScale, yScaleBottomPlot }
}

estimands.drawData = function(data, config, scales){
  var { margin, container, bodyHeight, bodyWidth, width, height, selector } = config;
  let { xScale, yScale, colorScale, strokeScale, yScaleBottomPlot } = scales;
  console.log('Data into estimands.drawData():', data)

  let meanY = d3.mean(data.scatter, d => +d.yName);
  let meanX = d3.mean(data.scatter, d => +d.xName);
  estimands.data.meanX = meanX;
  estimands.data.meanY = meanY;
  // let minY = d3.min(data.scatter, d => +d.yName);
  // let maxY = d3.max(data.scatter, d => +d.yName);
  // let minX = d3.min(data.scatter, d => +d.xName);
  // let maxX = d3.max(data.scatter, d => +d.xName);

  let strokeColor = '#6e6e6e'
  let strokeWidth = 2.5
  let pointOpacity = 0.8
  let pointRadius = 7
  estimands.styles = {strokeColor, strokeWidth, pointOpacity, pointRadius}



  // add X axis
  let xAxis = d3.axisBottom(xScale)
  xAxis.ticks(3);
  xAxis.tickValues([0, 1]);
  let tickLabels = ['y0', 'y1']
  xAxis.tickFormat((d, i) => tickLabels[i])
  container.append("g")
    .attr('class', "estimands-axis estimands-xAxis")
    .attr("transform", `translate(0,${bodyHeight})`)
    .call(xAxis);

  // add Y axis
  container.append("g")
    .attr('class', "estimands-axis estimands-yAxis")
    .call(d3.axisLeft(yScale).ticks(8));
  container.append('g')
    .attr('class', 'estimands-axis estimands-yAxisBottom')
    .call(d3.axisLeft(yScaleBottomPlot).ticks(6))
    .style('display', 'none')
  container.append('text')
    .attr('class', 'estimands-axisLabel estimands-yAxisLabel')
    .attr('x', -margin.left-10)
    .attr('y', yScale(meanY)+10)
    .attr('text-anchor', 'middle')
    .attr("transform", "rotate(-90,-" + (margin.left-10) + "," + yScale(meanY) + ")")
    .text("Running time")


  // create a tooltip
  // let tooltip = d3.select("#estimands-plot-ATE")
  //   .append("div")
  //   .style("opacity", 0)
  //   .attr("class", "tooltip")

  // add mouse events
  estimands.mouseover = function(d){
    // tooltip
    //   .style('opacity', 1)
    //   .style('display', null)

    // make sure all other points are not shown
    d3.selectAll('.estimands-showOnHover')
      .style('display', 'none')
      .transition() // kill any transitions

    // get the pair ID for this highlighted point
    let pairID = d3.select(this).attr('pairID')

    // de-emphasize points not in pairing
    d3.selectAll(".estimands-scatterPoints, .estimands-droppedPoints")
      .style('opacity', 0.2)
    d3.selectAll(".estimands-scatterPoints[pairID='" + pairID + "']")
      .style('opacity', 1)
      .attr('r', pointRadius*1.2)
      .style('filter', 'brightness(0.9)')
    d3.selectAll(".estimands-droppedPoints[pairID='" + pairID + "']")
      .style('opacity', 1)
      .attr('r', pointRadius*1.2*0.8)
      .style('filter', 'brightness(0.9)')
    
    // de-emphasize mean lines
    let meanLinesATCSelector = selector + ' .estimands-meanLinesATC, ' + selector + ' .estimands-meanLinesATCConnector, ' + selector + ' .estimands-meanLinesATCConnectorLabel'
    let meanLinesATTSelector = selector + ' .estimands-meanLinesATT, ' + selector + ' .estimands-meanLinesATTConnector, ' + selector + ' .estimands-meanLinesATTConnectorLabel'
    d3.selectAll(meanLinesATCSelector + ', ' + meanLinesATTSelector)
      .style('opacity', 0.05)

    // emphasize lines
    d3.selectAll(".estimands-showOnHover[pairID='" + pairID + "']")
      .style('display', null)
      .style('opacity', 1)
    
    // emphasize table row
    d3.selectAll(".estimands-table tr[pairID='" + pairID + "']")
      .style('font-weight', 700)
      .style('background-color', '#ebebeb')
    
    // change text in paragraph
    estimands.changeRunnerText(pairID)

    // emphasize legend
    // d3.selectAll("text[treatment='" + treatment + "']")
    //   .style('font-weight', 700)
    // d3.selectAll("text[treatment='" + other_treatment + "']")
    //   .style('fill', '#d9d9d9')
  }
  estimands.mousemove = function(d){
    // tooltip
    //   .html("<p style='font-weight: 700'>Treatment: " + d.treatment)
    //   .style("left", (d3.event.pageX + 20) + "px")
    //   .style("top", (d3.event.pageY + 20) + "px")
  }
  estimands.mouseleave = function(d){
    // tooltip
    //   .style('opacity', 0)
    //   .style('display', 'none')

    // remove font weight from legend
    // d3.selectAll("text")
    //   .style('font-weight', null)
    //   .style('fill', null)

    // re-emphasize other points
    d3.selectAll('.estimands-scatterPoints')
      .style('opacity', pointOpacity)
      .style('filter', null)
      .attr('r', pointRadius)
    d3.selectAll('.estimands-droppedPoints')
      .style('opacity', pointOpacity)
      .style('filter', null)
      .attr('r', pointRadius * 0.8)
    
    // re-emphasize mean lines
    let meanLinesATCSelector = selector + ' .estimands-meanLinesATC, ' + selector + ' .estimands-meanLinesATCConnector, ' + selector + ' .estimands-meanLinesATCConnectorLabel'
    let meanLinesATTSelector = selector + ' .estimands-meanLinesATT, ' + selector + ' .estimands-meanLinesATTConnector, ' + selector + ' .estimands-meanLinesATTConnectorLabel'
    d3.selectAll(meanLinesATCSelector + ', ' + meanLinesATTSelector)
      .style('opacity', 1)
    
    // de-emphasize table row
    d3.selectAll(".estimands-table tr")
      .style('font-weight', null)
      .style('background-color', null)

    d3.selectAll('.estimands-showOnHover')
      .style('display', 'none')
  }



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
      .attr('class', 'estimands-showOnHover')
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
      .attr('class', 'estimands-line-dashed estimands-showOnHover')
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
      .attr('class', 'estimands-showOnHover')

  // add endpoints to ICE lines
  container.append('g')
    .selectAll('estimands-endCircles')
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
      .attr('class', 'estimands-endCircle estimands-showOnHover')
      .attr('pairID', d => d.pair_id)
  container.append('g')
    .selectAll('estimands-endCircle')
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
      .attr('class', 'estimands-endCircle estimands-showOnHover')
      .attr('pairID', d => d.pair_id)

  // add ICE label
  // container.append('g')
  //   .selectAll('rect')
  //   .data(data.line)
  //   .enter()
  //   .append('rect')
  //     .attr('width', 125)
  //     .attr('height', 22)
  //     .attr('x', xScale(meanX * 0.70))
  //     .attr('y', d => yScale(((+d.yName_y0 + +d.yName_y1) / 2) + 2))
  //     .style('fill', '#fff')
  //     .style('display', 'none')
  //     .attr('pairID', d => d.pair_id)
  //     .attr('class', 'estimands-ICElabel estimands-showOnHover')
  container.append('g')
    .selectAll('text')
    .data(data.line)
    .enter()
    .append('text')
      .attr('x', function(d){
        let diff = d.yName_y1 - d.yName_y0
        let position = (diff > 0) ? xScale(meanX * 0.3) : xScale(meanX * 1.05);
        return position;
      })
      .attr('y', d => yScale((+d.yName_y0 + +d.yName_y1) / 2))
      .text(function(d) {
        let diff = d.yName_y1 - d.yName_y0
        let label = "Runner " + d.pair_id + " ICE: " + estimands.roundNumber(diff, 2)
        return label
      })
      .style('display', 'none')
      .attr('pairID', d => d.pair_id)
      .attr('class', 'estimands-ICElabel estimands-showOnHover')



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
      .on('mouseover', estimands.mouseover)
      .on('mousemove', estimands.mousemove)
      .on('mouseleave', estimands.mouseleave)
      .attr('class', 'estimands-scatter estimands-scatterPoints')
      .attr('factual', function(d) {
        if (d.factual === '1') return 'factual'
        return 'counterfactual'
      })
      .attr('treatment', function(d) {
        if (d.treatment === '0') return 'control'
        return 'treatment'
      })
      .attr('pairID', d => d.pair_id)
      .attr("pointer-events", "none")
  // hide counterfactual points
  d3.selectAll(selector + " .estimands-scatterPoints[factual='counterfactual']")
    .style('display', 'none')


    
  // show mean lines - ATE
  let meanYy0 = d3.mean(data.line, d => +d.yName_y0);
  let meanYy1 = d3.mean(data.line, d => +d.yName_y1);
  estimands.data.DoMATE = meanYy1 - meanYy0
  estimands.addMeanLines(container, meanYy0, meanYy1, 'ATE')

  // show mean lines - ATT
  let dataLineATT = data.line.filter(d => d.treatment == "1")
  let meanYy0ATT = d3.mean(dataLineATT, d => +d.yName_y0);
  let meanYy1ATT = d3.mean(dataLineATT, d => +d.yName_y1);
  estimands.addMeanLines(container, meanYy0ATT, meanYy1ATT, 'ATT')

  // show mean lines - ATC
  let dataLineATC = data.line.filter(d => d.treatment == "0")
  let meanYy0ATC = d3.mean(dataLineATC, d => +d.yName_y0);
  let meanYy1ATC = d3.mean(dataLineATC, d => +d.yName_y1);
  estimands.addMeanLines(container, meanYy0ATC, meanYy1ATC, 'ATC')

  

  // add ICE ATE line and label
  estimands.ATE = d3.mean(data.line, d => d.yName_y1 - d.yName_y0)
  estimands.bottomPlotOffset = 1
  container.append('g')
    .append('line')
      .attr('x1', xScale(-0.1))
      .attr('y1', yScaleBottomPlot(estimands.ATE))
      .attr('x2', xScale(1.1))
      .attr('y2', yScaleBottomPlot(estimands.ATE))
      .style('stroke', "#333333")
      .style('stroke-width', 4)
      .style('display', 'none')
      .attr('class', 'estimands-ICEATEline')
  container.append('g')
    .append('text')
      .style('display', 'none')
      .attr('class', 'estimands-ICEATElabel')
      .attr('x', xScale(0.95))
      .attr('y', yScaleBottomPlot(estimands.ATE + 0.5))
      .text('ATE: ' + estimands.roundNumber(estimands.ATE, 2))



  // add title and subtitle
  container
    .append('text')
    .attr('class', 'estimands-title')
    .attr('x', 0)
    .attr('y', -35)
    .text('Calculated average running times')
  container
    .append('text')
    .attr('x', 0)
    .attr('y', -15)
    .attr('class', 'estimands-subtitle')
    .text('Lower is better')

  // add legend
  let legend = container
    .append('g')
    .attr("class", "estimands-legend")
    .attr("transform",
            "translate(" + -bodyWidth*1/2.3 + " ," + (0 + (bodyHeight * 1.17)) + ")")
  legend.append("circle")
    .attr("cx", width*0.34)
    .attr("cy", -5)
    .attr("r", 5)
    .style("fill", strokeScale('0'))
  legend.append("circle")
    .attr("cx", width*0.62)
    .attr("cy", -5)
    .attr("r", 5)
    .style("fill", strokeScale('1'))
  legend.append("text")
    .attr("x", width*0.36)
    .attr("y", 0)
    .text("Standard shoes (y0)")
    .attr("alignment-baseline","middle")
    .attr('treatment', '0')
  legend.append("text")
    .attr("x", width*0.64)
    .attr("y", 0)
    .text("HyperShoes (y1)")
    .attr("alignment-baseline","middle")
    .attr('treatment', '1')
  legend.append("circle")
      .attr("cx", width*0.89)
      .attr("cy", -5)
      .attr("r", 5)
      .style("fill", '#fff')
      .style("stroke", 'grey')
  legend.append("circle")
    .attr("cx", width*1.11)
    .attr("cy", -5)
    .attr("r", 5)
    .style("fill", 'grey')
  legend.append("text")
    .attr("x", width*0.91)
    .attr("y", 0)
    .text("Counterfactual")
    .attr("alignment-baseline","middle")
    .attr('treatment', '0')
  legend.append("text")
    .attr("x", width*1.13)
    .attr("y", 0)
    .text("Factual")
    .attr("alignment-baseline","middle")
    .attr('treatment', '1')
}

estimands.buildPlot = function(data, selector){
  let config = estimands.getConfig(selector)
  let scales = estimands.getScales(data, config)
  estimands.drawData(data, config, scales)
  estimands.changeRunnerText(1)
}

estimands.resetPlot = function(){
  d3.select('#estimands-plot-ATE svg').remove()
  estimands.buildPlot(estimands.data, "#estimands-plot-ATE")
  estimands.changeRunnerText(1)
}

// add ATT plot
estimands.plotATT = function(data, selector){
  let config = estimands.getConfig(selector)
  let scales = estimands.getScales(data, config)
  estimands.drawData(data, config, scales)
  estimands.buildTable(data.line, selector, 'estimands-table-ATT')

  // delay to ensure other function selectors are not modifying this plot on init
  let delay = 2000

  // add meanLinesATT
  let meanLinesSelector = selector + ' .estimands-meanLinesATT, ' + selector + ' .estimands-meanLinesATTConnector, ' + selector + ' .estimands-meanLinesATTConnectorLabel'
  d3.selectAll(meanLinesSelector)
    .style('display', null)
    .style('opacity', 0.9)


  // add mouseover
  d3.selectAll(selector + ' .estimands-scatterPoints')
    .attr("pointer-events", null)
    .style('display', null)
    .style('opacity', 0.8)

  // remove control points
  d3.selectAll(selector + ' .estimands-scatterPoints[treatment="control"], ' + selector + ' .estimands-showOnHover[treatment="control"]')
    .remove()

  // remove control rows
  d3.selectAll(selector + ' tr[treatment="control"] > td')
    .remove()
    // .style('filter', 'opacity(0.2)')
    // .attr("pointer-events", 'none')
}

// add ATC plot
estimands.plotATC = function(data, selector){
  let config = estimands.getConfig(selector)
  let scales = estimands.getScales(data, config)
  estimands.drawData(data, config, scales)
  estimands.buildTable(data.line, selector, 'estimands-table-ATC')

  // add meanLinesATC
  let meanLinesSelector = selector + ' .estimands-meanLinesATC, ' + selector + ' .estimands-meanLinesATCConnector, ' + selector + ' .estimands-meanLinesATCConnectorLabel'
  d3.selectAll(meanLinesSelector)
    .style('display', null)

  // add mouseover
  d3.selectAll(selector + ' .estimands-scatterPoints')
    .attr("pointer-events", null)
    .style('display', null)
    .style('opacity', 0.9)
  
  // remove mouseover from table
  // nullFn = function(){}
  // d3.selectAll(selector + 'tr')
  //   .on('mouseover', null)
  //   .on('mouseleave', null)

  // remove control points
  d3.selectAll(selector + ' .estimands-scatterPoints[treatment="treatment"], ' + selector + ' .estimands-showOnHover[treatment="treatment"]')
    .remove()

  // remove treatment rows
  d3.selectAll(selector + ' tr[treatment="treatment"]')
    .remove()
    // .style('filter', 'opacity(0.2)')
    // .attr("pointer-events", 'none')
}
