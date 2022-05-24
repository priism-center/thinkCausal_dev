estimands.emphasizeText = function(selectors){
    d3.selectAll(".estimands-text-along-d3 > p, .estimands-text-along-d3 > h2")
        .style('filter', 'opacity(0.2)')
    // emphasize this text
    d3.selectAll(selectors)
        .style('filter', null)
}

estimands.dropICE = function(){
    // calculates the end position for each ICE segment
    d3.map(estimands.data.line, function(d) {
        d.drop_x1 = (d.pair_id - 1) / 10
        d.drop_y1 = 0 - estimands.bottomPlotOffset
        d.drop_x2 = (d.pair_id - 1) / 10
        d.drop_y2 = (d.yName_y1 - d.yName_y0) - estimands.bottomPlotOffset
    })
}

estimands.highlightText = function(selector, delay){
    // flashes the text color yellow and temporarily enlarges

    let currentFontSize = d3.selectAll(selector).style('font-size')
    currentFontSize = currentFontSize.replace('px', '')
    let bigFontSize = (currentFontSize * 1.3) + 'px'
    currentFontSize = currentFontSize + 'px'

    d3.selectAll(selector)
        .transition('highlightText')
        .duration(500)
        .style("fill", '#f0d000')
        .style('font-size', bigFontSize)
        .delay(delay)

    d3.selectAll(selector)
        .transition('highlightText')
        .duration(500)
        .style("fill", null)
        .style('font-size', currentFontSize)
        .delay(500 + delay)
}

estimands.killAnimations = function(){
    // calling a blank transition again will kill
    // any previous running ones with the same name
    d3.selectAll("#estimands-plot-ATE *")
        .transition()
    d3.selectAll("#estimands-plot-ATE *")
        .transition('highlightText')
}

estimands.roundNumber = function(num, dec){
    // rounds a number to a certain decimal place and always maintains a decimal point
    let rounded = Math.round(num * Math.pow(10, dec)) / Math.pow(10, dec)
    return rounded.toFixed(dec)
}

estimands.clone = function(selector) {
    var node = d3.select(selector).node();
    return d3.select(node.parentNode.insertBefore(node.cloneNode(true), node.nextSibling));
}

estimands.changeRunnerText = function(runner){
    let ICE = estimands.data.line.filter(d => d.pair_id == +runner)
    ICE = estimands.roundNumber(ICE[0].yName_y1 - ICE[0].yName_y0, 2)
    if (runner.toString().length < 2) runner = runner + ' '
    // console.log(runner.length)
    let newText = "Runner " + runner.toString() + " has an ICE of " + ICE
    d3.select("#estimands-runner-text").text(newText)
}

// add mean lines for the ATE, ATT, and ATC
estimands.addMeanLines = function(container, meanYy0, meanYy1, estimandType){
    let {xScale, yScale, colorScale, strokeScale, yScaleBottomPlot} = estimands.scales
    let {strokeColor, strokeWidth, pointOpacity, pointRadius} = estimands.styles
    let meanX = estimands.data.meanX
    let meanY = estimands.data.meanY

    let className = 'estimands-meanLines ' + 'estimands-meanLines' + estimandType
    let classConnector = className + 'Connector'
    let classLabel = className + 'Label'

    // add connecting lines
    container.append('g')
      .append('line')
        .attr('x1', xScale(-0.1))
        .attr('y1', yScale(meanYy0))
        .attr('x2', xScale(meanX))
        .attr('y2', yScale(meanYy0))
        .style('stroke', "#21918c")
        .style('stroke-width', strokeWidth * 2/3)
        .style('display', 'none')
        .attr('class', classConnector)
    container.append('g')
      .append('line')
        .attr('x1', xScale(meanX))
        .attr('y1', yScale(meanYy0))
        .attr('x2', xScale(meanX))
        .attr('y2', yScale(meanYy1))
        .style('stroke', strokeColor)
        .style('stroke-width', strokeWidth * 2/3)
        .style('display', 'none')
        .attr('class', classConnector + ' estimands-mean-dashed')
    container.append('g')
      .append('line')
        .attr('x1', xScale(1.1))
        .attr('y1', yScale(meanYy1))
        .attr('x2', xScale(meanX))
        .attr('y2', yScale(meanYy1))
        .style('stroke', "#440154")
        .style('stroke-width', strokeWidth * 2/3)
        .style('display', 'none')
        .attr('class', classConnector)
    // add end points
    container.append('g')
      .append('circle')
        .attr("cx", xScale(meanX))
        .attr("cy", yScale(meanYy0))
        .attr("r", pointRadius * 0.7)
        .attr('fill', '#21918c')
        .attr('stroke', "#21918c")
        .attr('stroke-width', 2)
        .style('display', 'none')
        .attr('class', classConnector + ' estimands-endCircle')
    container.append('g')
      .append('circle')
        .attr("cx", xScale(meanX))
        .attr("cy", yScale(meanYy1))
        .attr("r", pointRadius * 0.7)
        .attr('fill', '#440154')
        .attr('stroke', "#440154")
        .attr('stroke-width', 2)
        .style('display', 'none')
        .attr('class', classConnector + ' estimands-endCircle')
    // add label
    container.append('g')
      .append('rect')
        .attr('width', 50)
        .attr('height', 27)
        .attr('x', xScale(meanX * 0.97))
        .attr('y', yScale(meanY + 0.2))
        .style('fill', '#fff')
        .style('display', 'none')
        .attr('class', classConnector + ' label background')
    container.append('g')
      .append('text')
        .attr('x', xScale(meanX * 0.75))
        .attr('y', yScale(meanY))
        .text(estimandType + ": " + estimands.roundNumber(meanYy1 - meanYy0, 2))
        .style('display', 'none')
        .attr('class', classConnector + classLabel)
    // add the mean lines
    container.append('g')
      .append('line')
        .attr('x1', xScale(-0.1))
        .attr('y1', yScale(meanYy0))
        .attr('x2', xScale(0.1))
        .attr('y2', yScale(meanYy0))
        .style('stroke', "#333333")
        .style('stroke-width', 5)
        .style('display', 'none')
        .attr('class', className)
    container.append('g')
      .append('line')
        .attr('x1', xScale(0.9))
        .attr('y1', yScale(meanYy1))
        .attr('x2', xScale(1.1))
        .attr('y2', yScale(meanYy1))
        .style('stroke', '#333333')
        .style('stroke-width', 5)
        .style('display', 'none')
        .attr('class', className)
  }
  
