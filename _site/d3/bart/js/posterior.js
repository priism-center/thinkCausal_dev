
bart.posterior.drawPlot = function(data, scales, config){
    let { container, selector } = config;
    const { xScale, yScale } = scales
  
    // draw base plot
    bart.drawPlot(data, scales, config);
  
    // remove fitted lines besides BART
    let selectors = ".bart-lines-bartFit0, .bart-lines-bartFit1"
    // container.selectAll(`.bart-lines-group > :not(${selectors})`).remove()
    container.selectAll(selectors).style('display', null)

    // de-emphasize points
    container.selectAll('.bart-observations').transition().style('opacity', 0.2)

    // add title
    container.select('.bart-title').text('BART response surface')

    // add subtitle
    container.select('.bart-subtitle').text('Bayesian Additive Regression Trees (BART)')
}

bart.posterior.triggerAnimation = function(){
    const { container } = bart.posterior.config

    // replace button
    let newButton = $('<button id="bart-posterior-reset" onclick="bart.posterior.reset()">Hide posterior</button>')
    $('#bart-trigger-posterior').after(newButton)
    $('#bart-trigger-posterior').remove()
    
    // show posterior lines
    container.selectAll('.bart-distribution-line')
        .style('opacity', 0)
        .style('display', null)
        .transition()
        .duration(1000)
        .style('opacity', 0.6)
    
    // hide bart lines
    // container.selectAll('.bart-lines-bartFit0, .bart-lines-bartFit1')
    //     .transition()
    //     .duration(1000)
    //     .delay(2000)
    //     .style('opacity', 0)

    // show true fit
    // container.selectAll('.bart-lines-trueFit0, .bart-lines-trueFit1')
    //     .style('opacity', 0)
    //     .style('display', null)
    //     .transition()
    //     .duration(1000)
    //     .delay(3000)
    //     .style('opacity', 1)

    // adjust title
    container.select('.bart-title').text('BART posterior')
}

bart.posterior.reset = function(){
    const data = bart.data
    const { container } = bart.posterior.config

    // replace button
    let newButton = $('<button id="bart-trigger-posterior" onclick="bart.posterior.triggerAnimation()">Show posterior</button>')
    $('#bart-posterior-reset').after(newButton)
    $('#bart-posterior-reset').remove()

    // hide posterior lines
    container.selectAll('.bart-distribution-line') //, .bart-lines-trueFit0, .bart-lines-trueFit1')
        .transition()
        .duration(1000)
        .style('opacity', 0)
    
    // show bart lines
    container.selectAll('.bart-lines-bartFit0, .bart-lines-bartFit1')
        .transition()
        .duration(1000)
        .style('opacity', 1)

    // adjust title
    container.select('.bart-title').text('BART response surface')
  
    // // remove existing
    // d3.select('#bart-plot-posterior > svg').remove()
  
    // // build posterior plot
    // const configPosterior = bart.getConfig("#bart-plot-posterior");
    // bart.posterior.config = configPosterior;
    // const scalesPosterior = bart.getScales(data, configPosterior);
    // bart.posterior.scales = scalesPosterior;
    // bart.posterior.drawPlot(data, scalesPosterior, configPosterior);
}

// add posterior credible intervals
bart.posterior.addLines = function(container, data, scales){
    const { xScale, yScale, colorScale } = scales

    // add y0
    container.append('g')
        .selectAll("bart-distribution-line-90")
        .data(data.credibleIntervalsY0)
        .enter()
        .append('line')
        .attr('y1', d => yScale(+d.q_250))
        .attr('y2', d => yScale(+d.q_975))
        .attr('x1', d => xScale(+d.caloriesConsumed))
        .attr('x2', d => xScale(+d.caloriesConsumed))
        // .attr('class', 'bart-animated-plot')
        .style('stroke', colorScale('0'))
        .style('stroke-width', '1px')
        .attr('class', 'bart-distribution-line bart-distribution-line-90')
        .style('display', 'none')
    container.append('g')
        .selectAll("bart-distribution-line-80")
        .data(data.credibleIntervalsY0)
        .enter()
        .append('line')
        .attr('y1', d => yScale(+d.q_10))
        .attr('y2', d => yScale(+d.q_90))
        .attr('x1', d => xScale(+d.caloriesConsumed))
        .attr('x2', d => xScale(+d.caloriesConsumed))
        // .attr('class', 'bart-animated-plot')
        .style('stroke', colorScale('0'))
        .style('stroke-width', '3px')
        .attr('class', 'bart-distribution-line bart-distribution-line-80')
        .style('display', 'none')
    
    // add y1
    container.append('g')
        .selectAll("bart-distribution-line-90")
        .data(data.credibleIntervalsY1)
        .enter()
        .append('line')
        .attr('y1', d => yScale(+d.q_250))
        .attr('y2', d => yScale(+d.q_975))
        .attr('x1', d => xScale(+d.caloriesConsumed))
        .attr('x2', d => xScale(+d.caloriesConsumed))
        // .attr('class', 'bart-animated-plot')
        .style('stroke', colorScale('1'))
        .style('stroke-width', '1px')
        .attr('class', 'bart-distribution-line bart-distribution-line-90')
        .style('display', 'none')
    container.append('g')
        .selectAll("bart-distribution-line-80")
        .data(data.credibleIntervalsY1)
        .enter()
        .append('line')
        .attr('y1', d => yScale(+d.q_10))
        .attr('y2', d => yScale(+d.q_90))
        .attr('x1', d => xScale(+d.caloriesConsumed))
        .attr('x2', d => xScale(+d.caloriesConsumed))
        // .attr('class', 'bart-animated-plot')
        .style('stroke', colorScale('1'))
        .style('stroke-width', '3px')
        .attr('class', 'bart-distribution-line bart-distribution-line-80')
        .style('display', 'none')
}