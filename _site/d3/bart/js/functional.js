// namespace
bart.functional = {}

// draw functional plot
bart.functional.drawPlot = function(data, scales, config){
    const { container, selector } = config;
    const { xScale, yScale } = scales
  
    // draw base plot
    bart.drawPlot(data, scales, config);
  
    // remove lines and treated observations
    container.selectAll(".bart-lines-group, .bart-observations[treatment='1']").remove()

    // add spline
    container.append('path')
        .datum(data.draggablePoints)
        .attr('d', d3.line()
            .curve(d3.curveCardinal)
            .x(d => xScale(d.caloriesConsumed))
            .y(d => yScale(d.runningTime))
        )
        .attr('class', 'bart-draggable-line')

    // add dragable points
    container.append('g').attr('class', 'bart-draggable-group')
        .selectAll('.bart-draggable')
        .data(data.draggablePoints)
        .enter()
        .append("rect")
            .attr("x", d => xScale(d.caloriesConsumed) - 5)
            .attr("y", d => yScale(d.runningTime) - 14)
            .attr('width', 10)
            .attr('height', 28)
            .attr("r", 9)
            .attr('treatment', d => d.z)
            .attr('class', 'bart-draggable')
            .call(bart.functional.dragHandler);
}

// handler to execute on drag event
bart.functional.dragHandler = d3.drag()
    .on('drag', function(event, d){ 
        const xScale = bart.functional.scales.xScale
        const yScale = bart.functional.scales.yScale
        let newX = d3.event.x
        let newY = d3.event.y

        // fix the X value to prevent left-right drag
        // const xRange = xScale.range()
        // newX = this.cx.baseVal.value
        newX = this.x.baseVal.value

        // limit range to plot
        const yRange = yScale.range()
        newY = Math.min(Math.max(newY, yRange[1]), yRange[0])
        
        // update rect position
        d3.select(this)
            .attr('x', newX)
            .attr('y', newY - 14)
        
        // update spline
        d3.select('.bart-draggable-line')
            .datum(bart.functional.movedPoints)
            .attr('d', d3.line()
                .curve(d3.curveCardinal)
                .x(d => xScale(d.caloriesConsumed))
                .y(d => yScale(d.runningTime))
            )
        
        // record position
        let newPoint = {
            runningTime: yScale.invert(newY),
            caloriesConsumed: xScale.invert(newX) + 14 // hotfix for rect moving left on reset
        }
        bart.functional.movedPoints[d] = newPoint
        // console.log(bart.functional.movedPoints)
    })
    .on('start', function(event, d){ d3.select(this).raise().attr('r', 11) })
    .on('end', function(event, d){ d3.select(this).attr('r', 9) })

// generate the new points and pass data to R for model fitting
bart.functional.runModel = function(){
    let newPoints = bart.generateData()
    const { container, selector } = bart.functional.config;
    const { xScale, yScale, colorScale } = bart.functional.scales
    const yRange = yScale.domain()

    // reset button
    let newButton = $('<button id="bart-functional-reset" onclick="bart.functional.reset()">Reset</button>')
    $('#bart-functional-fit').after(newButton)
    $('#bart-functional-fit').remove()

    // add generated points
    container.append('g').attr('class', 'bart-generatedPoints-group')
        .selectAll('.bart-generatedPoints')
        .data(newPoints)
        .enter()
        .append("circle")
            .attr("cx", d => xScale(+d.caloriesConsumed))
            .attr("cy", d => yScale(+d.runningTime))
            .attr("r", 5.5)
            .attr('treatment', d => d.z)
            .style('fill', d => colorScale('1'))
            .style('opacity', 0)
            .attr('class', 'bart-generatedPoints')
            .transition()
            .duration(1000)
            .delay(d => 400 + Math.random() * 400)
            .style('opacity', d => d.runningTime > yRange[0] && d.runningTime < yRange[1] ? 0.8 : 0) 

    // fade out functional form spline
    container.selectAll('.bart-draggable, .bart-draggable-line')
        .transition()
        .duration(500)
        .style('opacity', 0.2)
        .attr('pointer-events', 'none')

    // this should pass bart.functional.movedPoints to R


}

bart.functional.reset = function(){
    const data = bart.data
    const selector = bart.functional.config.selector

    // reset button
    let newButton = $('<button id="bart-functional-fit" onclick="bart.functional.runModel()">Fit model</button>')
    $('#bart-functional-reset').after(newButton)
    $('#bart-functional-reset').remove()

    // remove plot
    d3.select(selector + ' > svg').remove();

    // redraw plot
    const configFunctional = bart.getConfig(selector);
    bart.functional.config = configFunctional;
    const scalesFunctional = bart.getScales(data, configFunctional);
    bart.functional.scales = scalesFunctional;
    bart.functional.drawPlot(data, scalesFunctional, configFunctional);
}
