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
        const xRange = xScale.range()
        // newX = this.cx.baseVal.value
        newX = this.x.baseVal.value // uncomment to let left-right drag

        // limit range to plot
        const yRange = yScale.range()
        newY = bart.clamp(newY, yRange[1], yRange[0])
        newX = bart.clamp(newX, xRange[0], xRange[1])
        
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

    // update tab buttons
    bart.functional.showModel('none')
    d3.selectAll('.bart-tab-button').attr('disabled', null)

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

    // add the control group to the data
    let controlData = bart.data.observations.filter(d => d.z == '0')
    let modelData = newPoints.concat(controlData)

    // calculate diff means
    let meanY0 = d3.mean(modelData.filter(d => d.z == '0'), d => d.runningTime)
    let meanY1 = d3.mean(modelData.filter(d => d.z == '1'), d => d.runningTime)
    modelData.map(d => d.diffFit0 = meanY0)
    modelData.map(d => d.diffFit1 = meanY1)

    // add mean diff and lm fit lines
    bart.functional.addLines(modelData, 'diffFit0', 'diffFit1')
    // bart.functional.addLines(modelData, 'lmFit0', 'lmFit1')

    // this should pass bart.functional.movedPoints to R

    // TODO: how to add BART lines? async?
}

// reset the functional plot
bart.functional.reset = function(){
    const data = bart.data
    const selector = bart.functional.config.selector

    // reset button
    let newButton = $('<button id="bart-functional-fit" onclick="bart.functional.runModel()">Generate</button>')
    $('#bart-functional-reset').after(newButton)
    $('#bart-functional-reset').remove()

    // disable model buttons
    d3.selectAll('.bart-tab-button').attr('disabled', true)

    // remove plot
    d3.select(selector + ' > svg').remove();

    // redraw plot
    const configFunctional = bart.getConfig(selector);
    bart.functional.config = configFunctional;
    const scalesFunctional = bart.getScales(data, configFunctional);
    bart.functional.scales = scalesFunctional;
    bart.functional.drawPlot(data, scalesFunctional, configFunctional);

    // update tab buttons
    bart.functional.showModel('none')
}

// update plot on button click
bart.functional.showModel = function(model){
    // model must be one of ['none', 'diff', 'lm', 'bart']
    let activeButtonStyle = 'border-top: #726078 solid 3px; color: #3f3f3f;'
    let selector = '#bart-functional-tab-' + model
    let container = bart.functional.config.container

    // highlight button
    d3.selectAll('.bart-tab-button').attr('style', null)
    d3.select(selector).attr('style', activeButtonStyle)

    // fade observations
    container.selectAll('.bart-generatedPoints, .bart-observations')
        .transition()
        .style('opacity', 0.2)

    // show model on plot
    if (model == 'none'){
        // update subtitle
        container.select('.bart-subtitle').text(null)

        // hide lines
        container.selectAll('.bart-lines').style('display', 'none');

        // bring back observations
        container.selectAll('.bart-generatedPoints, .bart-observations')
            .transition()
            .style('opacity', 0.8)

    } else if (model == 'diff') {
        // update subtitle
        container.select('.bart-subtitle').text('Difference in means')

        // show lines
        container.selectAll('.bart-lines').style('display', 'none');
        container.selectAll('.bart-lines-diffFit0, .bart-lines-diffFit1')
            .style('display', null);

    } else if (model == 'lm'){
        // update subtitle
        container.select('.bart-subtitle').text('Linear regression')

        // show lines
        container.selectAll('.bart-lines').style('display', 'none');
        container.selectAll('.bart-lines-lmFit0, .bart-lines-lmFit1')
            .style('display', null);

    } else if (model == 'bart'){
        // update subtitle
        container.select('.bart-subtitle').text('Bayesian Additive Regression Trees (BART)')

        // show lines
        container.selectAll('.bart-lines').style('display', 'none');
        container.selectAll('.bart-lines-bartFit0, .bart-lines-bartFit1')
            .style('display', null);

    }
}

// bart.functional.modelData = function(data){
    
//     // difference in means
//     let meanY0 = d3.mean(data.filter(d => d.z == '0'), d => d.runningTime)
//     let meanY1 = d3.mean(data.filter(d => d.z == '1'), d => d.runningTime)
//     data.map(d => d.diffFit0 = meanY0)
//     data.map(d => d.diffFit1 = meanY1)

//     // linear regression
//     // https://observablehq.com/@harrystevens/introducing-d3-regression 2D only!!!
//     // https://simplestatistics.org/docs/#linearregressionline
//     // let linearRegression = d3.regressionLinear()
//     //     .x(d => d.caloriesConsumed)
//     //     .y(d => d.runningTime)
//     //     // .domain([-1.7, 16]);
//     // console.log(linearRegression)

//     return data;
// }

// add fitted lines to functional plot
bart.functional.addLines = function(data, y0, y1){
    let container = bart.functional.config.container
    const {xScale, yScale, colorScale} = bart.functional.scales
    const class0 = "bart-lines bart-lines-" + y0
    const class1 = "bart-lines bart-lines-" + y1
  
    // add y0 line
    container.append('path')
      .datum(data)
      .attr('d', d3.line()
        .x(d => xScale(d.caloriesConsumed))
        .y(d => yScale(d[y0]))
      )
      .style("stroke", colorScale('0'))
      .style('fill', 'none')
      .style('display', 'none')
      .attr('class', class0)
  
    // add y1 line
    container.append('path')
      .datum(data)
      .attr('d', d3.line()
        .x(d => xScale(d.caloriesConsumed))
        .y(d => yScale(d[y1]))
      )
      .style("stroke", colorScale('1'))
      .style('fill', 'none')
      .style('display', 'none')
      .attr('class', class1)
  }
