// scrollytell
// these animation states are kind of a mess
// TODO: would a state machine work? https://bl.ocks.org/bricof/aff127297d7453ef18459cf52050ed6d
estimands.d3State1 = function(){
    console.log('estimandsState1')

    estimands.killAnimations()

    // trigger plot change
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", "none")
        // .transition() //this kills currently running transitions
        .style('opacity', 0.8)
    d3.selectAll(".scatterPoints[factual='counterfactual']")
        .style('display', 'none')
        .style('opacity', 0)
    d3.selectAll(".showOnHover")
        .style('display', 'none')

    estimands.emphasizeText("#estimands-trigger-1, #estimands-trigger-1 + p")
}

estimands.d3State2 = function(){
    console.log('estimandsState2')

    estimands.killAnimations()

    // trigger plot change
    d3.selectAll(".scatterPoints[factual='factual']")
        .style('display', null)
        // .style('opacity', 0)
        .transition()
        .duration(1200)
        .style('opacity', 0.2)
    d3.selectAll(".scatterPoints[factual='counterfactual']")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1200)
        .style('opacity', 0.8)
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", "none")
    d3.selectAll(".showOnHover")
        .style('display', 'none')

    estimands.emphasizeText("#estimands-trigger-2, #estimands-trigger-2 + p")
}

estimands.d3State3 = function(){
    console.log('estimandsState3')

    let {xScale, yScale} = estimands.scales;
    let meanX = d3.mean(estimands.data.scatter, d => +d.xName);

    estimands.killAnimations()

    // resets
    d3.selectAll(".xAxis text, .xAxis line, .legend")
    //   .transition() //this kills currently running transitions
      .style('opacity', 1)
    d3.selectAll(".meanLines")
        .style('display', 'none')
    d3.selectAll('.showOnHover')
        .style('display', 'none')
        .style('opacity', 0.8)
        // .transition() //this kills currently running transitions
        // .style('display', 'none')
    d3.selectAll('.line-dashed')
        .transition()
        .style('stroke-dasharray', null)
        .attr('x1', d => xScale(meanX))
        .attr('y1', d => yScale(d.yName_y0))
        .attr('x2', d => xScale(meanX))
        .attr('y2', d => yScale((d.yName_y1)))
    d3.selectAll('.ICEATEline')
        .transition()
        .style('display', 'none')
    d3.selectAll('.yAxisLabel')
        .transition()
        .text("Running time")

    // trigger plot change
    d3.selectAll(".scatterPoints")
        .style('display', null)
        .transition()
        .duration(1200)
        .style('opacity', 0.2)
        .attr("pointer-events", "all")

    // show example lines
    d3.selectAll(".showOnHover[pairID='" + 1 + "'], .scatterPoints[pairID='" + 1 + "']")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1400)
        .delay(1000)
        .style('opacity', 1)

    estimands.emphasizeText("#estimands-trigger-3, #estimands-trigger-3 + p, #estimands-trigger-3 + p + p")
}

estimands.d3State4 = function(){
    console.log('estimandsState4')

    let {xScale, yScale} = estimands.scales;
    let meanX = d3.mean(estimands.data.scatter, d => +d.xName);
    let meanY = d3.mean(estimands.data.scatter, d => +d.yName);

    estimands.killAnimations()

    // resets
    d3.selectAll(".showOnHover")
        .style('display', 'none')
        .transition() //this kills currently running transitions
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", "none")
        .style('opacity', 0.2)
        // .transition() //this kills currently running transitions
    d3.selectAll(".meanLines, .meanLinesConnector")
        .style('display', 'none')
        // .transition() //this kills currently running transitions
    d3.selectAll('.DoMATELabel')
        .style('display', 'none')
        .attr('x', xScale(meanX * 0.75))
        .attr('y', yScale(meanY))

    // animations
    // highlight each ICE
    delayFn = function(index){ return (((index**0.001)-1) * 5000000) - 1000 } // accelerating curve
    d3.selectAll('.showOnHover')
        .style('opacity', 0)
        .style('display', null)
    d3.selectAll(".showOnHover, .scatterPoints") //.scatterPoints"
        .transition()
        .duration(300)
        .delay(d => delayFn(d.pair_id))
        .style('opacity', 1)
    // unhighlight each ICE slightly later
    d3.selectAll(".showOnHover, .scatterPoints")
        .transition()
        .duration(300)
        .delay(d => delayFn(+d.pair_id+1 * 0.7))
        .style('opacity', 0)

    // this fades it away afterwards but should move it
    // d3.selectAll(".showOnHover")
    //     .transition()
    //     // .duration(300)
    //     .delay(d => delayFn(+d.pair_id+1) - 100)
    //     .style('opacity', 0.1)

    // make the ICE bars fall
    // TODO bars should fall to another plot
    estimands.dropICE(estimands.data.line)
    d3.selectAll('.line-dashed')
        .transition()
        .duration(2500)
        .attr('x1', d => xScale(d.drop_x1))
        .attr('y1', d => yScale(d.drop_y1))
        .attr('x2', d => xScale(d.drop_x2))
        .attr('y2', d => yScale((d.drop_y2)))
        .delay(d => delayFn(+d.pair_id+1 * 0.7))
    //   .attr('class', 'dropped-ICE')

    // remove x label and legend
    d3.selectAll(".xAxis text, .xAxis line, .legend")
        .transition()
        .duration(1000)
        .style('opacity', 0)
        .delay(2000)

    // change yaxis label
    d3.selectAll('.yAxisLabel')
        .transition()
        .duration(2500)
        .text("Difference in running time")
        .delay(delayFn(11) + 3000)
    estimands.highlightText(".yAxisLabel", delayFn(11) + 2750)

    // add average line and label
    d3.selectAll('.ICEATEline, .ICEATElabel')
        .style('opacity', 0)
        .attr('x', xScale(0.95))
        .attr('y', yScale(Math.abs(estimands.ATE) * 1.1))
        .text('ATE: ' + Math.round(Math.abs(estimands.ATE)*100) / 100)
        .transition()
        .duration(1000)
        .style('display', null)
        .style('opacity', 1)
        .delay(delayFn(11) + 3000)

    estimands.emphasizeText("#estimands-trigger-4, #estimands-trigger-4 + p")
}

estimands.d3State5 = function(){
    console.log('estimandsState5')

    let {xScale, yScale} = estimands.scales;
    let meanX = d3.mean(estimands.data.scatter, d => +d.xName);

    estimands.killAnimations()

    // resets
    d3.selectAll(".xAxis text, .xAxis line, .legend")
    //   .transition() //this kills currently running transitions
      .style('opacity', 1)
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", "none")
        .transition()
        .style('opacity', 0.2)
        .style('display', null)
    d3.selectAll(".showOnHover")
        .style('display', 'none')
    d3.selectAll('.ICEATEline')
        // .transition() //this kills currently running transitions
        .style('display', 'none')
    d3.selectAll('.line-dashed')
        .transition()
        .style('stroke-dasharray', null)
        .attr('x1', d => xScale(meanX))
        .attr('y1', d => yScale(d.yName_y0))
        .attr('x2', d => xScale(meanX))
        .attr('y2', d => yScale((d.yName_y1)))
    d3.selectAll('.yAxisLabel')
        .transition()
        .text("Running time")
    d3.selectAll('#estimands-plot-ATE > table').remove()

    // move ATE label to bottom left
    d3.selectAll('.ICEATElabel')
        .style('display', null)
        .style('opacity', 1)
        .transition()
        .duration(1000)
        .attr('x', xScale(0))
        .attr('y', yScale(0.5))
        .delay(500)
        .text('MoD ATE: ' + Math.round(estimands.ATE*100) / 100)

    // show mean lines and difference lines
    d3.selectAll(".meanLines, .meanLinesConnector, .DoMATELabel")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1000)
        .delay(1000)
        .style('opacity', 1)
    
    // move ATE label to bottom left
    d3.selectAll('.DoMATELabel')
        // .style('display', null)
        // .style('opacity', 1)
        .transition()
        .duration(1000)
        .delay(2000)
        .attr('x', xScale(0.35))
        .attr('y', yScale(0.5))
        .text('= DoM ATE: ' + Math.round(estimands.data.DoMATE*100) / 100)
        .delay(4000)
    d3.selectAll('.meanLinesConnector.label.background')
        .transition()
        .delay(4000)
        .style('display', 'none')

    estimands.emphasizeText("#estimands-trigger-5, #estimands-trigger-5 + p")
}

estimands.d3State6 = function(){
    console.log('estimandsState6')

    estimands.killAnimations()

    // resets
    d3.selectAll(".meanLines, .meanLinesConnector, .ICEATElabel")
        .style('display', 'none')
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", null)

    // show example lines
    d3.selectAll(".showOnHover[pairID='" + 1 + "'], .scatterPoints[pairID='" + 1 + "']")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1400)
        .delay(100)
        .style('opacity', 1)

    // add table
    estimands.buildTable(estimands.data.line)
    d3.selectAll('#estimands-table')
        .style('opacity', 0)
        .transition()
        .duration(1400)
        .delay(1500)
        .style('opacity', 1)

    // emphasize starting table row
    d3.selectAll("#estimands-table tr[pairID='" + 1 + "']")
        .transition()
        .duration(1400)
        .delay(1100)
        .style('font-weight', 700)
        .style('background-color', '#ebebeb')
    
    estimands.emphasizeText("#estimands-trigger-6, #estimands-trigger-6 + p")
}

estimands.plotState = 1
estimands.triggerD3Animation = function(){
    // trigger the closest animation

    // get the positions of divs relative to the top of the viewport
    let trigger1Pos = $('#estimands-trigger-1')[0].getBoundingClientRect().top
    let trigger2Pos = $('#estimands-trigger-2')[0].getBoundingClientRect().top
    let trigger3Pos = $('#estimands-trigger-3')[0].getBoundingClientRect().top
    let trigger4Pos = $('#estimands-trigger-4')[0].getBoundingClientRect().top
    let trigger5Pos = $('#estimands-trigger-5')[0].getBoundingClientRect().top
    let trigger6Pos = $('#estimands-trigger-6')[0].getBoundingClientRect().top
    let positions = [trigger1Pos, trigger2Pos, trigger3Pos, trigger4Pos, trigger5Pos, trigger6Pos]

    //// for elements that are off the page, replace with really large number
    // for (var i = positions.length-1; i >= 0; i--){
    //     if (positions[i] < 0){
    //         positions.splice(i, 1, Math.abs(positions[i]) * 10000)
    //     }
    // }

    // make off page elements positive
    positions = positions.map(Math.abs)

    // get smallest value
    const minVal = Math.min(...positions)
    const index = positions.indexOf(minVal)

    // update plot if state changed
    if (index != estimands.plotState){
        if (index == 0) estimands.d3State1()
        if (index == 1) estimands.d3State2()
        if (index == 2) estimands.d3State3()
        if (index == 3) estimands.d3State4()
        if (index == 4) estimands.d3State5()
        if (index == 5) estimands.d3State6()
        estimands.plotState = index
    }
}

// add listener
document.addEventListener('scroll', estimands.triggerD3Animation);


//// helpers ////
estimands.emphasizeText = function(selectors){
    d3.selectAll(".estimands-text-along-d3 > p, .estimands-text-along-d3 > h2")
        .style('filter', 'opacity(0.2)')
    // emphasize this text
    d3.selectAll(selectors)
        .style('filter', null)
}
estimands.dropICE = function(data){
    // calculates the end position for each ICE segment
    d3.map(data, function(d) {
        d.drop_x1 = (d.pair_id - 1) / 10
        d.drop_y1 = d.yName_y0 - d.yName_y1 // reverse?
        d.drop_x2 = (d.pair_id - 1) / 10
        d.drop_y2 = 0
    })
}
estimands.highlightText = function(selector, delay){
    // flashes the text color yellow and temporarily enlarges

    let currentFontSize = d3.selectAll(selector).style('font-size')
    currentFontSize = currentFontSize.replace('px', '')
    bigFontSize = (currentFontSize * 1.3) + 'px'
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
    rounded = Math.round(num * Math.pow(10, dec)) / Math.pow(10, dec)
    return rounded.toFixed(dec)
}


// initialize state
estimands.d3State1()
