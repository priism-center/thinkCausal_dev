// scrollytell
// these animation states are kind of a mess
// TODO: would a state machine work? https://bl.ocks.org/bricof/aff127297d7453ef18459cf52050ed6d
estimands.d3State1 = function(){
    console.log('estimandsState1')

    // trigger plot change
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", "none")
        .transition() //this kills currently running transitions
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

    let {xScale, yScale, colorScale} = estimands.getScales(estimands.data, estimands.getConfig());
    let meanX = d3.mean(estimands.data.scatter, d => +d.xName);


    // resets
    d3.selectAll(".xAxis text, .xAxis line")
      .transition() //this kills currently running transitions
      .style('opacity', 1)
    d3.selectAll(".meanLines")
        .style('display', 'none')
    d3.selectAll('.showOnHover')
        .style('display', 'none')
        .style('opacity', 0.8)
        .transition() //this kills currently running transitions
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

    // trigger plot change
    d3.selectAll(".scatterPoints")
        .style('display', null)
        .transition()
        .duration(1200)
        .style('opacity', 0.2)
        .attr("pointer-events", "all")

    // show example lines
    d3.selectAll(".showOnHover[pairID='" + 10 + "'], .scatterPoints[pairID='" + 10 + "']")
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

    let {xScale, yScale, colorScale} = estimands.getScales(estimands.data, estimands.getConfig());
    let meanX = d3.mean(estimands.data.scatter, d => +d.xName);

    // resets
    d3.selectAll(".showOnHover")
        .style('display', 'none')
        .transition() //this kills currently running transitions
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", "none")
        .style('opacity', 0.2)
        .transition() //this kills currently running transitions
    d3.selectAll(".meanLines")
        .style('display', 'none')
        .transition() //this kills currently running transitions

    // animations
    // highlight each ICE
    delayFn = function(x){ return ((x**0.001)-1) * 5000000 } // accelerating curve
    d3.selectAll('.showOnHover')
        .transition()
        .style('display', null)
        .style('opacity', 0)
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

    // remove x label
    d3.selectAll(".xAxis text, .xAxis line")
      .transition()
      .duration(1000)
      .style('opacity', 0)
      .delay(2000)
    
    // add average line and label TODO
    d3.selectAll('.ICEATEline')
        .style('opacity', 0)
        .transition()
        .duration(1000)
        .style('display', null)
        .style('opacity', 1)
        .delay(delayFn(11) + 3000)
    

    estimands.emphasizeText("#estimands-trigger-4, #estimands-trigger-4 + p")
}

estimands.d3State5 = function(){
    console.log('estimandsState5')

    let {xScale, yScale, colorScale} = estimands.getScales(estimands.data, estimands.getConfig());
    let meanX = d3.mean(estimands.data.scatter, d => +d.xName);

    // resets
    d3.selectAll(".xAxis text, .xAxis line")
      .transition() //this kills currently running transitions
      .style('opacity', 1)
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", "none")
        .transition()
        .style('opacity', 0.2)
        .style('display', null)
    d3.selectAll(".showOnHover")
        .style('display', 'none')
    d3.selectAll('.ICEATEline')
        .transition() //this kills currently running transitions
        .style('display', 'none')
    d3.selectAll('.line-dashed')
        .transition()
        .style('stroke-dasharray', null)
        .attr('x1', d => xScale(meanX))
        .attr('y1', d => yScale(d.yName_y0))
        .attr('x2', d => xScale(meanX))
        .attr('y2', d => yScale((d.yName_y1)))

    // show mean lines
    d3.selectAll(".meanLines")
        .style('display', null)

    estimands.emphasizeText("#estimands-trigger-5, #estimands-trigger-5 + p")
}

estimands.d3State6 = function(){
    console.log('estimandsState6')

    // // trigger plot change
    // d3.selectAll(".meanLines")
    //     .style('display', null)
    // d3.selectAll(".scatterPoints")
    //     .attr("pointer-events", "none")
    //     .transition() //this kills currently running transitions
    //     .style('opacity', 0.2)
    // d3.selectAll(".showOnHover")
    //     .style('display', 'none')

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
