// scrollytell
// these animation states are kind of a mess
// TODO: would a state machine work? https://bl.ocks.org/bricof/aff127297d7453ef18459cf52050ed6d
estimands.scrollytellState1 = function(){
    console.log('Estimands Scrollytell State 1')

    // emphasize text and redraw plot
    estimands.emphasizeText("#estimands-trigger-1, #estimands-trigger-1 + p")
    estimands.resetPlot()
    
    // show only factual scatter points
    d3.selectAll("#estimands-plot-ATE .scatterPoints")
        .attr("pointer-events", "none")
        .style('opacity', 0.8)
    d3.selectAll("#estimands-plot-ATE .scatterPoints[factual='counterfactual']")
        .style('display', 'none')
        .style('opacity', 0)
    d3.selectAll("#estimands-plot-ATE .showOnHover")
        .style('display', 'none')
}

estimands.scrollytellState2 = function(){
    console.log('Estimands Scrollytell State 2')

    // emphasize text and redraw plot
    estimands.emphasizeText("#estimands-trigger-2, #estimands-trigger-2 + p")
    estimands.resetPlot()

    // highlight counterfactual points
    d3.selectAll("#estimands-plot-ATE .scatterPoints[factual='factual']")
        .style('display', null)
        .transition()
        .duration(1200)
        .style('opacity', 0.2)
    d3.selectAll("#estimands-plot-ATE .scatterPoints[factual='counterfactual']")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1200)
        .style('opacity', 0.8)
    d3.selectAll("#estimands-plot-ATE .scatterPoints")
        .attr("pointer-events", "none")
    d3.selectAll("#estimands-plot-ATE .showOnHover")
        .style('display', 'none')
}

estimands.scrollytellState3 = function(){
    console.log('Estimands Scrollytell State 3')

    // emphasize text and redraw plot
    estimands.emphasizeText("#estimands-trigger-3, #estimands-trigger-3 + p, #estimands-trigger-3 + p + p")
    estimands.resetPlot()

    // reset scatter points to match previous state
    d3.selectAll("#estimands-plot-ATE .scatterPoints[factual='factual']")
        .style('display', null)
        .style('opacity', 0.2)
        .attr("pointer-events", "all")
    d3.selectAll("#estimands-plot-ATE .scatterPoints[factual='counterfactual']")
        .style('display', null)
        .transition("fade-out")
        .duration(1200)
        .style('opacity', 0.2)
        .attr("pointer-events", "all")

    // show example lines
    d3.selectAll("#estimands-plot-ATE .showOnHover[pairID='" + 1 + "']")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1200)
        .delay(300)
        .style('opacity', 1)
    d3.selectAll("#estimands-plot-ATE .scatterPoints[pairID='" + 1 + "']")
        .transition()
        .duration(1200)
        .delay(300)
        .style('opacity', 1)
}

estimands.scrollytellState4 = function(){
    console.log('Estimands Scrollytell State 4')

    let {xScale, yScale} = estimands.scales;

    // emphasize text and redraw plot
    estimands.emphasizeText("#estimands-trigger-4, #estimands-trigger-4 + p")
    estimands.resetPlot()

    // extend viewbox so there is space for new plot
    let newHeight = 540
    d3.selectAll("#estimands-plot-ATE > svg")
        .attr('viewBox', '0 0 ' + newHeight + ' ' + estimands.config.heightTall)

    // show all points
    d3.selectAll("#estimands-plot-ATE .scatterPoints")
        .style('display', null)
        .style('opacity', 0.2)

    // duplicate the dashed lines for animation
    // estimands.clone('.line-dashed').attr('class', 'line-dashed-keep')

    // animations
    // highlight each ICE
    delayFn = function(index){ return (((index**0.001)-1) * 5000000) - 1000 } // accelerating curve
    d3.selectAll('#estimands-plot-ATE .showOnHover')
        .style('opacity', 0)
        .style('display', null)
    d3.selectAll("#estimands-plot-ATE .showOnHover, #estimands-plot-ATE .scatterPoints") 
        .transition()
        .duration(300)
        .delay(d => delayFn(d.pair_id))
        .style('opacity', 1)
    // unhighlight each ICE slightly later
    d3.selectAll("#estimands-plot-ATE .showOnHover, #estimands-plot-ATE .scatterPoints")
        .transition()
        .duration(300)
        .delay(d => delayFn(+d.pair_id+1 * 0.7))
        .style('opacity', 0)

    // make the ICE bars fall
    estimands.dropICE()
    d3.selectAll('#estimands-plot-ATE .line-dashed')
        .transition()
        .duration(2500)
        .attr('x1', d => xScale(d.drop_x1))
        .attr('y1', d => yScale(d.drop_y1))
        .attr('x2', d => xScale(d.drop_x2))
        .attr('y2', d => yScale((d.drop_y2)))
        .delay(d => delayFn(+d.pair_id+1 * 0.7))
    d3.selectAll('#estimands-plot-ATE .line-dashed')
        .transition('disappear')
        .duration(750)
        .delay(delayFn(11) + 5000)
        .style('opacity', 0)

    // add points
    d3.select("#estimands-plot-ATE > svg > g")
        .append('g')
        .selectAll('droppedPoints')
        .data(estimands.data.line)
        .enter()
        .append("circle")
          .attr("cx", d => xScale(d.drop_x2))
          .attr("cy", d => yScale(d.drop_y2))
          .attr("r", 5)
          .style('fill', "#6e6e6e")
          .style('stroke', 'white')
          .style('stroke-width', 1)
          .style('radius', 7 * 0.8)
          .attr('class', 'droppedPoints')
          .attr('pairID', d => d.pair_id)
          .style('opacity', 0)
          .on('mouseover', estimands.mouseover)
          .on('mousemove', estimands.mousemove)
          .on('mouseleave', estimands.mouseleave)
          .attr('pointer-events', 'none')
          .transition()
          .duration(750)
          .delay(delayFn(11) + 3500)
          .style('opacity', 1)
          .attr('pointer-events', null)

    // remove x label and legend
    d3.selectAll("#estimands-plot-ATE .xAxis text, #estimands-plot-ATE .xAxis line, #estimands-plot-ATE .legend")
        .transition()
        .duration(1000)
        .style('opacity', 0)
        .delay(2000)

    // add new x axis
    let axisDelay = delayFn(1) + 4000
    estimands.clone('#estimands-plot-ATE .xAxis')
        .attr('class', 'axis xAxisBottom')
        .style('opacity', 0)
        .transition()
        .duration(1000)
        .delay(axisDelay)
        .style('opacity', null)
        .attr("transform", "translate(0," + (estimands.config.bodyHeight + 65) + ")") // not sure why 65 works
    d3.selectAll('#estimands-plot-ATE .xAxisBottom text, #estimands-plot-ATE .xAxisBottom line').remove()

    // add new y axis
    d3.select('#estimands-plot-ATE .yAxisBottom')
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1000)
        .delay(axisDelay)
        .style('opacity', null)
    estimands.clone('#estimands-plot-ATE .yAxisLabel')
        .attr('class', 'axisLabel YAxisLabelBottom')
        .attr('x', -400) // not sure why -400 works
        .text("Difference in running time")
        .style('opacity', 0)
        .transition()
        .duration(1000)
        .delay(axisDelay)
        .style('opacity', null)

    // add average line and label
    d3.selectAll('#estimands-plot-ATE .ICEATEline, #estimands-plot-ATE .ICEATElabel')
        .style('opacity', 0)
        .transition()
        .duration(1000)
        .style('display', null)
        .style('opacity', 1)
        .delay(delayFn(11) + 6500)
    
    // add back points
    d3.selectAll("#estimands-plot-ATE .scatterPoints")
        .transition()
        .duration(1000)
        .style('display', null)
        .style('opacity', 0.8)
        .delay(delayFn(11) + 8000)
        .attr("pointer-events", "all")
    
    // move ICE bars back up
    d3.selectAll("#estimands-plot-ATE .line-dashed")
        .transition()
        .duration(1000)
        .delay(delayFn(11) + 8000)
        .attr('x1', d => xScale(estimands.data.meanX))
        .attr('y1', d => yScale(d.yName_y0))
        .attr('x2', d => xScale(estimands.data.meanX))
        .attr('y2', d => yScale((d.yName_y1)))
}

estimands.scrollytellState5 = function(){
    console.log('Estimands Scrollytell State 5')

    let {xScale, yScale} = estimands.scales;

    // emphasize text and redraw plot
    estimands.emphasizeText("#estimands-trigger-5, #estimands-trigger-5 + p")
    estimands.resetPlot()

    // remove table if exists
    d3.selectAll("#estimands-table-ATE").remove()

    // show all points
    d3.selectAll("#estimands-plot-ATE .scatterPoints")
        .style('display', null)
        .style('opacity', 0.2)

    // move MoD ATE label to bottom left
    d3.selectAll('#estimands-plot-ATE .ICEATElabel')
        .style('display', null)
        .style('opacity', 1)
        .transition()
        .duration(1000)
        .attr('x', xScale(0))
        .attr('y', yScale(0.5))
        .delay(500)
        .text('MoD ATE: ' + estimands.roundNumber(estimands.ATE, 2))

    // show mean lines and difference lines
    d3.selectAll("#estimands-plot-ATE .meanLinesATE")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1000)
        .delay(2500)
        .style('opacity', 1)
    d3.selectAll("#estimands-plot-ATE .meanLinesATEConnector, #estimands-plot-ATE .meanLinesATELabel")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1000)
        .delay(4000)
        .style('opacity', 1)
    
    // move DoM ATE label to bottom left
    d3.selectAll('#estimands-plot-ATE .meanLinesATELabel')
        .transition('move')
        .duration(1000)
        .attr('x', xScale(0.35))
        .attr('y', yScale(0.5))
        .text('= DoM ATE: ' + estimands.roundNumber(estimands.data.DoMATE, 2))
        .delay(7000)
    d3.selectAll('#estimands-plot-ATE .meanLinesATEConnector.label.background')
        .transition()
        .delay(7000)
        .style('display', 'none')
}

estimands.scrollytellState6 = function(){
    console.log('Estimands Scrollytell State 6')

    // emphasize text and redraw plot
    estimands.emphasizeText("#estimands-trigger-6, #estimands-trigger-6 + p")
    estimands.resetPlot()

    // show all points
    d3.selectAll("#estimands-plot-ATE .scatterPoints")
        .style('display', null)
        .style('opacity', 0.2)
        .attr("pointer-events", "all")

    // show example lines
    let initialHighlightedPoint = 1
    d3.selectAll("#estimands-plot-ATE .showOnHover[pairID='" + initialHighlightedPoint + "'], #estimands-plot-ATE .scatterPoints[pairID='" + initialHighlightedPoint + "']")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1400)
        .delay(100)
        .style('opacity', 1)

    // add table
    estimands.buildTable(estimands.data.line, '#estimands-plot-ATE', 'estimands-table-ATE')
    d3.selectAll('#estimands-table-ATE')
        .style('opacity', 0)
        .transition()
        .duration(1400)
        .delay(1500)
        .style('opacity', 1)

    // emphasize starting table row
    d3.selectAll("#estimands-table-ATE tr[pairID='" + initialHighlightedPoint + "']")
        .transition()
        .duration(1400)
        .delay(1100)
        .style('font-weight', 700)
        .style('background-color', '#ebebeb')
}

estimands.plotState = 1
estimands.triggerScrollytellAnimation = function(){
    // trigger the closest animation

    // get the positions of divs relative to the top of the viewport
    let trigger1Pos = $('#estimands-trigger-1')[0].getBoundingClientRect().top
    let trigger2Pos = $('#estimands-trigger-2')[0].getBoundingClientRect().top
    let trigger3Pos = $('#estimands-trigger-3')[0].getBoundingClientRect().top
    let trigger4Pos = $('#estimands-trigger-4')[0].getBoundingClientRect().top
    let trigger5Pos = $('#estimands-trigger-5')[0].getBoundingClientRect().top
    let trigger6Pos = $('#estimands-trigger-6')[0].getBoundingClientRect().top
    let positions = [trigger1Pos, trigger2Pos, trigger3Pos, trigger4Pos, trigger5Pos, trigger6Pos]
    let scrollyFns = [estimands.scrollytellState1, estimands.scrollytellState2, estimands.scrollytellState3, estimands.scrollytellState4, estimands.scrollytellState5, estimands.scrollytellState6]

    // make off page elements positive
    positions = positions.map(Math.abs)

    // get smallest value
    const minVal = Math.min(...positions)
    const index = positions.indexOf(minVal)

    // update plot if state changed
    if (index != estimands.plotState){
        scrollyFns[index]()
        estimands.plotState = index
    }
}

// add listener to trigger the animations on scroll
document.addEventListener('scroll', estimands.triggerScrollytellAnimation);
