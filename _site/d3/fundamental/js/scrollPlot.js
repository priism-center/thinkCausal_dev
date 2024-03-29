// scrollytell
// these animation states are kind of a mess
// TODO: would a state machine work? https://bl.ocks.org/bricof/aff127297d7453ef18459cf52050ed6d
fundamental.scrollytellState1 = function(){
    console.log('fundamentalState1')

    fundamental.killAnimations()

    // resets
    d3.selectAll(".fundamental-studyLine, .fundamental-rugLines")
        .style('display', 'none')
    // d3.selectAll(".trueMeanLineLabel")
    //     .style('display', null)
    fundamental.changeStudyText(true)
    clearTimeout(fundamental.timeoutStudyText)

    // remove trueMean label
    d3.selectAll('.fundamental-studyLineLabel')
        .style('display', 'none')

    fundamental.emphasizeText("#fundamental-trigger-1, #fundamental-trigger-1 + p")
}

fundamental.scrollytellState2 = function(){
    console.log('fundamentalState2')

    fundamental.killAnimations()

    // resets
    d3.selectAll(".fundamental-rugLines, .fundamental-sampleMeanLine")
        .transition('fade-in')
        .transition('final-state')
        .style('display', 'none')
    
    // remove labels
    d3.selectAll('.fundamental-trueMeanLineLabel, .fundamental-firstRepeatedStudyLabel')
        .transition()
        .style('display', 'none')
    
    // kill kde
    fundamental.killkdeAnimations()
    d3.select('#fundamental-plot').selectAll('.fundamental-kdeLabel, .fundamental-kde').remove()

    // update the ATE text in the paragraph
    let delay = 700
    fundamental.timeoutStudyText = setTimeout(fundamental.changeStudyText, delay, false)
    
    // make sure trueMean and studyLine are displayed
    d3.selectAll(".fundamental-trueMeanLine, .fundamental-studyLine, .fundamental-studyLineLabel")
        .style('display', null)
    d3.selectAll(".fundamental-studyLine")
        .style('opacity', 0)
        .transition()
        .delay(delay) // ensures axis animation is finished
        .duration(1000)
        .style('opacity', 1)

    fundamental.emphasizeText("#fundamental-trigger-2, #fundamental-trigger-2 + p")
}

fundamental.scrollytellState3 = function(){
    console.log('fundamentalState3')

    fundamental.killAnimations()
    let container = d3.select('#fundamental-plot > svg > g')
    let {xScale, yScale} = fundamental.scales
    let delayFn = fundamental.delayFn

    // resets
    d3.select('#fundamental-plot').selectAll(".fundamental-rugLines, .fundamental-kde").style('display', 'none')
    clearTimeout(fundamental.timeoutStudyText)

    // remove study line label
    d3.selectAll('.fundamental-studyLineLabel, .fundamental-kdeLabel')
        .style('display', 'none')

    // make sure trueMean, studyLine, and rugLines are displayed
    d3.selectAll(".fundamental-trueMeanLine, .fundamental-studyLine")
        .style('display', null)
    
    // add new study text and then remove it
    container.append('text')
        .attr('class', 'fundamental-firstRepeatedStudyLabel')
        .attr('x', xScale(+fundamental.data.distribution[0].x))
        .attr('y', yScale(0.005*1.1))
        .text('A repeated draw')
        .attr('opacity', 0)
        .transition()
        .duration(500)
        .attr('opacity', 1)
        .transition()
        .delay(delayFn(fundamental.data.distribution[1].index) - 600)
        .style('opacity', 0)
        .remove()
    
    // animate rugLines
    d3.selectAll(".fundamental-rugLines")
        .transition('fade-in')
        .delay(d => delayFn(d.index))
        .style('display', null)
        .style('opacity', 1)
        .style('stroke-width', 2)
        .style('stroke', 'black')
    d3.selectAll(".fundamental-rugLines")
        .transition('final-state')
        .duration(400)
        .delay(d => delayFn(d.index + 1) + 100)
        .style('opacity', 0.1)
        .style('stroke-width', 1)
        .style('stroke', '#525252')
    
    // animate kde
    fundamental.data.distribution.map(function(d){
        // plot kde for every other index (for performance)
        let headstart = 100
        if (d.index % 2 == 0){
            fundamental.timeouts.push(setTimeout(fundamental.drawKDE, delayFn(d.index) - headstart, '#fundamental-plot > svg > g', d.index))
        }
    })

    // add final label
    let lag = 2000
    let n = fundamental.data.distribution.length
    container.append('text')
        .attr('class', 'fundamental-kdeLabel')
        .attr('x', 0)
        .attr('y', 0)
        .text(`Distribution of ${n.toLocaleString("en-US")} repeated draws`)
        .style('opacity', 0)
        .transition()
        .delay(delayFn(n) + lag)
        .duration(2000)
        .style('opacity', 1)
    
    // fade out rugLines
    d3.selectAll(".fundamental-rugLines")
        .transition('fade-out')
        .delay(delayFn(n) + lag)
        .duration(2000)
        .style('opacity', 0.01)

    // add mean line
    // d3.selectAll(".sampleMeanLine")
    //     .style('opacity', 0)
    //     .transition('fade-out')
    //     .delay(delayFn(n) + lag + 2000)
    //     .duration(2000)
    //     .style('opacity', 0.8)
    //     .style('display', null)
    //     .attr('y2', yScale(fundamental.data.kdeHeight))

        
    fundamental.emphasizeText("#fundamental-trigger-3, #fundamental-trigger-3 + p, #fundamental-trigger-3 + p + p")
}

fundamental.plotState = 1
fundamental.triggerScrollytellAnimation = function(){
    // trigger the closest animation

    // get the positions of divs relative to the top of the viewport
    let trigger1Pos = $('#fundamental-trigger-1')[0].getBoundingClientRect().top
    let trigger2Pos = $('#fundamental-trigger-2')[0].getBoundingClientRect().top
    let trigger3Pos = $('#fundamental-trigger-3')[0].getBoundingClientRect().top
    let positions = [trigger1Pos, trigger2Pos, trigger3Pos]
    let scrollyFns = [fundamental.scrollytellState1, fundamental.scrollytellState2, fundamental.scrollytellState3]

    // make off page elements positive
    positions = positions.map(Math.abs)

    // get smallest value
    const minVal = Math.min(...positions)
    const index = positions.indexOf(minVal)

    // update plot if state changed
    if (index != fundamental.plotState){
        scrollyFns[index]()
        fundamental.plotState = index
    }
}

// add listener
document.addEventListener('scroll', fundamental.triggerScrollytellAnimation);
