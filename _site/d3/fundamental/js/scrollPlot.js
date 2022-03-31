// scrollytell
// these animation states are kind of a mess
// TODO: would a state machine work? https://bl.ocks.org/bricof/aff127297d7453ef18459cf52050ed6d
fundamental.d3State1 = function(){
    console.log('fundamentalState1')

    fundamental.killAnimations()

    // resets
    d3.selectAll(".studyLine, .rugLines")
        .style('display', 'none')

    fundamental.emphasizeText("#fundamental-trigger-1, #fundamental-trigger-1 + p")
}

fundamental.d3State2 = function(){
    console.log('fundamentalState2')

    fundamental.killAnimations()

    // resets
    d3.selectAll(".rugLines")
        .style('display', 'none')

    // // generate new distribution and study based on input
    // newMean = +$("#input-distribution-mean")[0].value
    // fundamental.data.trueMean = newMean
    // fundamental.data.distribution = fundamental.generateData(newMean);
    // fundamental.data.studyLine = fundamental.sampleFrom(fundamental.data.distribution).x;

    // remove plot and redraw
    // d3.select('#fundamental-plot svg').remove()
    // fundamental.drawData()
    
    // make sure trueMean and studyLine are displayed
    d3.selectAll(".trueMeanLine, .studyLine")
        .style('display', null)

    fundamental.emphasizeText("#fundamental-trigger-2, #fundamental-trigger-2 + p")
}

fundamental.d3State3 = function(){
    console.log('fundamentalState3')

    fundamental.killAnimations()

    // resets

    // make sure trueMean, studyLine, and rugLines are displayed
    d3.selectAll(".trueMeanLine, .studyLine, .rugLines")
        .style('display', null)

    fundamental.emphasizeText("#fundamental-trigger-3, #fundamental-trigger-3 + p, #fundamental-trigger-3 + p + p")
}

fundamental.d3State4 = function(){
    console.log('fundamentalState4')

    fundamental.killAnimations()

    // resets
  

    fundamental.emphasizeText("#fundamental-trigger-4, #fundamental-trigger-4 + p")
}

fundamental.d3State5 = function(){
    console.log('fundamentalState5')

    fundamental.killAnimations()

    // resets


    fundamental.emphasizeText("#fundamental-trigger-5, #fundamental-trigger-5 + p")
}

fundamental.d3State6 = function(){
    console.log('fundamentalState6')

    fundamental.killAnimations()


    fundamental.emphasizeText("#fundamental-trigger-6, #fundamental-trigger-6 + p")
}

fundamental.plotState = 1
fundamental.triggerD3Animation = function(){
    // trigger the closest animation

    // get the positions of divs relative to the top of the viewport
    let trigger1Pos = $('#fundamental-trigger-1')[0].getBoundingClientRect().top
    let trigger2Pos = $('#fundamental-trigger-2')[0].getBoundingClientRect().top
    let trigger3Pos = $('#fundamental-trigger-3')[0].getBoundingClientRect().top
    let trigger4Pos = $('#fundamental-trigger-4')[0].getBoundingClientRect().top
    let trigger5Pos = $('#fundamental-trigger-5')[0].getBoundingClientRect().top
    let trigger6Pos = $('#fundamental-trigger-6')[0].getBoundingClientRect().top
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
    if (index != fundamental.plotState){
        if (index == 0) fundamental.d3State1()
        if (index == 1) fundamental.d3State2()
        if (index == 2) fundamental.d3State3()
        if (index == 3) fundamental.d3State4()
        if (index == 4) fundamental.d3State5()
        if (index == 5) fundamental.d3State6()
        fundamental.plotState = index
    }
}

// add listener
document.addEventListener('scroll', fundamental.triggerD3Animation);


//// helpers ////
fundamental.emphasizeText = function(selectors){
    d3.selectAll(".fundamental-text-along-d3 > p, .fundamental-text-along-d3 > h2")
        .style('filter', 'opacity(0.2)')
    // emphasize this text
    d3.selectAll(selectors)
        .style('filter', null)
}
// fundamental.dropICE = function(data){
// // calculates the end position for each ICE segment
//     d3.map(data, function(d) {
//         d.drop_x1 = (d.pair_id - 1) / 10
//         d.drop_y1 = d.yName_y0 - d.yName_y1 // reverse?
//         d.drop_x2 = (d.pair_id - 1) / 10
//         d.drop_y2 = 0
//     })
// }
fundamental.highlightText = function(selector, delay){

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
fundamental.killAnimations = function(){
    // calling a blank transition again will kill
    // any previous running ones with the same name
    d3.selectAll("#fundamental-plot-ATE *")
        .transition()
    d3.selectAll("#fundamental-plot-ATE *")
        .transition('highlightText')
}


// initialize state
fundamental.d3State1()
