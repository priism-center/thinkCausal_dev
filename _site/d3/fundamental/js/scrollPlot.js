// scrollytell
// these animation states are kind of a mess
// TODO: would a state machine work? https://bl.ocks.org/bricof/aff127297d7453ef18459cf52050ed6d
fundamental.scrollytellState1 = function(){
    console.log('fundamentalState1')

    fundamental.killAnimations()

    // resets
    d3.selectAll(".studyLine, .rugLines")
        .style('display', 'none')
    // d3.selectAll(".trueMeanLineLabel")
    //     .style('display', null)
    
    // remove trueMean label
    d3.selectAll('.studyLineLabel')
        .style('display', 'none')

    fundamental.emphasizeText("#fundamental-trigger-1, #fundamental-trigger-1 + p")
}

fundamental.scrollytellState2 = function(){
    console.log('fundamentalState2')

    fundamental.killAnimations()

    // resets
    d3.selectAll(".rugLines")
        .style('display', 'none')
    
    // remove trueMean label
    d3.selectAll('.trueMeanLineLabel')
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
    d3.selectAll(".trueMeanLine, .studyLine, .studyLineLabel")
        .style('display', null)

    fundamental.emphasizeText("#fundamental-trigger-2, #fundamental-trigger-2 + p")
}

fundamental.scrollytellState3 = function(){
    console.log('fundamentalState3')

    fundamental.killAnimations()

    // resets

    // remove study line label
    d3.selectAll('.studyLineLabel')
        .style('display', 'none')

    // make sure trueMean, studyLine, and rugLines are displayed
    d3.selectAll(".trueMeanLine, .studyLine, .rugLines")
        .style('display', null)

    fundamental.emphasizeText("#fundamental-trigger-3, #fundamental-trigger-3 + p, #fundamental-trigger-3 + p + p")
}

fundamental.scrollytellState4 = function(){
    console.log('fundamentalState4')

    fundamental.killAnimations()

    // resets
  

    fundamental.emphasizeText("#fundamental-trigger-4, #fundamental-trigger-4 + p")
}

fundamental.scrollytellState5 = function(){
    console.log('fundamentalState5')

    fundamental.killAnimations()

    // resets


    fundamental.emphasizeText("#fundamental-trigger-5, #fundamental-trigger-5 + p")
}

fundamental.scrollytellState6 = function(){
    console.log('fundamentalState6')

    fundamental.killAnimations()


    fundamental.emphasizeText("#fundamental-trigger-6, #fundamental-trigger-6 + p")
}

fundamental.plotState = 1
fundamental.triggerScrollytellAnimation = function(){
    // trigger the closest animation

    // get the positions of divs relative to the top of the viewport
    let trigger1Pos = $('#fundamental-trigger-1')[0].getBoundingClientRect().top
    let trigger2Pos = $('#fundamental-trigger-2')[0].getBoundingClientRect().top
    let trigger3Pos = $('#fundamental-trigger-3')[0].getBoundingClientRect().top
    let trigger4Pos = $('#fundamental-trigger-4')[0].getBoundingClientRect().top
    let trigger5Pos = $('#fundamental-trigger-5')[0].getBoundingClientRect().top
    let trigger6Pos = $('#fundamental-trigger-6')[0].getBoundingClientRect().top
    let positions = [trigger1Pos, trigger2Pos, trigger3Pos, trigger4Pos, trigger5Pos, trigger6Pos]

    // make off page elements positive
    positions = positions.map(Math.abs)

    // get smallest value
    const minVal = Math.min(...positions)
    const index = positions.indexOf(minVal)

    // update plot if state changed
    if (index != fundamental.plotState){
        if (index == 0) fundamental.scrollytellState1()
        if (index == 1) fundamental.scrollytellState2()
        if (index == 2) fundamental.scrollytellState3()
        if (index == 3) fundamental.scrollytellState4()
        if (index == 4) fundamental.scrollytellState5()
        if (index == 5) fundamental.scrollytellState6()
        fundamental.plotState = index
    }
}

// add listener
document.addEventListener('scroll', fundamental.triggerScrollytellAnimation);
