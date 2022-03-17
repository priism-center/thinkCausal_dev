// scrollytell
// these animation states are kind of a mess
// TODO: would a state machine work? https://bl.ocks.org/bricof/aff127297d7453ef18459cf52050ed6d
function estimands_d3State1(){
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

    emphasizeText("#trigger-1, #trigger-1 + p")
}

function estimands_d3State2(){
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

    emphasizeText("#trigger-2, #trigger-2 + p")
}

function estimands_d3State3(){
    console.log('estimandsState3')

    // trigger plot change
    d3.selectAll(".scatterPoints")
        .style('display', null)
        .transition()
        .duration(1200)
        .style('opacity', 0.2)
        .attr("pointer-events", "all")
    d3.selectAll(".meanLines")
        .style('display', 'none')
    
    // show example lines
    d3.selectAll(".showOnHover[pairID='" + 10 + "'], .scatterPoints[pairID='" + 10 + "']")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1400)
        .delay(1000)
        .style('opacity', 1)

    emphasizeText("#trigger-3, #trigger-3 + p, #trigger-3 + p + p")
}

function estimands_d3State4(){
    console.log('estimandsState4')

    // trigger plot change
    d3.selectAll(".meanLines")
        .style('display', null)
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", "none")
        .transition() //this kills currently running transitions
        .style('opacity', 0.2)
    d3.selectAll(".showOnHover")
        .style('display', 'none')

    emphasizeText("#trigger-4, #trigger-4 + p")
}

function estimands_d3State5(){
    console.log('estimandsState5')

    // // trigger plot change
    // d3.selectAll(".meanLines")
    //     .style('display', null)
    // d3.selectAll(".scatterPoints")
    //     .attr("pointer-events", "none")
    //     .transition() //this kills currently running transitions
    //     .style('opacity', 0.2)
    // d3.selectAll(".showOnHover")
    //     .style('display', 'none')

    emphasizeText("#trigger-5, #trigger-5 + p")
}

function estimands_d3State6(){
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

    emphasizeText("#trigger-6, #trigger-6 + p")
}

let estimands_plotState = 1
function estimands_triggerD3Animation(){
    // trigger the closest animation 

    // get the positions of divs relative to the top of the viewport
    let trigger1Pos = $('#trigger-1')[0].getBoundingClientRect().top
    let trigger2Pos = $('#trigger-2')[0].getBoundingClientRect().top
    let trigger3Pos = $('#trigger-3')[0].getBoundingClientRect().top
    let trigger4Pos = $('#trigger-4')[0].getBoundingClientRect().top
    let trigger5Pos = $('#trigger-5')[0].getBoundingClientRect().top
    let trigger6Pos = $('#trigger-6')[0].getBoundingClientRect().top
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
    if (index != estimands_plotState){
        if (index == 0) estimands_d3State1()
        if (index == 1) estimands_d3State2()
        if (index == 2) estimands_d3State3()
        if (index == 3) estimands_d3State4()
        if (index == 4) estimands_d3State5()
        if (index == 5) estimands_d3State6()
        estimands_plotState = index
    }
}

// add listener 
document.addEventListener('scroll', estimands_triggerD3Animation);


//// helpers
function emphasizeText(selectors){
    d3.selectAll(".estimands-text-along-d3 > p, .estimands-text-along-d3 > h2")
        .style('filter', 'opacity(0.2)')
    // emphasize this text
    d3.selectAll(selectors)
        .style('filter', null)
}
