function showData(data){
  //buildTable(data.ratings)
  buildPlot(data)
}

// read in the data
function loadData() {
    return Promise.all([
        d3.csv("data/point-data.csv"),
        d3.csv("data/line-data.csv"),
    ]).then(datasets => {
        store = {},
        store.scatter = datasets[0];
        store.line = datasets[1];
        console.log("Loaded data:", store)
        return store;
    })
}

loadData().then(showData)

// scrollytell
// these animation states are kind of a mess
// TODO: would a state machine work? https://bl.ocks.org/bricof/aff127297d7453ef18459cf52050ed6d
function d3State1(){
    console.log('state1')

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

    // emphasize this text
    d3.selectAll("#trigger-1, #trigger-1 + p")
        .style('filter', null)
    
    // dephasize other text
    d3.selectAll("#trigger-2, #trigger-2 + p, #trigger-2 + p + p, #trigger-3, #trigger-3 + p")
        .style('filter', 'opacity(0.2)')
}
function d3State2(){
    console.log('state2')

    // trigger plot change
    // TODO: scaffold factual / CF points
    d3.selectAll(".scatterPoints")
        .style('display', null)
        .transition()
        .duration(1200)
        .style('opacity', 0.2)
        .attr("pointer-events", "all")
    d3.selectAll(".meanLines")
        .style('display', 'none')
    
    // show example lines
    // TODO: issue here with animation timing
    d3.selectAll(".showOnHover[pairID='" + 10 + "'], .scatterPoints[pairID='" + 10 + "']")
        .style('display', null)
        .style('opacity', 0)
        .transition()
        .duration(1400)
        .delay(3500)
        .style('opacity', 1)

    // emphasize this text
    d3.selectAll("#trigger-2, #trigger-2 + p, #trigger-2 + p + p")
        .style('filter', null)
    
    // dephasize other text
    d3.selectAll("#trigger-1, #trigger-1 + p, #trigger-3, #trigger-3 + p")
        .style('filter', 'opacity(0.2)')
}

function d3State3(){
    console.log('state3')

    // trigger plot change
    d3.selectAll(".meanLines")
        .style('display', null)
    d3.selectAll(".scatterPoints")
        .attr("pointer-events", "none")
        .transition() //this kills currently running transitions
        .style('opacity', 0.2)
    d3.selectAll(".showOnHover")
        .style('display', 'none')

    // emphasize this text
    d3.selectAll("#trigger-3, #trigger-3 + p")
        .style('filter', null)
    
    // dephasize other text
    d3.selectAll("#trigger-1, #trigger-1 + p, #trigger-2, #trigger-2 + p, #trigger-2 + p + p")
        .style('filter', 'opacity(0.2)')
}

function triggerD3Animation(){
    // trigger the closest animation 

    // get the positions of divs relative to the top of the viewport
    let trigger1Pos = $('#trigger-1')[0].getBoundingClientRect().top
    let trigger2Pos = $('#trigger-2')[0].getBoundingClientRect().top
    let trigger3Pos = $('#trigger-3')[0].getBoundingClientRect().top
    let positions = [trigger1Pos, trigger2Pos, trigger3Pos]

    //// for elements that are off the page, replace with really large number
    // make off page elements positive
    for (var i = positions.length-1; i >= 0; i--){
        if (positions[i] < 0){
            positions.splice(i, 1, Math.abs(positions[i])) //*10000
        }
    }
    // console.log(positions)

    // get smallest value 
    const minVal = Math.min(...positions)
    const index = positions.indexOf(minVal)

    // console.log(index)
    if (index == 0) d3State1()
    if (index == 1) d3State2()
    if (index == 2) d3State3()
}

// add listener 
document.addEventListener('scroll', triggerD3Animation);

// TODO set initial state on page load
