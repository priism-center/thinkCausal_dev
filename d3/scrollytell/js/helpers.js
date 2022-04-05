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
        d.drop_y1 = 0 - estimands.bottomPlotOffset
        d.drop_x2 = (d.pair_id - 1) / 10
        d.drop_y2 = (d.yName_y1 - d.yName_y0) - estimands.bottomPlotOffset
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

estimands.clone = function(selector) {
    var node = d3.select(selector).node();
    return d3.select(node.parentNode.insertBefore(node.cloneNode(true), node.nextSibling));
}

estimands.changeRunnerText = function(runner){
    ICE = estimands.data.line.filter(d => d.pair_id == +runner)
    ICE = estimands.roundNumber(ICE[0].yName_y1 - ICE[0].yName_y0, 2)
    newText = "Runner " + runner + " has an ICE of " + ICE
    d3.select("#estimands-runner-text").text(newText)
}
