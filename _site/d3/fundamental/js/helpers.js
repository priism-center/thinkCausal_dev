
fundamental.generateData = function(mean){
    let sd = 10 //30 //Math.max(...[0.5, Math.abs(mean) * 0.2]) 
    let n = 1000
    let distribution = d3.range(n).map(function(i){
      num = jStat.normal.sample(mean, sd)
      num = num.toString()
      return {'x': num, "index": i}
    })

    return distribution;
  }
  
fundamental.sampleFrom = function(array){
    return array[Math.floor(Math.random() * array.length)];
}

// https://observablehq.com/@d3/kernel-density-estimation
function kde(kernel, thresholds, data) {
    return thresholds.map(t => [t, d3.mean(data, d => kernel(t - d.x))]); //x is the column name
  }
function epanechnikov(bandwidth) {
    return x => Math.abs(x /= bandwidth) <= 1 ? 0.75 * (1 - x * x) / bandwidth : 0;
}


fundamental.emphasizeText = function(selectors){
    d3.selectAll(".fundamental-text-along-d3 > p, .fundamental-text-along-d3 > h2")
        .style('filter', 'opacity(0.2)')
    // emphasize this text
    d3.selectAll(selectors)
        .style('filter', null)
}

fundamental.highlightText = function(selector, delay){

    let currentFontSize = d3.selectAll(selector).style('font-size')
    currentFontSize = currentFontSize.replace('px', '')
    let bigFontSize = (currentFontSize * 1.3) + 'px'
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

// controls the delay on the rugplot animation
fundamental.delayFn = function(index){ return ((((((index+1)**0.001)-1) * 5000000)) / 1.3) + 100 } // accelerating curve

// store timeouts for later cleaing
fundamental.timeouts = []
fundamental.killkdeAnimations = function(){
    fundamental.timeouts.map(clearTimeout)
    fundamental.timeouts = []
}

fundamental.runif = function(min, max) { 
  return Math.random() * (max - min) + min
}

// find a random point within +/- 0.5 and 0.75 sd from the mean
fundamental.setStudy = function(){
  let trueMean = +fundamental.data.trueMean
  let sd = jStat.stdev(fundamental.data.distribution.map(d => +d.x))
  let direction = Math.round(Math.random()) * 2 -1
  let magnitude = fundamental.runif(0.5, 0.75)
  let study = trueMean + (direction * sd * magnitude)

  return(study)
}

// find a point away from the study line and true mean
// this is used for highlighting the first point and making sure its readable
fundamental.setFirstRepeat = function(){
  let trueMean = +fundamental.data.trueMean
  let study = +fundamental.data.studyLine
  if (trueMean > study){
    return trueMean + 5 // Math.max(...[0.5, Math.abs(trueMean) * 0.1])
  } else return study + 5 // Math.max(...[0.5, Math.abs(study) * 0.1])
}

fundamental.roundNumber = function(num, dec){
  // rounds a number to a certain decimal place and always maintains a decimal point
  let rounded = Math.round(num * Math.pow(10, dec)) / Math.pow(10, dec)
  return rounded.toFixed(dec)
}

fundamental.changeStudyText = function(blank){
  let ATE = fundamental.roundNumber(fundamental.data.studyLine, 2)
  if (blank) ATE = "____"
  // console.log(runner.length)
  let newText = "Your study had an ATE of " + ATE
  d3.select("#fundamental-study-text").text(newText)
}