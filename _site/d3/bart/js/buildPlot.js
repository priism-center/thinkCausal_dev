

fundamental.getConfig = function(selector) {
  let width = 540; //900px is width of learning article * 0.6
  let height = 400;
  let margin = {
      top: 30,
      bottom: 80,
      left: 60,
      right: 20
  }

  // the body is the area that will be occupied by the plot
  let bodyHeight = height - margin.top - margin.bottom;
  let bodyWidth = width - margin.left - margin.right;

  // the container is the SVG where we will draw the plot
  let container = d3.select(selector)
    .append("svg")
      .attr("class", "plot")
      .attr("preserveAspectRatio", "xMinYMin meet")
      .attr("viewBox", "0 0 " + width + " " + height)
    .append("g")
      .attr("transform",
            "translate(" + margin.left + "," + margin.top + ")")

  return {width, height, margin, bodyHeight, bodyWidth, container}
}

fundamental.getScales = function(data, config) {
 let { bodyWidth, bodyHeight, container } = config;
 let maximumValue = fundamental.data.max  //d3.max(data.distribution, d => +d.x);
 let minimumValue = fundamental.data.min //d3.min(data.distribution, d => +d.x);
 let padding = (maximumValue - minimumValue) * 0.15

 let xScale = d3.scaleLinear()
    .domain([minimumValue - padding, maximumValue + padding])
    .range([0, bodyWidth])
 let yScale = d3.scaleLinear()
    .domain([0, 0.045]) //1.5])
    .range([bodyHeight, 0])

 return {xScale, yScale}
}

fundamental.drawRug = function(data, scales, config){
  let {margin, container, bodyHeight, bodyWidth} = config;
  let {xScale, yScale} = scales
  let rugHeight = 0.005
  console.log('Data into fundamental.drawRug():', data)


  // draw the rug
  container.append('g')
    .selectAll('line')
    .data(data.distribution)
    .enter()
    .append('line')
      .attr('x1', d => xScale(d.x))
      .attr('y1', yScale(0))
      .attr('x2', d => xScale(d.x))
      .attr('y2', yScale(rugHeight))
      .style('stroke', "#525252")
      .style('stroke-width', 1)
      .style('opacity', '0.03')
      .style('display', 'none')
      .attr('class', 'fundamental-rugLines')

  // add true mean line
  container.append('line')
    .attr('class', 'fundamental-trueMeanLine')
    .attr('x1', xScale(fundamental.data.trueMean))
    .attr('y1', yScale(-0.003))
    .attr('x2', xScale(fundamental.data.trueMean))
    .attr('y2', yScale(rugHeight*3))
    .style('display', 'none')
    .style('z-index', -999)
  // container.append('text')
  //   .attr('class', 'trueMeanLineLabel')
  //   .attr('x', xScale(+fundamental.data.trueMean + 5))
  //   .attr('y', yScale(1*0.95))
  //   .text('True mean')
  //   .style('display', 'none')

  // add study line
  container.append('line')
    .attr('class', 'fundamental-studyLine')
    .attr('x1', xScale(fundamental.data.studyLine))
    .attr('y1', yScale(0))
    .attr('x2', xScale(fundamental.data.studyLine))
    .attr('y2', yScale(rugHeight))
    .style('stroke', '#674ca1')
    .style('stroke-width', 3)
    .style('display', 'none')
  // container.append('text')
  //   .attr('class', 'studyLineLabel')
  //   .attr('x', xScale(+fundamental.data.studyLine + 5))
  //   .attr('y', yScale(1*0.95))
  //   .text('Study results')
  //   .style('display', 'none')

  // add sample mean line
  let rugSampleHeight = rugHeight * 2 //fundamental.data.kdeHeight
  container.append('line')
    .attr('class', 'fundamental-sampleMeanLine')
    .attr('x1', xScale(fundamental.data.sampleMean))
    .attr('y1', yScale(0))
    .attr('x2', xScale(fundamental.data.sampleMean))
    .attr('y2', yScale(rugSampleHeight))
    .style('stroke', 'black')
    .style('stroke-width', 3)
    .style('display', 'none')


  // add X axis
  let xAxis = d3.axisBottom(xScale)
  container.append("g")
    .attr('class', "fundamental-axis fundamental-xAxis")
    .attr("transform", "translate(0," + bodyHeight + ")")
    .call(xAxis);
  container.append('text')
    .attr('class', 'fundamental-axisLabel')
    .attr("x", bodyWidth/2)
    .attr('y', bodyHeight + margin.bottom/2)
    .attr('text-anchor', 'middle')
    .text("Change in running time (seconds)")
}

fundamental.drawKDE = function(selector, index){
  let container = d3.selectAll(selector)
  let xScale = fundamental.scales.xScale
  let yScale = fundamental.scales.yScale
  let data = fundamental.data.distribution.filter(d => d.index <= index)
  let opacity = index / d3.max(fundamental.data.distribution, d => d.index)

  // remove current line
  container.selectAll('.fundamental-kde').remove()
  
  // add KDE
  let thresholds = xScale.ticks(30)
  let density = kde(epanechnikov(7), thresholds, data)
  fundamental.data.kdeHeight = d3.max(density, d => d[1])
  container.append('path')
    .attr('class', 'fundamental-kde fundamental-kde-' + index)
    .datum(density)
    .attr('d', d3.line()
      .curve(d3.curveBasis)
      .x(d => xScale(d[0]))
      .y(d => yScale(d[1]))
    )
    .style("stroke-width", 2)
    .style("stroke-linejoin", "round")
    .style("fill", "none")
    .style("stroke", "#292929")
    .style('opacity', opacity)
}

fundamental.drawData = function(selector) {
  let data = fundamental.data
  let config = fundamental.getConfig(selector);
  fundamental.config = config
  let scales = fundamental.getScales(data, config);
  fundamental.scales = scales
  fundamental.drawRug(data, scales, config);
}

// update plot when user changes the mean
fundamental.updatePlot = function(value) {

  // ensure mean line is shown when there is input
  d3.selectAll(".fundamental-trueMeanLine, .fundamental-trueMeanLineLabel")
      .style('display', null)
  
  // don't update if numeric value is the same as previous input
  if (+value == +fundamental.data.trueMean) return ;
  
  // update true mean value
  let newMean = +fundamental.trueMeanSlider.val() //$("#input-distribution-mean").val()
  fundamental.data.trueMean = newMean

  // generate new distribution and study based on input
  fundamental.data.distribution = fundamental.generateData(newMean, fundamental.data.trueSD);
  fundamental.data.studyLine = fundamental.setStudy();
  fundamental.data.sampleMean = d3.mean(fundamental.data.distribution, d => d.x);
  // fundamental.data.studyLine = fundamental.sampleFrom(fundamental.data.distribution).x;
  
  // guarantee first observation is far from studyLine and mean
  fundamental.data.distribution[0].x = fundamental.setFirstRepeat()

  // remove plot and redraw
  d3.select('#fundamental-plot svg').remove()
  fundamental.drawData("#fundamental-plot")
  fundamental.updateXAxis()
  
  // make sure true lines are displayed
  d3.selectAll(".fundamental-trueMeanLine, .fundamental-trueMeanLineLabel")
    .style('display', null)
}

// update the x scale based on user input
fundamental.updateXAxis = function(){
  const range = d3.extent(fundamental.data.distribution, d => +d.x)
  const maximumValue = range[1]
  const minimumValue = range[0]
  fundamental.data.max = maximumValue
  fundamental.data.min = minimumValue
  const animationDuration = 1000

  // new scale and axis
  let padding = (maximumValue - minimumValue) * 0.15
  let newXScale = d3.scaleLinear()
    .domain([minimumValue - padding, maximumValue + padding])
    .range([0, fundamental.config.bodyWidth])
  let newAxis = d3.axisBottom(newXScale)

  // store for use in KDE
  fundamental.scales.xScale = newXScale

  // animate to new axis
  d3.select('.fundamental-xAxis')
    .transition()
    .duration(animationDuration)
    // .ease(d3.easeLinear)
    .call(newAxis)
  
  // animate rug
  d3.selectAll('.fundamental-rugLines')
    .transition()
    .duration(animationDuration)
    // .ease(d3.easeLinear)
    .attr('x1', d => newXScale(d.x))
    .attr('x2', d => newXScale(d.x))
  d3.selectAll('.fundamental-trueMeanLine')
    .transition()
    .duration(animationDuration)
    // .ease(d3.easeLinear)
    .attr('x1', newXScale(fundamental.data.trueMean))
    .attr('x2', newXScale(fundamental.data.trueMean))
  d3.selectAll('.fundamental-studyLine')
    .transition()
    .duration(animationDuration)
    // .ease(d3.easeLinear)
    .attr('x1', newXScale(fundamental.data.studyLine))
    .attr('x2', newXScale(fundamental.data.studyLine))
}

// intialize plot
fundamental.showData = function() {
    // initialize values
    fundamental.data = {}
    fundamental.data.trueMean = 0;
    fundamental.data.trueSD = 10;
    fundamental.data.distribution = fundamental.generateData(fundamental.data.trueMean, fundamental.data.trueSD);
    // fundamental.data.studyLine = fundamental.sampleFrom(fundamental.data.distribution).x;
    fundamental.data.studyLine = fundamental.setStudy()
    fundamental.data.distribution[0].x = fundamental.setFirstRepeat()
    fundamental.data.sampleMean = d3.mean(fundamental.data.distribution, d => d.x);
    fundamental.data.max = d3.max(fundamental.data.distribution, d => +d.x);
    fundamental.data.min = d3.min(fundamental.data.distribution, d => +d.x);

    // initialize plot
    fundamental.drawData('#fundamental-plot');
}


// build the efficiency and bias plots
fundamental.buildDensity = function(selector){ 

  // attach svg
  let data = fundamental.data
  let config = fundamental.getConfig(selector);
  let {margin, container, bodyHeight, bodyWidth} = config
  let {xScale, yScale} = fundamental.getScales(data, config);

  // add X axis
  let xAxis = d3.axisBottom(xScale)
  container.append("g")
    .attr('class', "fundamental-axis fundamental-xAxis")
    .attr("transform", "translate(0," + bodyHeight + ")")
    .call(xAxis);
  container.append('text')
    .attr('class', 'fundamental-axisLabel')
    .attr("x", bodyWidth/2)
    .attr('y', bodyHeight + margin.bottom/2)
    .attr('text-anchor', 'middle')
    .text("Change in running time (seconds)")

  // draw density
  selector = selector + " > svg > g"
  let n_data = fundamental.data.distribution.length
  fundamental.drawKDE(selector, n_data)

  // add true line
  container.append('line')
    .attr('class', 'fundamental-trueMeanLine')
    .attr('x1', xScale(fundamental.data.trueMean))
    .attr('y1', yScale(-0.003))
    .attr('x2', xScale(fundamental.data.trueMean))
    .attr('y2', yScale(0.005*3))
    // .style('display', 'none')
    .style('z-index', -999)

  return {config, xScale, yScale};
}
fundamental.buildEfficiencyPlot = function(){
  // build plot with original kde
  let {config, xScale, yScale} = fundamental.buildDensity('#fundamental-plot-efficiency');
  let {margin, container, bodyHeight, bodyWidth} = config;


  // add labels 
  container.append('text')
    .attr('class', 'fundamental-support-label fundamental-efficiency-label-old')
    .attr("x", bodyWidth * 1/2)
    .attr('y', 10)
    .attr('text-anchor', 'middle')
    .text('More efficient')
  container.append('text')
    .attr('class', 'fundamental-support-label fundamental-efficiency-label-new')
    .attr("x", bodyWidth * 1/2)
    .attr('y', 115)
    .attr('text-anchor', 'middle')
    .text('Less efficient')
    .style('opacity', 0)


  // calculate new kde
  let wideningFactor = 1.75
  let data = fundamental.generateData(fundamental.data.trueMean, fundamental.data.trueSD * wideningFactor);

  // duplicate current KDE
  container.select('.fundamental-kde')
    .clone()
    .style('stroke-dasharray', '4, 4')
    .style('opacity', 0.2);

  // calculate new KDE
  let thresholds = xScale.ticks(30)
  let densityOld = kde(epanechnikov(7), thresholds, fundamental.data.distribution)
  let densityNew = kde(epanechnikov(7), thresholds, data)

  // transitions to and fro new KDE
  let duration = 5000
  function transitionToNewKDE(){
    let stall = 1000
    container.select('.fundamental-kde') 
      .datum(densityNew)
      .transition()
      .duration(duration)
      .delay(stall)
      .attr('d', d3.line()
        .curve(d3.curveBasis)
        .x(d => xScale(d[0]))
        .y(d => yScale(d[1]))
      )
      .on('end', transitionToOldKDE)

  // update labels
  container.select('.fundamental-efficiency-label-old')
    .transition()
    .duration(duration)
    .delay(stall)
    .ease(d3.easeExpOut)
    .style('opacity', 0)
  container.select('.fundamental-efficiency-label-new')
    .transition()
    .duration(duration)
    .delay(stall)
    .ease(d3.easeExpIn)
    .style('opacity', 1)
  }
  function transitionToOldKDE(){
    let stall = 3000
    container.select('.fundamental-kde')
      .datum(densityOld)
      .transition()
      .duration(duration)
      .delay(stall)
      .attr('d', d3.line()
        .curve(d3.curveBasis)
        .x(d => xScale(d[0]))
        .y(d => yScale(d[1]))
      )
      .on('end', transitionToNewKDE)

    // update labels
    container.select('.fundamental-efficiency-label-old')
      .transition()
      .duration(duration)
      .delay(stall)
      .ease(d3.easeExpIn)
      .style('opacity', 1)
    container.select('.fundamental-efficiency-label-new')
      .transition()
      .duration(duration)
      .delay(stall)
      .ease(d3.easeExpOut)
      .style('opacity', 0)
  }

  // initialize transition
  transitionToNewKDE();
};
fundamental.buildBiasPlot = function(){
  // build plot with original kde
  let {config, xScale, yScale} = fundamental.buildDensity('#fundamental-plot-bias');
  let {margin, container, bodyHeight, bodyWidth} = config;


  // add labels 
  container.append('text')
    .attr('class', 'fundamental-support-label fundamental-bias-label-old')
    .attr("x", bodyWidth * 1/2)
    .attr('y', 10)
    .attr('text-anchor', 'middle')
    .text('Unbiased')
  container.append('text')
    .attr('class', 'fundamental-support-label fundamental-bias-label-new')
    .attr("x", bodyWidth * 2.5/4)
    .attr('y', 10)
    .attr('text-anchor', 'middle')
    .text('Biased')
    .style('opacity', 0)

  // calculate new kde by shifting the current kde
  let data = JSON.parse(JSON.stringify(fundamental.data.distribution)) // deep copy
  data.forEach(d => d.x = +d.x + 7) // right shift

  // duplicate current KDE
  container.select('.fundamental-kde')
    .clone()
    .style('stroke-dasharray', '4, 4')
    .style('opacity', 0.2);

  // calculate new KDE
  let thresholds = xScale.ticks(30)
  let densityOld = kde(epanechnikov(7), thresholds, fundamental.data.distribution)
  let densityNew = kde(epanechnikov(7), thresholds, data)

  // transitions to and fro new KDE
  let duration = 5000
  function transitionToNewKDE(){
    let stall = 1000
    container.select('.fundamental-kde') 
      .datum(densityNew)
      .transition()
      .duration(duration)
      .delay(stall)
      .attr('d', d3.line()
        .curve(d3.curveBasis)
        .x(d => xScale(d[0]))
        .y(d => yScale(d[1]))
      )
      // .ease(d3.easeQuadOut) //https://github.com/d3/d3-ease
      .on('end', transitionToOldKDE)

    // update labels
    container.select('.fundamental-bias-label-old')
      .transition()
      .duration(duration)
      .delay(stall)
      .ease(d3.easeExpOut)
      .style('opacity', 0)
    container.select('.fundamental-bias-label-new')
      .transition()
      .duration(duration)
      .delay(stall)
      .ease(d3.easeExpIn)
      .style('opacity', 1)
  }
  function transitionToOldKDE(){
    let stall = 3000
    // update KDE
    container.select('.fundamental-kde')
      .datum(densityOld)
      .transition()
      .duration(duration)
      .delay(stall)
      .attr('d', d3.line()
        .curve(d3.curveBasis)
        .x(d => xScale(d[0]))
        .y(d => yScale(d[1]))
      )
      // .ease(d3.easeQuadInOut) //https://github.com/d3/d3-ease
      .on('end', transitionToNewKDE)

    // update labels
    container.select('.fundamental-bias-label-old')
      .transition()
      .duration(duration)
      .delay(stall)
      .ease(d3.easeExpIn)
      .style('opacity', 1)
    container.select('.fundamental-bias-label-new')
      .transition()
      .duration(duration)
      .delay(stall)
      .ease(d3.easeExpOut)
      .style('opacity', 0)
  }

  // initialize transition
  transitionToNewKDE()

  // wrap labels in a div
  // $('.fundamental-support-label').wrap("<div class='fundamental-support-label-bg'></div>")
};
fundamental.buildSupportPlots = function(){

  // remove old plots
  d3.selectAll('#fundamental-plot-efficiency > svg').remove()
  d3.selectAll('#fundamental-plot-bias > svg').remove()

  // build new plots
  fundamental.buildEfficiencyPlot();
  fundamental.buildBiasPlot();
}