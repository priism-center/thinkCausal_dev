

fundamental.getConfig = function() {
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
  let container = d3.select("#fundamental-plot")
    .append("svg")
      .attr("class", "plot")
      .attr("preserveAspectRatio", "xMinYMin meet")
      .attr("viewBox", "0 0 " + width + " " + height)
    .append("g")
      .attr("transform",
            "translate(" + margin.left + "," + margin.top + ")")

  fundamental.config = {width, height, margin, bodyHeight, bodyWidth, container}
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

  fundamental.scales = {xScale, yScale}

 return {xScale, yScale}
}

fundamental.drawRug = function(data, scales, config){
  let {margin, container, bodyHeight, bodyWidth} = config;
  let {xScale, yScale} = scales
  let rugHeight = 0.005
  console.log('Data into fundamental.drawRug():', data)


  // add X axis
  let xAxis = d3.axisBottom(xScale)
  container.append("g")
    .attr('class', "axis xAxis")
    .attr("transform", "translate(0," + bodyHeight + ")")
    .call(xAxis);
  container.append('text')
    .attr('class', 'axisLabel')
    .attr("x", bodyWidth/2)
    .attr('y', bodyHeight + margin.bottom/2)
    .attr('text-anchor', 'middle')
    .text("Change in running time")

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
      .attr('class', 'rugLines')

  // add true mean line
  container.append('line')
    .attr('class', 'trueMeanLine')
    .attr('x1', xScale(fundamental.data.trueMean))
    .attr('y1', yScale(0))
    .attr('x2', xScale(fundamental.data.trueMean))
    .attr('y2', yScale(rugHeight))
    .style('stroke', 'red')
    .style('stroke-width', 3)
    .style('display', 'none')
  // container.append('text')
  //   .attr('class', 'trueMeanLineLabel')
  //   .attr('x', xScale(+fundamental.data.trueMean + 5))
  //   .attr('y', yScale(1*0.95))
  //   .text('True mean')
  //   .style('display', 'none')

  // add study line
  container.append('line')
    .attr('class', 'studyLine')
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
}

fundamental.redrawKDE = function(index){
  let container = d3.selectAll('#fundamental-plot > svg > g')
  let xScale = fundamental.scales.xScale
  let yScale = fundamental.scales.yScale
  let data = fundamental.data.distribution.filter(d => d.index <= index)
  let opacity = index / d3.max(fundamental.data.distribution, d => d.index)

  // remove current line
  d3.selectAll('.fundamental-kde').remove()
  
  // add KDE
  let thresholds = xScale.ticks(30)
  let density = kde(epanechnikov(7), thresholds, data)
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

fundamental.drawData = function() {
  let data = fundamental.data
  let config = fundamental.getConfig();
  let scales = fundamental.getScales(data, config);
  fundamental.drawRug(data, scales, config);
}

// update plot when user changes the mean
fundamental.updatePlot = function(value) {

  // ensure mean line is shown when there is input
  d3.selectAll(".trueMeanLine, .trueMeanLineLabel")
      .style('display', null)
  
  // don't update if numeric value is the same as previous input
  if (+value == +fundamental.data.trueMean) return ;
  
  // update true mean value
  let newMean = +fundamental.trueMeanSlider.val() //$("#input-distribution-mean").val()
  fundamental.data.trueMean = newMean

  // generate new distribution and study based on input
  fundamental.data.distribution = fundamental.generateData(newMean);
  fundamental.data.studyLine = fundamental.sampleFrom(fundamental.data.distribution).x;
  
  // guarantee first observation is far from studyLine and mean
  fundamental.data.distribution[0].x = fundamental.findFarPoint()

  // remove plot and redraw
  d3.select('#fundamental-plot svg').remove()
  fundamental.drawData()
  fundamental.updateXAxis()
  
  // make sure studyLine are displayed
  d3.selectAll(".trueMeanLine, .trueMeanLineLabel")
    .style('display', null)
}

// update the x scale based on user input
fundamental.updateXAxis = function(){
  const range = d3.extent(fundamental.data.distribution, d => +d.x)
  const maximumValue = range[1]
  const minimumValue = range[0]
  fundamental.data.max = maximumValue
  fundamental.data.min = minimumValue
  const animationDuration = 2000

  // new scale and axis
  let padding = (maximumValue - minimumValue) * 0.15
  let newXScale = d3.scaleLinear()
    .domain([minimumValue - padding, maximumValue + padding])
    .range([0, fundamental.config.bodyWidth])
  let newAxis = d3.axisBottom(newXScale)

  // store for use in KDE
  fundamental.scales.xScale = newXScale

  // animate to new axis
  d3.select('.xAxis')
    .transition()
    .duration(animationDuration)
    // .ease(d3.easeLinear)
    .call(newAxis)
  
  // animate rug
  d3.selectAll('.rugLines')
    .transition()
    .duration(animationDuration)
    // .ease(d3.easeLinear)
    .attr('x1', d => newXScale(d.x))
    .attr('x2', d => newXScale(d.x))
  d3.selectAll('.trueMeanLine')
    .transition()
    .duration(animationDuration)
    // .ease(d3.easeLinear)
    .attr('x1', newXScale(fundamental.data.trueMean))
    .attr('x2', newXScale(fundamental.data.trueMean))
  d3.selectAll('.studyLine')
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
    fundamental.data.distribution = fundamental.generateData(fundamental.data.trueMean);
    fundamental.data.studyLine = fundamental.sampleFrom(fundamental.data.distribution).x;
    fundamental.data.distribution[0].x = fundamental.findFarPoint()
    fundamental.data.max = d3.max(fundamental.data.distribution, d => +d.x);
    fundamental.data.min = d3.min(fundamental.data.distribution, d => +d.x);

    // initialize plot
    fundamental.drawData();
}
