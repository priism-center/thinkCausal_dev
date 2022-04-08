

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

  return {width, height, margin, bodyHeight, bodyWidth, container}
}

fundamental.getScales = function(data, config) {
 let { bodyWidth, bodyHeight, container } = config;
 let maximumValue = 250 //d3.max(data.distribution, d => +d.x);
 let minimumValue = -250 //d3.min(data.distribution, d => +d.x);

 let xScale = d3.scaleLinear()
    .domain([minimumValue, maximumValue])
    .range([0, bodyWidth])
 let yScale = d3.scaleLinear()
    .domain([0, 0.015]) //1.5])
    .range([bodyHeight, 0])

  fundamental.scales = {xScale, yScale}

 return {xScale, yScale}
}

fundamental.drawRug = function(data, scales, config){
  let {margin, container, bodyHeight, bodyWidth} = config;
  let {xScale, yScale} = scales
  let rugHeight = 0.003
  console.log('Data into fundamental.drawRug():', data)

  let meanY = d3.mean(data.distribution, d => +d.x);
  let meanX = d3.mean(data.distribution, d => +d.index);
  let minY = d3.min(data.distribution, d => +d.x);
  let maxY = d3.max(data.distribution, d => +d.x);
  let minX = d3.min(data.distribution, d => +d.index);
  let maxX = d3.max(data.distribution, d => +d.index);

  let strokeColor = '#6e6e6e'
  let strokeWidth = 2.5
  let pointOpacity = 0.8
  let pointRadius = 7



  // add X axis
  let xAxis = d3.axisBottom(xScale)
  // xAxis.ticks(3);
  // xAxis.tickValues([0, 1]);
  // let tickLabels = ['y0', 'y1']
  // xAxis.tickFormat((d, i) => tickLabels[i])
  container.append("g")
    .attr('class', "axis xAxis")
    .attr("transform", "translate(0," + bodyHeight + ")")
    .call(xAxis);
  container.append('text')
    .attr('class', 'axisLabel')
    .attr("x", bodyWidth/2)
    .attr('y', bodyHeight + margin.bottom/2)
    .attr('text-anchor', 'middle')
    .text("Running time")

  // add Y axis
  // container.append("g")
  //   .attr('class', "axis yAxis")
  //   .call(d3.axisLeft(yScale));
  // container.append('text')
  //   .attr('class', 'axisLabel yAxisLabel')
  //   .attr('x', -margin.left-10)
  //   .attr('y', yScale(meanY))
  //   .attr('text-anchor', 'middle')
  //   .attr("transform", "rotate(-90,-" + (margin.left-10) + "," + yScale(meanY) + ")")
  //   .text("Running time")

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
    .style('stroke', '#525252')
    .style('stroke-width', 3)
    .style('display', 'none')
  container.append('text')
    .attr('class', 'trueMeanLineLabel')
    .attr('x', xScale(+fundamental.data.trueMean + 5))
    .attr('y', yScale(1*0.95))
    .text('True mean')
    .style('display', 'none')

  // add study line
  container.append('line')
    .attr('class', 'studyLine')
    .attr('x1', xScale(fundamental.data.studyLine))
    .attr('y1', yScale(0))
    .attr('x2', xScale(fundamental.data.studyLine))
    .attr('y2', yScale(rugHeight))
    .style('stroke', 'red')
    .style('stroke-width', 3)
    .style('display', 'none')
  container.append('text')
    .attr('class', 'studyLineLabel')
    .attr('x', xScale(+fundamental.data.studyLine + 5))
    .attr('y', yScale(1*0.95))
    .text('Study results')
    .style('display', 'none')
  
  
  // add KDE
  // fundamental.redrawKDE(10)
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
d3.select("#input-distribution-mean").on('input', function() {
  
  // update true mean value
  newMean = +$("#input-distribution-mean")[0].value
  fundamental.data.trueMean = newMean

  // generate new distribution and study based on input
  fundamental.data.distribution = fundamental.generateData(newMean);
  fundamental.data.studyLine = fundamental.sampleFrom(fundamental.data.distribution).x;

  // remove plot and redraw
  d3.select('#fundamental-plot svg').remove()
  fundamental.drawData()
  
  // make sure studyLine are displayed
  d3.selectAll(".trueMeanLine, .trueMeanLineLabel")
    .style('display', null)
})

// // update plot when user hits get study results
// d3.select("#generate-study").on('click', function() {

//   // generate new distribution and study based on input
//   newMean = +$("#input-distribution-mean")[0].value
//   fundamental.data.trueMean = newMean
//   fundamental.data.distribution = fundamental.generateData(newMean);
//   fundamental.data.studyLine = fundamental.sampleFrom(fundamental.data.distribution).x;

//   // remove plot and redraw
//   d3.select('#fundamental-plot svg').remove()
//   fundamental.drawData()
  
//   // make sure trueMean and studyLine are displayed
//   d3.selectAll(".trueMeanLine, .studyLine")
//     .style('display', null)
// })

// show rug lines on user input
// d3.select("#generate-distribution").on("click", function() {
//   // make sure trueMean, studyLine, and rugLines are displayed
//   d3.selectAll(".trueMeanLine, .studyLine, .rugLines")
//     .style('display', null)
// })

// intialize plot
fundamental.showData = function() {
    // initialize values
    fundamental.data = {}
    fundamental.data.trueMean = 0;
    fundamental.data.distribution = fundamental.generateData(fundamental.data.trueMean);
    fundamental.data.studyLine = fundamental.sampleFrom(fundamental.data.distribution).x;

    // initialize plot
    fundamental.drawData();
    // fundamental.drawHistogram
}
