

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
 let maximumValue = d3.max(data.distribution, d => +d.x);
 let minimumValue = d3.min(data.distribution, d => +d.x);

 let xScale = d3.scaleLinear()
    //  .domain([minimumValue, maximumValue])
     .domain([-250, 250])
     .range([0, bodyWidth])
 let yScale = d3.scaleLinear()
      .domain([0, 1.5])
     .range([bodyHeight, 0])

 return {xScale, yScale}
}

fundamental.drawRug = function(data, scales, config){
  let {margin, container, bodyHeight, bodyWidth} = config;
  let {xScale, yScale} = scales
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
      .attr('y1', d => yScale(0))
      .attr('x2', d => xScale(d.x))
      .attr('y2', d => yScale(1))
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
    .attr('y2', yScale(1.5))
    .style('stroke', '#6e6e6e')
    .style('stroke-width', 3)
    // .style('display', 'none')
  container.append('text')
    .attr('class', 'trueMeanLine')
    .attr('x', xScale(+fundamental.data.trueMean + 5))
    .attr('y', yScale(1.5*0.95))
    .text('True mean')

  // add study line
  container.append('line')
    .attr('class', 'studyLine')
    .attr('x1', xScale(fundamental.data.studyLine))
    .attr('y1', yScale(0))
    .attr('x2', xScale(fundamental.data.studyLine))
    .attr('y2', yScale(1.25))
    .style('stroke', 'red')
    .style('stroke-width', 3)
    .style('display', 'none')
  container.append('text')
    .attr('class', 'studyLine')
    .attr('x', xScale(+fundamental.data.studyLine + 5))
    .attr('y', yScale(1.25*0.95))
    .text('Study results')
    .style('display', 'none')
  
  
  // add KDE

}

// fundamental.drawBars = function(data, scales, config){
//   let {margin, container, bodyHeight, bodyWidth} = config;
//   let {xScale, yScale} = scales;
//   let nbins = 30;

//   // set the parameters for the histogram
//   let histogram = d3.histogram()
//         .value(d => d.x)
//         .domain(xScale.domain())
//         .thresholds(xScale.ticks(nbins));

//   // get the binned data
//   let bins = histogram(data.distribution);

//   // remove and redraw X axis
//   d3.selectAll(".bottomaxis").remove()
//   container.append("g")
//     .attr("class", "bottomaxis")
//     .attr("transform", "translate(" + margin.left + "," + bodyHeight + ")")
//     .call(d3.axisBottom(xScale));

//   // remove and redraw Y axis
//   yScale.domain([0, d3.max(bins, d => d.length)]);
//   d3.selectAll(".leftaxis").remove()
//   container.append("g")
//     .attr("class", "leftaxis")
//     .call(d3.axisLeft(yScale))
//     .style("transform",
//       `translate(${margin.left}px, 0px)`
//     )

//   // join data with rect
//   let rects = container
//     .selectAll("rect")
//     .data(bins)

//   // add the new bars
//   rects
//     .enter()
//     .append("rect") // Add a new rect for each new elements
//     .merge(rects) // get the already existing elements as well
//       .attr("x", 1)
//       .attr("transform", function(d) { return "translate(" + (xScale(d.x0) + margin.left) + "," + yScale(d.length) + ")"; })
//       .attr("width", function(d) { return xScale(d.x1) - xScale(d.x0) -1 ; })
//       .attr("height", function(d) { return bodyHeight - yScale(d.length); })
//       .style("fill", "#394E48")

//   // delete the old bars
//   rects
//     .exit()
//     .remove()
//  }

//  d3.select("#generateDistribution").on("click", function() {
//   newMean = +$("#input-distribution-mean")[0].value
//   fundamental.data.studyLine = +$("#input-distribution-mean")[0].value
//  })

// fundamental.drawHistogram = function() {
//   let data = fundamental.data
//   let config = fundamental.getConfig();
//   let scales = fundamental.getScales(data, config);
//   fundamental.drawBars(data, scales, config);
// }

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

  // remove plot and redraw
  d3.select('#fundamental-plot svg').remove()
  fundamental.drawData()
  
  // make sure studyLine are displayed
  d3.selectAll(".trueMeanLine")
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
    let newMean = 100

    // simulate initial data
    fundamental.data.distribution = fundamental.generateData(newMean)

    // initialize plot
    fundamental.drawData();
    // fundamental.drawHistogram
}