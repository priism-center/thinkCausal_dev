function getConfigTree(){
  let width = 500;
  let height = 400;
  let margin = {
      top: 30,
      bottom: 80,
      left: 60,
      right: 20
  }

  //The body is the area that will be occupied by the bars.
  let bodyHeight = height - margin.top - margin.bottom;
  let bodyWidth = width - margin.left - margin.right;

  //The container is the SVG where we will draw the chart
  let container = d3.select("#tree")
    .append("svg")
      .attr("class", "plot")
      .attr("preserveAspectRatio", "xMinYMin meet")
      .attr("viewBox", "0 0 " + width + " " + height)
    .append("g")
      .attr("transform",
            "translate(" + margin.left + "," + margin.top + ")")

  return {width, height, margin, bodyHeight, bodyWidth, container}
}

function drawTree(data, config){
  let {margin, container, bodyHeight, bodyWidth, width, height} = config;
  console.log('Data into drawTree():', data)

  // add vertical line
  container.append("line")
    .attr("x1", 250)
    .attr('y1', 80)
    .attr('x2', 250)
    .attr('y2', 100)
  container.append("line")
    .attr("x1", 140)
    .attr('y1', 160)
    .attr('x2', 140)
    .attr('y2', 100)
  container.append("line")
    .attr("x1", 360)
    .attr('y1', 180)
    .attr('x2', 360)
    .attr('y2', 100)
  container.append("line")
    .attr("x1", 360 - 90)
    .attr('y1', 180 + 20)
    .attr('x2', 360 - 90)
    .attr('y2', 100 + 80)
  container.append("line")
    .attr("x1", 360 + 90)
    .attr('y1', 180 + 20)
    .attr('x2', 360 + 90)
    .attr('y2', 100 + 80)

  // add horizontal lines
  container.append("line")
    .attr("x1", 140)
    .attr('y1', 100)
    .attr('x2', 360)
    .attr('y2', 100)
  container.append("line")
    .attr("x1", 360-90)
    .attr('y1', 180)
    .attr('x2', 360+90)
    .attr('y2', 180)

  // add boxes
  container.append('rect')
    .attr('x', 200)
    .attr('y', 40)
    .attr('width', 100)
    .attr('height', 40)
  container.append('rect')
    .attr('x', 90)
    .attr('y', 120)
    .attr('width', 100)
    .attr('height', 40)
  container.append('rect')
    .attr('x', 310)
    .attr('y', 120)
    .attr('width', 100)
    .attr('height', 40)
  container.append('rect')
    .attr('x', 310 - 90)
    .attr('y', 120 + 80)
    .attr('width', 100)
    .attr('height', 40)
  container.append('rect')
    .attr('x', 310 + 90)
    .attr('y', 120 + 80)
    .attr('width', 100)
    .attr('height', 40)
}

function buildTree(data){
  config = getConfigTree()
  drawTree(data, config)
}
