// TODO: container should be same as other plot so should not recreate it but do need new height
bart.animation.getConfig = function(selector) {
    const width = 540; //900px is width of learning article * 0.6
    const height = 600; // set in bart.drawPosteriorPlot()
    const margin = {
        top: 50,
        bottom: 80,
        left: 60,
        right: 20
    }
  
    // the body is the area that will be occupied by the plot
    const bodyHeight = height - margin.top - margin.bottom;
    const bodyWidth = width - margin.left - margin.right;
  
    // the container is the SVG where we will draw the plot
    const container = d3.select(selector)
      .append("svg")
        .attr("class", "plot")
        .attr("preserveAspectRatio", "xMinYMin meet")
        .attr("viewBox", "0 0 " + width + " " + height)
      .append("g")
        .attr("transform",
              `translate(${margin.left},${margin.top})`)
  
    return {width, height, margin, bodyHeight, bodyWidth, container, selector}
  }
  
  bart.animation.getScales = function(data, config) {
    const { bodyWidth, bodyHeight, container } = config;
    const maxX = d3.max(data.observations, d => +d.caloriesConsumed);
    const minX = d3.min(data.observations, d => +d.caloriesConsumed);
    const maxY = d3.max(data.observations, d => +d.runningTime);
    const minY = d3.min(data.observations, d => +d.runningTime);
    const padding = (maxX - minX) * 0.075
  
    const xScale = d3.scaleLinear()
      .domain([minX - padding, maxX + padding])
      .range([0, bodyWidth])
    const yScale = d3.scaleLinear()
      .domain([minY - padding, maxY + padding]) 
      .range([bodyHeight, 0])
  
    const colorScale = d3.scaleOrdinal()
      .domain(["0", "1"])
      .range(["#21918c", "#440154"])
  
   return {xScale, yScale, colorScale}
  }
  