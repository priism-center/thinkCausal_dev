function buildTable(data){

  console.log("Data into buildTable():", data)

  // set color scale for conference
  let colorScale = d3.scaleOrdinal()
      .domain(["Eastern", "Western"])
      .range(["#183b32", "#80b0a4"])

  // filter to latest date
  filteredData = data.filter(d => {return d.latest == 1})

  // create rankings
  rankedData =  filteredData.sort((a, b) => d3.descending(a.rating, b.rating))

  // add rank column and clean up rating
  for (var i=0; i<rankedData.length; i++){
    rankedData[i]['rank'] = i+1
    rankedData[i]['rating_string'] = parseFloat(rankedData[i]['rating']).toFixed(0)
  }

  // columns to include in table
  columns = ['rank', 'team', 'rating_string']
  columnNames = ['Rank', 'Team', 'Rating']
  //columns = ['rank1', 'team1', 'rating_string1', 'empty', 'rank2', 'team2', 'rating_string2']
  //columnNames = ['Rank', 'Team', 'Rating', "", 'Rank', 'Team', 'Rating']

  // create table
  let table = d3.select('#tableRank').append('table')
    let tcol = table.append('colgroup')
    let thead = table.append('thead')
    let tbody = table.append('tbody')

    // create the col group
    tcol
      .selectAll('colgroup')
      .data(columnNames)
      .enter()
      .append('col')
      .attr('class', d => 'colWidth_' + d)

    // create the table head
    thead.append('tr')
      .selectAll('th')
      .data(columnNames)
      .enter()
    .append('th')
      .text(d => d)

    let rows = tbody.selectAll('tr')
      .data(rankedData)
      .enter()
      .append('tr')
      .attr('team', d => d.team)
      .on("mouseover", function(d, i){
        // highlight this row
        d3.select(this)
          .style("background-color", '#e4ebe7')

        // de-emphasize other points
        d3.selectAll('.currentPoints')
          .style('opacity', 0.2)

        // get team name and change the stroke with all values with this team name
        let name = d3.select(this).attr('team')
        d3.selectAll("[team=" + name + "_path]")
          .transition()
            .duration(150)
            .style('stroke-opacity', 1)
        d3.selectAll("[team=" + name + "_circle]")
          .transition()
            .duration(150)
            .style('opacity', 1)

        // emphasize the current point
        d3.selectAll("[teamShape=" + name + "_currentCircle]")
          .style('opacity', 1)
          .style('fill', '#394E48')
      })
      .on("mouseleave", function(d, i){
        // de-highlight this row
        d3.select(this)
          .style("background-color", '#fff')

        // re-emphasize other points
        d3.selectAll('.currentPoints')
          .style('opacity', 0.8)
          //.style('fill', '#71807b')
          .style('fill', d => colorScale(d.conference))

        // get team name and change the stroke with all values with this team name
        let name = d3.select(this).attr('team')
        d3.selectAll("[team=" + name + "_path]")
          .transition()
            .duration(200)
            .style('stroke-opacity', 0)
        d3.selectAll("[team=" + name + "_circle]")
          .transition()
            .duration(200)
            .style('opacity', 0)
    })

    let cells = rows.selectAll('td')
      .data(function(row){
        return columns.map(function(column){
          return { column: column, value: row[column]}
        })
      })
      .enter()
    .append('td')
      .text(d => d.value)

}
