estimands.buildTable = function(data){

    console.log("Data into buildTable():", data)
  
    // // set color scale for conference
    // let colorScale = d3.scaleOrdinal()
    //     .domain(["Eastern", "Western"])
    //     .range(["#183b32", "#80b0a4"])
  
    // // filter to latest date
    // filteredData = data.filter(d => {return d.latest == 1})
  
    // // create rankings
    // rankedData =  filteredData.sort((a, b) => d3.descending(a.rating, b.rating))
  
    // // add rank column and clean up rating
    // for (var i=0; i<rankedData.length; i++){
    //   rankedData[i]['rank'] = i+1
    //   rankedData[i]['rating_string'] = parseFloat(rankedData[i]['rating']).toFixed(0)
    // }
  
    // columns to include in table
    columns = ['pair_id', 'yName_y0', 'yName_y1', 'y', 'treatment', 'ICE']
    columnNames = ['Runner', 'y0', 'y1', 'y', 'z', 'ICE']

    // add y and ICE column
    d3.map(data, function(d){
        d.yName_y0 = estimands.roundNumber(+d.yName_y0, 2)
        d.yName_y1 = estimands.roundNumber(+d.yName_y1, 2)
        d.y = d.treatment == '0' ? d.yName_y0 : d.yName_y1
        d.ICE = estimands.roundNumber(d.yName_y1 - d.yName_y0, 2)
    })
  
    // create table
    let table = d3.select('#estimands-plot-ATE').append('table').attr('id', 'estimands-table')
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
        .data(data)
        .enter()
        .append('tr')
        .attr('pairID', d => d.pair_id)
        .on('mouseover', estimands.mouseover) 
        .on('mouseleave', estimands.mouseleave)
  
    let cells = rows.selectAll('td')
        .data(function(row){
            out = columns.map(function(column){
                return { column: column, value: row[column]}
            })
            return out
        })
        .enter()
        .append('td')
            .text(d => d.value)
  
}
  