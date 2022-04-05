estimands.buildTable = function(data, selector, id){

    console.log("Data into buildTable():", data)
  
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
    let table = d3.select(selector).append('table').attr('class', 'estimands-table').attr('id', id)
    let tcol = table.append('colgroup')
    let thead = table.append('thead')
    let tbody = table.append('tbody')
  
    // create the col group
    tcol
        .selectAll('colgroup')
        .data(columnNames)
        .enter()
        .append('col')
        .style('width', 1/6 + "%") // column width
        // .attr('class', d => 'colWidth_' + d)
  
    // create the table head
    thead.append('tr')
        .selectAll('th')
        .data(columnNames)
        .enter()
        .append('th')
            .text(d => d)
  
    // create the table rows
    let rows = tbody.selectAll('tr')
        .data(data)
        .enter()
        .append('tr')
        .attr('pairID', d => d.pair_id)
        .attr('treatment', function(d) {
            if (d.treatment === '0') return 'control'
            return 'treatment'
          })
        .on('mouseover', estimands.mouseover) 
        .on('mouseleave', estimands.mouseleave)
  
    // add the text to the cells
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
  