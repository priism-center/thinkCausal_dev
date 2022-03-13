function buildGrid(data){

  // split data into each unique date
  // these dates should already be sorted in R or Python
  uniqueDates = d3.map(data, d => d.date).keys()

  // for each date, create a grid
  for (var i=0; i<uniqueDates.length; i++){

    // get date and filter the data to that date
    thisDate = uniqueDates[i]
    filteredData = data.filter(d => {return d.date == thisDate})

    // sort list by win probability
    filteredData = filteredData.sort((a, b) => d3.ascending(Math.abs(0.5 - a.probA), Math.abs(0.5 - b.probA)))

    // add new div
    let new_div = $("<div>").appendTo('#gridContainer')
    new_div.attr('class', 'subGridContainer')

    // add date title
    dateFormat = {weekday: 'long', month: 'long', day: 'numeric'}
    formattedDate = new Date(uniqueDates[i]).toLocaleDateString('en', dateFormat)
    let h_div = $("<div>").appendTo(new_div)
    h_div.attr('class', 'subGridContainerHeaderDiv')
    $("<h3>" + formattedDate + "</h3>").appendTo(h_div) //('#gridContainer')

    // create list
    let new_ul = $('<ul>').appendTo(new_div) //'#gridContainer')
    new_ul.attr('id', 'newUl'+i)

    // for each game, create a grid item
    for (var j=0; j<filteredData.length; j++){

      // get team names and probabilities
      teamA = filteredData[j].teamA
      teamB = filteredData[j].teamB
      teamAprob = filteredData[j].probA
      teamBprob = filteredData[j].probB
      teamAprob = parseFloat(teamAprob*100).toFixed(1)+"%"
      teamBprob = parseFloat(teamBprob*100).toFixed(1)+"%"

      // create new list item
      let new_li = $('<li>').appendTo('#newUl'+i)
      new_li.append('<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmo</p>')
      let new_ul_inner = new_li.append('<ul>')
      new_ul_inner.append('<li><div class="teamBox"><p class="alignleft">' + teamA + '</p><p class="alignright">' + teamAprob + '</p></div><div style="clear: both;"></div></li>')
      new_ul_inner.append('<li><hr></li>')
      new_ul_inner.append('<li><div class="teamBox"><p class="alignleft">' + teamB + '</p><p class="alignright">' + teamBprob + '</p></div><div style="clear: both;"></div></li>')
      new_ul_inner.append('</ul>')
    }
  }
}
