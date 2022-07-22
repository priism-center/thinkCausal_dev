
test = {}
test.plotState = 1
test.emphasizeText = function(selectors){
  // de-emphasize this text
  d3.selectAll(".scroll-text-section")
      .style('filter', 'opacity(0.2)')
  // emphasize this text
  d3.selectAll(selectors)
      .style('filter', null)
}
test.triggerScrollytellAnimation = function(){
    // trigger the closest animation

    // get the positions of divs relative to the top of the viewport
    let trigger1Pos = $('#scroll-text-section-test-text-1')[0].getBoundingClientRect().top
    let trigger2Pos = $('#scroll-text-section-test-text-2')[0].getBoundingClientRect().top
    let trigger3Pos = $('#scroll-text-section-test-text-3')[0].getBoundingClientRect().top
    let positions = [trigger1Pos, trigger2Pos, trigger3Pos]

    // make off page elements positive
    positions = positions.map(Math.abs)

    // get smallest value
    const minVal = Math.min(...positions)
    const index = positions.indexOf(minVal)

    // update plot if state changed
    if (index != test.plotState){
        Shiny.setInputValue("test-scroll_index", index+1, {priority: "event"})
        test.emphasizeText('#scroll-text-section-test-text-' + (index+1))
        test.plotState = index
    }
}

// add listener
document.addEventListener('scroll', test.triggerScrollytellAnimation);
