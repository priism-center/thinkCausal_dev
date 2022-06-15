// scrollytell
overlap.scrollytellState1 = function(){
    let container = overlap.config.container
    // console.log('overlapState1')

    // resets
    container.selectAll('.overlap-lines:not(.overlap-lines-trueFit0, .overlap-lines-trueFit1)').style('display', 'none');
    overlap.verticalLine.style('display', 'none')
    container.select('.overlap-hoverRect').attr('pointer-events', 'none')
    overlap.verticalLine.style('display', 'none')

    // adjust subtitle
    container.select('.overlap-subtitle').text(null)

    // show points
    container.selectAll('.overlap-observations')
        .transition()
        .duration(500)
        .delay(d => Math.random() * 1200)
        .style('opacity', null)

    overlap.emphasizeText("#overlap-trigger-1, #overlap-trigger-1 + p")
}

overlap.scrollytellState2 = function(){
    let container = overlap.config.container
    // console.log('overlapState2')

    // de-emphasize points
    container.selectAll('.overlap-observations').transition().style('opacity', 0.2)

    // emphasize lines
    container.selectAll('.overlap-lines:not(.overlap-lines-trueFit0, .overlap-lines-trueFit1)').style('display', 'none');
    container.selectAll('.overlap-lines-diffFit0, .overlap-lines-diffFit1')
        .style('display', null);

    // update vertical line
    overlap.updatePointerOnScroll(container, 'diff')

    // adjust subtitle
    container.select('.overlap-subtitle').text('Difference in means')

    overlap.emphasizeText("#overlap-trigger-2, #overlap-trigger-2 + p")
}

overlap.scrollytellState3 = function(){
    let container = overlap.config.container
    // console.log('overlapState3')

    // resets
    container.selectAll('.overlap-lines:not(.overlap-lines-trueFit0, .overlap-lines-trueFit1)').style('display', 'none');
    container.selectAll('.overlap-lines-lmFit0, .overlap-lines-lmFit1')
        .style('display', null);

    // de-emphasize points
    container.selectAll('.overlap-observations').style('opacity', 0.2)

    // update vertical line
    overlap.updatePointerOnScroll(container, 'lm')

    // adjust subtitle
    container.select('.overlap-subtitle').text('Linear regression')
        
    overlap.emphasizeText("#overlap-trigger-3, #overlap-trigger-3 + p, #overlap-trigger-3 + p + p")
}

overlap.scrollytellState4 = function(){
    let container = overlap.config.container
    // console.log('overlapState4')

    // resets
    container.selectAll('.overlap-lines:not(.overlap-lines-trueFit0, .overlap-lines-trueFit1)').style('display', 'none');
    container.selectAll('.overlap-lines-treeFit0, .overlap-lines-treeFit1')
        .style('display', null);

    // de-emphasize points
    container.selectAll('.overlap-observations').style('opacity', 0.2)

    // update vertical line
    overlap.updatePointerOnScroll(container, 'tree')

    // adjust subtitle
    container.select('.overlap-subtitle').text('Regression tree')


    overlap.emphasizeText("#overlap-trigger-4, #overlap-trigger-4 + p")
}

overlap.scrollytellState5 = function(){
    let container = overlap.config.container
    // console.log('overlapState5')

    // resets
    container.selectAll('.overlap-lines:not(.overlap-lines-trueFit0, .overlap-lines-trueFit1)').style('display', 'none');
    container.selectAll('.overlap-lines-overlapFit0, .overlap-lines-overlapFit1')
        .style('display', null);

    // de-emphasize points
    container.selectAll('.overlap-observations').style('opacity', 0.2)

    // update vertical line
    overlap.updatePointerOnScroll(container, 'overlap')

    // adjust subtitle
    container.select('.overlap-subtitle').text('Bayesian Additive Regression Trees (overlap)')
        
    overlap.emphasizeText("#overlap-trigger-5, #overlap-trigger-5 + p")
}


overlap.plotState = 1
overlap.triggerScrollytellAnimation = function(){
    // trigger the closest animation

    // get the positions of divs relative to the top of the viewport
    let trigger1Pos = $('#overlap-trigger-1')[0].getBoundingClientRect().top
    let trigger2Pos = $('#overlap-trigger-2')[0].getBoundingClientRect().top
    let trigger3Pos = $('#overlap-trigger-3')[0].getBoundingClientRect().top
    let trigger4Pos = $('#overlap-trigger-4')[0].getBoundingClientRect().top
    let trigger5Pos = $('#overlap-trigger-5')[0].getBoundingClientRect().top
    // let trigger6Pos = $('#overlap-trigger-6')[0].getBoundingClientRect().top
    let positions = [trigger1Pos, trigger2Pos, trigger3Pos, trigger4Pos, trigger5Pos]
    let scrollyFns = [overlap.scrollytellState1, overlap.scrollytellState2, overlap.scrollytellState3, overlap.scrollytellState4, overlap.scrollytellState5]

    // make off page elements positive
    positions = positions.map(Math.abs)

    // get smallest value
    const minVal = Math.min(...positions)
    const index = positions.indexOf(minVal)

    // update plot if state changed
    if (index != overlap.plotState){
        scrollyFns[index]()
        overlap.plotState = index
    }
}

// add listener
document.addEventListener('scroll', overlap.triggerScrollytellAnimation);
