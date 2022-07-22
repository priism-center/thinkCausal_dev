// functions to manage control the scrollytell. See fct_scrollytell.R

scrolly = {}
scrolly.emphasizeText = function(moduleId, index){
  // de-emphasize this text
  $(`.${moduleId}-scroll-text-section`).css('filter', 'opacity(0.2)')

  // emphasize this text
  $(`#${moduleId}-text-${index+1}-scroll-text-section`).css('filter', 'none')

}
scrolly.scroll = function(moduleId){

  // get the positions of section divs relative to the top of the viewport
  let positions = $(`.${moduleId}-scroll-text-section`).map(function() {
    return $(this)[0].getBoundingClientRect().top
  }).toArray();

  // make off page elements positive
  positions = positions.map(Math.abs);

  // get smallest value
  const minVal = Math.min(...positions);
  const index = positions.indexOf(minVal);

  // show/hide visual if state changed
  if (index != window.scrolly[`plotState${moduleId}`]){
    $(`#${moduleId}-scroll_visual > *`).css('visibility', 'hidden').hide();
    $(`#${moduleId}-scroll_visual > :nth-child(${index+1})`).css('visibility', 'visible').show();
    scrolly.emphasizeText(moduleId, index);
    window.scrolly[`plotState${moduleId}`] = index;
  }
}
