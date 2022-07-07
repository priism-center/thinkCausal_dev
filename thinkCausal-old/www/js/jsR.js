// this are functions that communicate between Shiny and JavaScript

// make {page} active
function go_to_shiny_page(page) {
  closeHelp()
  document.body.scrollTop = document.documentElement.scrollTop = 0
  
  setTimeout(function(){Shiny.setInputValue("js_open_page", page, {priority: "event"}); }, 400);
}


Shiny.addCustomMessageHandler('previous-analysis-page', function(page) {
  page
});