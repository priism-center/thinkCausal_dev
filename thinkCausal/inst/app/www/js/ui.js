
$( window ).on( "load", function() {

  // wrap every h3 header in the help markdown in a div
  $("#help-slideover h3").wrap("<div class='helpSubHeader'></div>");

  // add id to every h3 based on its title
  // prefix with namespace "help-"
  $('.helpSubHeader').each(function(){
    id = $(this).text();
    id = id.replace(/\s/g, '');
    id = id.toLowerCase();
    id = "help-" + id;
    $(this).attr('id', id);
  });

  // replace slideover icon
  $("#controlbar-toggle > i").removeClass('fa-th').addClass('fa-question');

  // remove attribute from fullscreen toggle that forces link to open in new tab
  $(".navbar-nav .nav-link").removeAttr('href');
});

// make {page} active
function go_to_shiny_page(page, toggleHelp) {
  document.body.scrollTop = document.documentElement.scrollTop = 0;
  setTimeout(function(){Shiny.setInputValue("js_open_page", {page: page, toggleHelp: toggleHelp}, {priority: "event"}); }, 400);
};

// log previous page for return to analysis button
let last_page = "";

function log_page(page){
  last_page = page
};

function show_back_button() {
  $("#back_to_analysis").show()
};

function hide_back_button() {
  $("#back_to_analysis").hide()
};

// popup to prevent user from accidentally leaving the page if closing the tab or using the browser back button
window.onbeforeunload = function() { return "Please use the navigation buttons on the page."; };
