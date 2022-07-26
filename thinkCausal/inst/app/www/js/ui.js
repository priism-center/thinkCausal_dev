
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

  // remove attribute from fullscreen toggle that forces link to open in new tab
  $(".navbar-nav .nav-link").removeAttr('href');

  // replace slideover icon
  $("#controlbar-toggle > i").removeClass('fa-th').addClass('fa-question');
});

// make {page} active
function go_to_shiny_page(page) {
  document.body.scrollTop = document.documentElement.scrollTop = 0;
  setTimeout(function(){Shiny.setInputValue("js_open_page", page, {priority: "event"}); }, 400);
}
