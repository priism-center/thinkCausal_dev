$( document ).ready(function() {
   // remove dark mode switch
  $(".navbar-nav > .custom-switch").remove()

  // replace slideover icon
  $("#controlbar-toggle > i").removeClass('fa-th').addClass('fa-question')
});
