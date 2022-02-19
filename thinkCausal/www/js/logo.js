// hide the logo if on mobile
function show_hide_logo(){
  let window_width = $(window).width();
  let logo = $(".logo")
  let header = $("header")
  if (window_width < 768) {
    logo.hide(); 
    header.hide();
  } else {
    logo.show(); 
    header.show();
  }
}
show_hide_logo()
$(window).resize(function() {
  show_hide_logo()
})
