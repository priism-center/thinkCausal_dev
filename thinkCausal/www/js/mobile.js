// hide the logo, feedback, header, and footer if on mobile
function show_hide_mobile(){
  let window_width = $(window).width();
  let logo = $(".logo")
  let feedback = $(".feedback-button")
  let header = $("header")
  let footer = $("footer")
  let elements = [logo, feedback, header, footer]
  
  if (window_width < 768) {
    elements.forEach(el => el.hide())
  } else {
    elements.forEach(el => el.show())
  }
}
show_hide_mobile()
$(window).resize(function() {
  show_hide_mobile()
})
