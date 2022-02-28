// hide the logo, feedback, header, and footer if on mobile
function showHideMobile(){
  let window_width = $(window).width();
  let logo = $(".logo")
  let feedback = $(".feedback-button")
  let back = $(".back-to-analysis-button")
  let header = $("header")
  let footer = $("footer")
  let elements = [logo, feedback, back, header, footer]
  
  if (window_width < 768) {
    elements.forEach(el => el.hide())
  } else {
    elements.forEach(el => el.show())
  }
}
showHideMobile()
$(window).resize(showHideMobile)

// send mobile TRUE/FALSE to shiny server
function isOnMobile(){
 let isMobile = +$(window).width() < 768 //window.matchMedia("only screen and (max-width: 767px)").matches 
 Shiny.setInputValue("js_is_on_mobile", isMobile, {priority: "event"});
}
isOnMobile()
$(window).resize(isOnMobile)
