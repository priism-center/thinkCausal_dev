// open/closes the help sidebar
function openHelp() {
  let sideDiv = document.getElementById("mySideBar")
  let sideDivExit = document.getElementById("mySideBarExit")
  if (sideDiv.style.width === 'min(100%, 700px)'){
    sideDiv.style.width = '0';
    sideDivExit.style.width = '0';
  } else {
    sideDiv.style.width = "min(100%, 700px)"
    sideDivExit.style.width = "100%"
  }
}

// maybe replace with this https://css-tricks.com/sticky-smooth-active-nav/
function openHelpPage(divID) {
  document.getElementById("mySideBar").style.width = "min(100%, 700px)";
  document.getElementById("mySideBarExit").style.width = "100%";
  let elmnt = document.getElementById(divID);
  setTimeout(function(){elmnt.scrollIntoView(true);}, 600);
}

function closeHelp() {
  document.getElementById("mySideBar").style.width = "0";
  document.getElementById("mySideBarExit").style.width = "0";
}

// wrap every h3 header in the help markdown in a div
$(".markdownContainer h3").wrap("<div class='helpSubHeader'></div>");

// add id to every h3 based on its title
$('.helpSubHeader').each(function(){
  id = $(this).text()
  id = id.replace(/\s/g, '')
  $(this).attr('id', id)
})

// set width markdowon container on page load and when window is resized
// this prevents the text from changing width when opening the help slideover 
// and allows the help buttons to open to the right spot
function resize_container(){
  let window_width = $(window).width();
  let x_width = Math.min(window_width, 700) + "px";
  document.getElementById("mySideBarContainer").style.width = x_width;
}
resize_container()
$(window).resize(function() {
  resize_container()
})
