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

// open the help slide over to a specific header
function openHelpPage(divID) {
  let mySideBar = document.getElementById('mySideBar')
  mySideBar.style.width = "min(100%, 700px)";
  document.getElementById("mySideBarExit").style.width = "100%";
  let elmnt = document.getElementById(divID);
  
  // get the p after the div since the elmnt's sticky property makes it tricky to get the elmnt's position
  let elmnt_next = elmnt.nextElementSibling
  
  // scroll to that position
  let yOffset = $('.helpHeader').height() + 15 + elmnt.offsetHeight
  let y = elmnt_next.offsetTop + window.pageYOffset - yOffset;
  setTimeout(function(){mySideBar.scrollTo({top: y, behavior: 'smooth'}); }, 600);
}

function closeHelp() {
  document.getElementById("mySideBar").style.width = "0";
  document.getElementById("mySideBarExit").style.width = "0";
}

// wrap every h3 header in the help markdown in a div
$(".markdownContainer h3").wrap("<div class='helpSubHeader'></div>");

// add id to every h3 based on its title
// prefix with namespace "help-"
$('.helpSubHeader').each(function(){
  id = $(this).text()
  id = id.replace(/\s/g, '')
  id = id.toLowerCase()
  id = "help-" + id
  $(this).attr('id', id)
})

// set width markdown container on page load and when window is resized
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

// make [page] active
function go_to_shiny_page(page) {
  closeHelp()
  document.body.scrollTop = document.documentElement.scrollTop = 0
  
  Shiny.setInputValue("js_open_page", page, {priority: "event"})
}