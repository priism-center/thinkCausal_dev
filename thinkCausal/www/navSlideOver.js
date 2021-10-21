// open/closes the concepts sidebar
function openConcepts() {
  let sideDiv = document.getElementById("mySideBar")
  if (sideDiv.style.width === 'min(100%, 700px)'){
    sideDiv.style.width = '0';
  } else {
    sideDiv.style.width = "min(100%, 700px)"
  }
}

// maybe replace with this https://css-tricks.com/sticky-smooth-active-nav/
function openConceptsPage(divID) {
  document.getElementById("mySideBar").style.width = "min(100%, 700px)";
  let elmnt = document.getElementById(divID);
  setTimeout(function(){elmnt.scrollIntoView(true);}, 600);
  //let topPos = document.getElementById('Concept2').offsetTop
  //setTimeout(function(){document.getElementById("conceptsSideBarContainer").scrollTop = topPos;}, 500);
}

function closeConcepts() {
  document.getElementById("mySideBar").style.width = "0";
}

// wrap every h3 header in the concepts markdown in a div
$(".markdownContainer h3").wrap("<div class='conceptsSubHeader'></div>");

// add id to every h3 based on its title
$('.conceptsSubHeader').each(function(){
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
  document.getElementById("myMarkdown").style.width = x_width;
}
resize_container()
$(window).resize(function() {
  resize_container()
})
