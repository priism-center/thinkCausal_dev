function openNav() {
  document.getElementById("mySideBar").style.width = "min(100%, 700px)";
}

// maybe replace with this https://css-tricks.com/sticky-smooth-active-nav/
function openNavPage(divID) {
  document.getElementById("mySideBar").style.width = "min(100%, 700px)";
  let elmnt = document.getElementById(divID);
  elmnt.scrollIntoView(true);
}

function closeNav() {
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
