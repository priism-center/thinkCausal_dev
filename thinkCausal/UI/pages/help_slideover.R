help_slideover <- tagList(
  
  # floating button to activate help slide over
  tags$div(id = 'sideBarBtn',
           class = 'sideBarBtn',
           onclick = "openHelp()",
           'Help'),
  
  # exit div for help slide over
  tags$div(
    id = 'mySideBarExit',
    class = 'sideBarExit',
    onclick = 'closeHelp()'
  ),
  
  # main div for help slide over
  tags$div(id = 'mySideBar',
           class = 'helpSideBar',
           tags$div(
             id = 'mySideBarContainer',
             class = 'helpSideBarContainer',
             tags$div(
               class = 'helpHeader',
               h1("Help"),
               HTML('<a style="cursor:pointer" class="closebtn" onclick="closeHelp()">&times;</a>')
             ),
             tags$div(
               class = 'markdownContainer',
               includeMarkdown(file.path("UI", "markdowns", 'help.md'))
             )
           )
  ),
)
