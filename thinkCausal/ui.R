shinyUI(
    fluidPage(
        # download roboto font
        HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
        
        # use shinyjs
        shinyjs::useShinyjs(),
        
        # load custom CSS file
        includeCSS("www/thinkCausal.css"),
        includeCSS("www/slider.css"),

        # floating button to activate slide over
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
        
        # div for help slide over
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
        
        # set main navigation
        tags$div(
            class = "wrapper",
            navbarPage(
                id = "nav",
                title = 'thinkCausal',
                welcome_header,
                concepts_header,
                analysis_header,
                reproducibility_header,
                settings_header
                )
        ),

        # load custom JavaScript
        tags$script(src = "navSlideOver.js")
    )
)
