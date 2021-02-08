shinyUI(
    fluidPage(
        # download roboto font
        HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
        
        # set default slider skin
        chooseSliderSkin(skin = "Flat", color = "#221146"),
        
        # initiate shinyjs
        # useShinyjs(),
        
        # load custom CSS file
        includeCSS("www/custom_css.css"),
        
        # set top left title
        # titlePanel(
        #     title = h2("thinkCausal"),
        #     windowTitle = "thinkCausal"
        # ),
        
        # floating button to activate slide over
        tags$div(id = 'sideBarBtn',
                 class = 'sideBarBtn',
                 onclick = "openConcepts()",
                 'Help'),
        
        # div for help slide over
        tags$div(id = 'mySideBar',
                 class = 'conceptsSideBar',
                 tags$div(class = 'conceptsSideBarContainer',
                     tags$div(
                         class = 'conceptsHeader',
                         h1("Help"),
                         HTML('<a style="cursor:pointer" class="closebtn" onclick="closeConcepts()">&times;</a>')
                     ),
                     tags$div(
                         class = 'markdownContainer',
                         includeMarkdown(file.path("UI", "markdowns", 'help.md'))
                     )
                 )
                ),
        
        # set main navigation
        navbarPage(
            id = "nav",
            title = 'thinkCausal',
            welcome_dropdown,
            analysis_dropdown, 
            concepts_dropdown,
            log_dropdown
            # help_dropdown
            ),

        # load custom JavaScript
        tags$script(src = "navSlideOver.js")
        )
)
