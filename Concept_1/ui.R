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
        
        # load custom JavaScript
        tags$script(src = "navSlideOver.js"),
        
        # set top left title
        titlePanel(
            title = h2("thinkCausal"),
            windowTitle = "thinkCausal"
        ),
        
        # div for help slide over
        tags$div(id = 'mySideNav',
                 class = 'conceptsSideBar',
                 tags$div(class = 'conceptsSideBarContainer',
                     HTML('<a href="javascript:void(0)" class="closebtn" onclick="closeNav()">&times;</a>'),
                     includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))
                 )
                ),
        
        # set main navigation
        navbarPage(
            id = "nav",
            title = HTML('<span style="font-size:20px;cursor:pointer" onclick="openNav()">&#9776;</span>'), #'thinkCausal',
            welcome_dropdown,
            analysis_dropdown, 
            concepts_dropdown,
            log_dropdown, 
            help_dropdown
            )
        )
)
