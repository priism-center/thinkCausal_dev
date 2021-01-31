shinyUI(
    fluidPage(
        # download roboto font
        HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
        
        # install jquery
        HTML('<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>'),
        
        # set default slider skin
        chooseSliderSkin(skin = "Flat", color = "#221146"),
        
        # initiate shinyjs
        # useShinyjs(),
        
        # load custom CSS file
        includeCSS("www/custom_css.css"),
        
        # set top left title
        titlePanel(
            title = h2("thinkCausal"),
            windowTitle = "thinkCausal"
        ),
        
        # div for help slide over
        tags$div(id = 'mySideBar',
                 class = 'conceptsSideBar',
                 tags$div(class = 'conceptsSideBarContainer',
                     tags$div(
                         class = 'conceptsHeader',
                         h1("Concepts"),
                         HTML('<a href="javascript:void(0)" class="closebtn" onclick="closeNav()">&times;</a>')
                     ),
                     tags$div(
                         class = 'markdownContainer',
                         includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))
                     )
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
            ),
        
        # load custom JavaScript
        tags$script(src = "navSlideOver.js")
        )
)
