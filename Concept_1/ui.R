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
        #     title = h1("thinkCausal"),
        #     windowTitle = "thinkCausal"
        # ),
        
        # set main navigation
        navbarPage(
            id = "nav",
            title = 'thinkCausal',
            welcome_dropdown,
            analysis_dropdown, 
            concepts_dropdown,
            log_dropdown, 
            help_dropdown
            )
        )
)
