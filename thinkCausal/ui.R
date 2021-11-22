shinyUI(
    fluidPage(
        
        # download roboto font
        HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
        
        # use shinyjs
        shinyjs::useShinyjs(),
        
        # load custom CSS files
        includeCSS("www/thinkCausal.css"),
        includeCSS("www/helpSlideOver.css"),
        includeCSS("www/slider.css"),
        includeCSS("www/sortable.css"),
        includeCSS("www/information_icon.css"),
        
        # add help slideover
        help_slideover,

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
        tags$script(src = "helpSlideOver.js")
    )
)
