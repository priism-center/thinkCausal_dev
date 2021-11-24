shinyUI(
    fluidPage(
        
        # use shinyjs
        shinyjs::useShinyjs(),
        
        # download roboto font
        tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic"),
        
        # make all links open in a new tab
        tags$head(tags$base(target = "_blank")),
        
        # load custom CSS files
        map(list.files('www/css'), function(file) includeCSS(file.path('www', 'css', file))),

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
        tags$script(src = "js/helpSlideOver.js"),
        
        # add beta ribbon
        tags$div(class = 'cornerRibbon', 'BETA'),
        
        # add header and footer elements
        tags$footer('New York University'),
        tags$header()
    )
)
