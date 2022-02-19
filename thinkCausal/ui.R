shinyUI(
    fluidPage(
        
        # use shinyjs
        shinyjs::useShinyjs(),
        
        # download roboto font
        tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic"),
        
        # make all links open in a new tab
        tags$head(tags$base(target = "_blank")),
        
        # load custom CSS files
        map(list.files('www/css', pattern = "*.css"), function(file) includeCSS(file.path('www', 'css', file))),

        # set main navigation
        tags$div(
            class = "wrapper",
            navbarPage(
                id = "nav",
                title = tags$div(class = 'logo', img(src='img/logo/thinkCausal_logo_pink.png')), #'thinkCausal',
                welcome_header,
                concepts_header,
                analysis_header,
                reproducibility_header,
                settings_header
                ),
            br(),br(),br()
        ),
        
        # show disconnect message
        shinydisconnect::disconnectMessage(
            text = 'Ugh, an error occured. Please refresh the page. If this occurs again please report it via the feedback button.'
        ),
        
        # add help slideover
        help_slideover,
        
        # add link to feedback form
        tags$div(class = 'feedback-button',
                 onclick = "window.open(
                 'https://docs.google.com/forms/d/e/1FAIpQLSd7dZjpw4FtoVAFUmovNOgKeW-kxnJrs3zV2r3lJ8kvhdq8lA/viewform?usp=sf_link',
                 '_blank')",
                 'Feedback'
        ),

        # add beta ribbon
        tags$div(class = 'cornerRibbon', 'BETA'),
        
        # add header and footer elements
        tags$header(tags$title('thinkCausal')),
        tags$footer(
            # link to priism
            HTML('<a href="https://steinhardt.nyu.edu/priism">New York University</a>'),
            
            # load custom JavaScript files
            map(list.files('www/js', pattern = "*.js"), function(file) tags$script(src = file.path('js', file)))
        )  
    )
)
