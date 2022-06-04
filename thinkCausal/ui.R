shinyUI(
    fluidPage(
      
        # use shinyjs
        shinyjs::useShinyjs(),
        
        # download roboto font
        tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic"),
        
        # load custom CSS files
        map(list.files('www/css', pattern = "*.css"), function(file) includeCSS(file.path('www', 'css', file))),

        # set main navigation
        suppressWarnings(
            tags$div(
                class = "wrapper",
                navbarPage(
                    id = "nav",
                    title = tags$div(class = 'logo', 
                                     img(src='img/logo/thinkCausal_logo_mint.png'),
                                     onclick="go_to_shiny_page('Welcome')"),
                    welcome_header,
                    concepts_header,
                    analysis_header,
                    reproducibility_header,
                    settings_header
                ),
                br(),br(),br()
            )
        ),
        
        # show disconnect message
        shinydisconnect::disconnectMessage(
            text = 'Ugh, an error occured. Please refresh the page. If this occurs again please report it via the feedback button.'
        ),
        
        # add help slideover
        help_slideover,
        
        # add link to feedback form
        # tags$div(class = 'feedback-button',
        #          onclick = "window.open(
        #          'https://docs.google.com/forms/d/e/1FAIpQLSd7dZjpw4FtoVAFUmovNOgKeW-kxnJrs3zV2r3lJ8kvhdq8lA/viewform?usp=sf_link',
        #          '_blank')",
        #          'Feedback'
        # ),
        
        # add 'back to analysis' button
        # tags$div(class = 'back-to-analysis-button',
        #          actionButton(inputId = 'back_to_analysis_button',
        #                       label = 'Back')
        #          ),

        # add beta ribbon
        tags$div(
          class = 'cornerRibbon',
          'BETA',
          tags$div(
            tags$a(
              href = 'https://docs.google.com/forms/d/e/1FAIpQLSd7dZjpw4FtoVAFUmovNOgKeW-kxnJrs3zV2r3lJ8kvhdq8lA/viewform?usp=sf_link',
              target = "_blank",
              'Have feedback?'
            )
          )
        ), 
        
        # add header elements
        htmltools::singleton(tags$head(
          
          # add title to browser tab
          tags$title('thinkCausal'),
          
          # make all links open in a new tab
          tags$base(target = "_blank"),
          
          # load d3.js
          tags$script(src = c(href = "https://d3js.org/d3.v5.js")),
          tags$script(src = c(href = "https://cdn.jsdelivr.net/jstat/latest/jstat.min.js"))
        )),
        
        # add footer elements
        tags$footer(
          progress_footer,
            
          # load custom JavaScript files
          map(list.files('www/js', pattern = "*.js"), function(file) tags$script(src = file.path('js', file)))
          )
    )
)
