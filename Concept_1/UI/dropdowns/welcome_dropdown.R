welcome_dropdown <- tabPanel(
  title = 'Welcome', 
  mainPanel(
    h2("thinkCausal"),
    p('Lobortis inceptos pulvinar ridiculus euismod habitant erat quam quis parturient suspendisse fermentum, ligula dignissim habitasse vulputate lacus est ad porta egestas. Ultrices semper phasellus augue enim fringilla suscipit ornare, nascetur nec fames nostra etiam luctus rutrum, sociis magnis ullamcorper mollis dui in. Dui viverra vehicula varius a interdum orci parturient tincidunt netus urna tristique bibendum, sagittis neque porta euismod dapibus dis taciti est ad hendrerit montes. Molestie ligula proin tincidunt aptent rhoncus sapien consequat nisi conubia, '),
    br(),
    div(id = 'conceptsGridHome',
        class = 'conceptsGrid',
        fluidRow(
          column(4, 
                 wellPanel(
                   actionLink("welcome_link_concepts", img(src = 'thumbnails/randomization.png')),
                   br(),
                   h3("Learn"),
                   p("Molestie ligula proin tincidunt aptent rhoncus sapien consequat nisi conubia, vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu egestas blandit. ")
                 )
          ),
          column(4, 
                 wellPanel(
                   actionLink("concepts_link_Fundamental problem", img(src = 'thumbnails/fundamental_problem.png')),
                   br(),
                   h3("Practice"),
                   p("Tincidunt pellentesque viverra ultrices bibendum mauris duis ad tempor, nam aliquet quis feugiat augue pretium vulputate dictumst montes, volutpat porttitor.")
                 )
          ),
          column(4, 
                 wellPanel(
                   actionLink("welcome_link_Analysis", img(src = 'thumbnails/assumptions.png')),
                   br(),
                   h3("Fit"),
                   p("Vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu  ad tempor, nam aliquet quis feugiat augue.")
                 )
          )
        )
        ),
    h3("What is thinkCausal?"),
    includeMarkdown('UI/markdowns/landing.md')
    )
)