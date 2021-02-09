# concepts_dropdown <- navbarMenu(title = 'Concepts',
#                                 tabPanel(
#                                   title = "Concepts",
#                                   includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))
#                                 )
# )
concepts_dropdown <- navbarMenu(
  title = 'Concepts',
  tabPanel(title = "All concepts",
           div(id = 'conceptsGrid1',
               class = 'conceptsGrid',
               fluidRow(
                 column(3, 
                  wellPanel(
                    actionLink("concepts_link_Randomization", img(src = 'thumbnails/randomization.png')),
                    br(),
                    h3("Randomization"),
                    p("Molestie ligula proin tincidunt aptent rhoncus sapien consequat nisi conubia, vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu egestas blandit. ")
                  )
                 ),
                 column(3, 
                  wellPanel(
                    actionLink("concepts_link_Fundamental problem", img(src = 'thumbnails/fundamental_problem.png')),
                    br(),
                    h3("Fundamental problem of causal inference"),
                    p("Tincidunt pellentesque viverra ultrices bibendum mauris duis ad tempor, nam aliquet quis feugiat augue pretium vulputate dictumst montes, volutpat porttitor elementum eget eleifend nisi cubilia.")
                  )
                 ),
                 column(3, 
                   wellPanel(
                     actionLink("concepts_link_Assumptions", img(src = 'thumbnails/assumptions.png')),
                     br(),
                     h3("Assumptions of causal inference"),
                     p("Vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu  ad tempor, nam aliquet quis feugiat augue.")
                    )
                 ),
                 column(3, 
                  wellPanel(
                    actionLink("concepts_link_Regression methods", img(src = 'thumbnails/regression.png')),
                    br(),
                    h3("BART vs. regression methods"),
                    p("Quis feugiat augue pretium vulputate dictumst montes, volutpat porttitor elementum eget eleifend nisi cubilia.")
                  )
                 )
              ),
              fluidRow(
                column(3, 
                       wellPanel(
                         img(src = 'thumbnails/propensity.png'),
                         br(),
                         h3("BART vs. propensity score methods"),
                         p("Molestie ligula proin tincidunt aptent rhoncus sapien consequat nisi conubia, vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu egestas blandit. ")
                       )
                ),
                column(3, 
                       wellPanel(
                         img(src = 'thumbnails/balance.png'),
                         br(),
                         h3("Assessing overlap and balance"),
                         p("Tincidunt pellentesque viverra ultrices bibendum mauris duis ad tempor, nam aliquet quis feugiat augue pretium vulputate dictumst montes, volutpat porttitor elementum eget eleifend nisi cubilia.")
                       )
                ),
                column(3, 
                       wellPanel(
                         img(src = 'thumbnails/regression_discontinuity.png'),
                         br(),
                         h3("BART for regression discontinuity"),
                         p("Vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu  ad tempor, nam aliquet quis feugiat augue.")
                       )
                ),
                column(3, 
                       wellPanel(
                         img(src = 'thumbnails/randomization.png'),
                         br(),
                         h3("BART for experimental studies, observational studies, and natural experiments"),
                         p("Quis feugiat augue pretium vulputate dictumst montes, volutpat porttitor elementum eget eleifend nisi cubilia.")
                       )
                )
              )
           )
  ),
  "-----",
  tabPanel(title = 'Randomization', 
           randomizationUI(id = "concepts_randomization")),
  tabPanel(title = "Fundamental problem",
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))),
  tabPanel(title = "Assumptions",
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md'))),
  tabPanel(title = "Regression methods",
           includeMarkdown(file.path("UI", "markdowns", 'concepts.md')))
  )