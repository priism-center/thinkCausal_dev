concepts_page <- tabPanel(
  title = "All concepts",
  div(id = 'conceptsGrid1',
  class = 'conceptsGrid',
  fluidRow(
    column(
      width = 12,
      wellPanel(
        actionLink('practice_test', img(src = 'thumbnails/practice_test.png')),
        br(),
        h3("Test Your Understanding"),
        p("Take practice tests on causal inference concepts")
      )
    )
  ),
  fluidRow(
    column(
      width = 4,
      wellPanel(
        actionLink('concepts_link_Randomization',img(src = 'thumbnails/propensity.png')),
        br(),
        h3("Randomizaion"),
        p("Molestie ligula proin tincidunt aptent rhoncus sapien consequat nisi conubia, vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu egestas blandit.")
      )
    ),
    column(
      width = 4,
      wellPanel(
        img(src = 'thumbnails/PO.png'),
        br(),
        h3("PO"),
        p("Molestie ligula proin tincidunt aptent rhoncus sapien consequat nisi conubia, vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu egestas blandit.")
      )
    ),
      column(
        width = 4,
        wellPanel(
          img(src = 'thumbnails/regression.png'),
          br(),
          h3("All confounders measured"),
          p("Molestie ligula proin tincidunt aptent rhoncus sapien consequat nisi conubia, vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu egestas blandit.")
        )
      )
    ),

  fluidRow(
    column(
      width = 4,
      wellPanel(
        img(src = 'thumbnails/balance.png'),
        br(),
        h3("Causal Estimands"),
        p("Tincidunt pellentesque viverra ultrices bibendum mauris duis ad tempor, nam aliquet quis feugiat augue pretium vulputate dictumst montes, volutpat porttitor elementum eget eleifend nisi cubilia.")
      )
    ),
    column(
      width = 4,
      wellPanel(
        img(src = 'thumbnails/regression_discontinuity.png'),
        br(),
        h3("Regression Trees vs Linear Regression"),
        p("Vitae montes hac diam a odio magnis ante, velit risus gravida fames nunc sociosqu  ad tempor, nam aliquet quis feugiat augue.")
      )
    )
    )
  )
)
