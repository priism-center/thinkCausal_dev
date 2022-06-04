concepts_page <- tabPanel(
  title = "All concepts",
  div(id = 'conceptsGrid1',
    class = 'conceptsGrid',
    # fluidRow(
    #   column(
    #     width = 12,
    #     wellPanel(
    #       actionLink('practice_test', img(src = 'thumbnails/practice_test.png')),
    #       br(),
    #       h3("Test Your Understanding"),
    #       p("Take practice tests on causal inference concepts")
    #     )
    #   )
    # ),
    
    fluidRow(
      create_learning_card(
        page_id = 'concepts_link_causal_estimands',
        thumbnail_url = 'estimands.png',
        title = "Causal estimands",
        description = "BART allows for robust estimation of a wide variety of estimands. Learn how they differ and how to choose one."
      ),
      create_learning_card(
        page_id = 'concepts_link_post_treatment_variables',
        thumbnail_url = 'post-treatment.png',
        title = "Post-treatment variables",
        description = "Post-treatment variables are a class of variables that can be affected by the treatment and should be removed prior to modeling. Learn how to identify them to make sure you are not biasing your treatment effect estimates."
      ),
      create_learning_card(
        page_id = 'concepts_link_potential_outcomes',
        thumbnail_url = 'potential-outcomes.png',
        title = "Potential outcomes",
        description = "The potential outcomes framework is a methodology to estimate causal effects. Learn the theory foundations here."
      )
    ),
  
    fluidRow(
      create_learning_card(
        page_id = 'concepts_link_randomization',
        thumbnail_url = 'randomization.png',
        title = "Randomization",
        description = "Randomization balances groups on both observed and unobserved characteristics. Learn how this mechanism is exploited for causal inference."
      ),
      create_learning_card(
        page_id = 'concepts_link_bias_and_efficiency',
        thumbnail_url = 'balance.png',
        title = "Bias and efficiency",
        description = "Coming soon"
      ),
      create_learning_card(
        page_id = 'concepts_link_lorem_ipsum',
        thumbnail_url = 'decision_tree.png',
        title = "Decision trees",
        description = "Coming soon"
      )
    )
  )
)
