plot_node <- tabPanel(
  title = "Exploratory Plots", #htmlOutput("exploration_tab_name"),
  tabsetPanel(
    id = "analysis_plot_tabs", 
    analysis_plot_eda, 
    analysis_plot_common_sup, 
    analysis_plot_balance
    )
  )
