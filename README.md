# Dev repo for thinkCausal

thinkCausal is federally funded project devoted to building scaffolded causal inference software implementing Bayesian Additive Regression Trees.

<br>
<p align="center">
<img src="screenshot.png" width=80%>
</p>

<!--
### Run the latest development build via R
``` r
# packages <- c('shiny', 'foreign', 'readstata13', 'openxlsx', 'shinyjs', 'shinyWidgets', 'DT', 'sortable', 'tidyverse', 'patchwork', 'viridis', 'bartCause', 'rpart.plot')
# install.packages(packages)
# devtools::install_github('joemarlo/plotBart')
shiny::runGitHub("thinkCausal_dev", "gperrett", subdir = 'thinkCausal', ref = 'v0.1.0',  launch.browser = TRUE)
```
-->

### Folder structure
    .
    ├── student_work            # Personal store for students
    ├── thinkCausal             # Main project directory
    │   ├── data                # Test data
    │   ├── man                 # Compiled manual for functions
    │   ├── R                   # Functions
    │   ├── tests               # Unit tests
    │   ├── UI                  # Scripts that build the UI
    │   │  ├── concepts         # Modules that define each concept sub-page
    │   │  ├── headers          # UI code that defines the top nav bar hierarchy
    │   │  ├── markdowns        # Markdowns containing static text such as the help slideover
    │   │  └── pages            # UI code that defines each page
    │   ├── www                 # Browser-side code such as CSS, JavaScript, imgs
    │   ├── DESCRIPTION         # Unused but required for required for documenting R functions via devtools
    │   ├── global.R            # Code that is run prior to launching the Shiny app
    │   ├── manual_workflow.R   # Testing script useful for replicating the tool's workflow
    │   ├── NAMESPACE           # Unused but required for required for documenting R functions via devtools
    │   ├── server.R            # Server-side code for Shiny app
    │   └── UI.R                # Main UI framework
    ├── writing                 # Store for vignettes and similar content
    ├── LICENSE
    ├── screenshot.png
    └── README.md
