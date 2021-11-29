# thinkCausal: A causal inference tool

<!-- badges: start -->
[![GitHub license](https://img.shields.io/github/license/gperrett/thinkCausal_dev?style=flat-square)](https://github.com/gperrett/thinkCausal_dev/blob/master/LICENSE)
[![last commit](https://img.shields.io/github/last-commit/gperrett/thinkCausal_dev?style=flat-square)](https://github.com/gperrett/thinkCausal_dev/commits/master)
<!-- badges: end -->

thinkCausal is federally funded project devoted to building scaffolded causal inference software implementing Bayesian Additive Regression Trees. You can find a live, beta version [here](https://apsta.shinyapps.io/thinkCausal/).

<br>
<p align="center">
<img src="screenshot.png" width=80%>
</p>

<!--
### Run the latest development build via R
``` r
# packages <- c('shiny', 'foreign', 'readstata13', 'openxlsx', 'Hmisc', 'readr', 'shinyjs', 'shinyWidgets', 'DT', 'sortable', 'tidyverse', 'dplyr', 'ggplot2', 'stringr', 'purrr', 'viridis', 'rpart.plot', 'bartCause')
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
