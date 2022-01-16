# thinkCausal: A causal inference tool

<!-- badges: start -->
[![GitHub license](https://img.shields.io/github/license/gperrett/thinkCausal_dev?style=flat-square)](https://github.com/gperrett/thinkCausal_dev/blob/master/LICENSE)
[![last commit](https://img.shields.io/github/last-commit/gperrett/thinkCausal_dev?style=flat-square)](https://github.com/gperrett/thinkCausal_dev/commits/master)
<!-- badges: end -->

thinkCausal is federally funded project devoted to building scaffolded causal inference software implementing Bayesian Additive Regression Trees. You can find a live, beta version [here](https://apsta.shinyapps.io/thinkCausal/).

See also the R packages [bartCause](https://github.com/vdorie/bartCause) and [plotBart](https://github.com/joemarlo/plotBart) for much of the underlying causal inference functionality.

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

