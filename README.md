# thinkCausal: A causal inference tool

<!-- badges: start -->
[![GitHub license](https://img.shields.io/github/license/gperrett/thinkCausal_dev?style=flat-square)](https://github.com/gperrett/thinkCausal_dev/blob/master/LICENSE)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg?style=flat-square)]()
[![last commit](https://img.shields.io/github/last-commit/gperrett/thinkCausal_dev?style=flat-square)](https://github.com/gperrett/thinkCausal_dev/commits/master)
<!-- badges: end -->

thinkCausal is federally funded project devoted to building scaffolded causal inference software implementing Bayesian Additive Regression Trees. You can find a live, beta version [here](https://apsta.shinyapps.io/thinkCausal/).

See also the R packages [bartCause](https://github.com/vdorie/bartCause) and [plotBart](https://github.com/joemarlo/plotBart) for much of the underlying causal inference functionality.

thinkCausal is managed by [NYU PRIISM](https://steinhardt.nyu.edu/priism) and is open to contributions. If you'd like to contribute to this project, please see the [contribution guidelines](/.github/CONTRIBUTING.md).

<br>
<p align="center">
<img src="screenshot.png" width=80%>
</p>
<br>

### Run the latest development build locally via R
``` r
# packages <- c('shiny', 'readstata13', 'openxlsx', 'Hmisc', 'readr', 'shinyjs', 'DT', 'sortable', 'dplyr', 'ggplot2', 'stringr', 'purrr', 'bartCause')
# install.packages(packages)
# remotes::install_github('joemarlo/plotBart', ref = "0.1.3")
shiny::runGitHub("thinkCausal_dev", "gperrett", subdir = 'thinkCausal', launch.browser = TRUE)
```
