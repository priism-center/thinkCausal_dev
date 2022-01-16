# Contributing to thinkCausal

This outlines how to contribute to thinkCausal and applies to both programmatic and educational content changes. thinkCausal heavily uses [bartCause](https://github.com/vdorie/bartCause) and [plotBart](https://github.com/joemarlo/plotBart). Please consider contributing to those packages if you have specific model or plotting changes.

## Issues
Issues and questions can be submitted through the [Github Issues page](https://github.com/gperrett/thinkCausal_dev/issues).

## Code and content guidelines

Contributions should be made via the [pull request process (PR)](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests). We recommend [forking](https://docs.github.com/en/get-started/quickstart/fork-a-repo) thinkCausal, creating a new [branch](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-branches), making your changes/additions, and then submitting the PR. Please include a description of the changes and the 'why' within the PR. See [here](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/) for more information about forking and branching.

Feel free to raise an [issue](https://github.com/gperrett/thinkCausal_dev/issues) if you're uncomfortable with the PR process and only have a small or isolated change.

## Code guidelines

thinkCausal is a mix of 'base' R and tidyverse code. We try to adhere to the [tidyverse style guide](https://style.tidyverse.org/) for code formatting. The easiest way to to format ~90% of your R code is `Code -> Reformat Code` within Rstudio.

We heavily use [roxygen2](https://roxygen2.r-lib.org/) for documenting functions and [testthat](https://testthat.r-lib.org/) for unit testing. Please familiarize yourself with both and feel free to reach out with an [issue](https://github.com/gperrett/thinkCausal_dev/issues) if you have any questions.

## Content structure

The core Shiny app is within the `/thinkCausal` directory. Drafts for articles and vignettes should be stored within the `/writing` directory. If you're a NYU student, please use the `/student_work` directory for storing draft work.

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
