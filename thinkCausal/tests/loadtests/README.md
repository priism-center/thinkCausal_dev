# Load testing

Note: shinyloadtests has an issue with selectize elements so loadtests that open the EDA page or model page with subgroup analysis enabled will fail. Must use the lalonde test data (do not upload data) as upload will fail as well.

See https://rstudio.github.io/shinyloadtest/index.html. Must install `shinyloadtest` library and `shinycannon` command line tool.

## Process
- Need to run app in a seperate R process
- Then run `shinyloadtest::record_session()`, use the app, then close
- Run the load test within terminal using `shinycannon recording.log http://127.0.0.1:6830 --workers 5 --loaded-duration-minutes 2 --output-dir run1`
-Load the results within R `df <- shinyloadtest::load_runs("run1")` 
-Generate the report `shinyloadtest::shinyloadtest_report(df, "report.html")`

