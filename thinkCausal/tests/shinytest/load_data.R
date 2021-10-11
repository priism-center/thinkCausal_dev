app <- ShinyDriver$new("../../")
app$snapshotInit("load_data")

app$setInputs(nav = "Data")
app$uploadFile(analysis_data_upload = "../../data/lalonde.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$waitForValue("analysis_data_dragdrop", ignore = list(NULL))
app$setInputs(analysis_data_dragdrop_treatment = 'treat', allowInputNoBinding_ = TRUE)
app$setInputs(analysis_data_dragdrop_response = 're78', allowInputNoBinding_ = TRUE)
app$setInputs(analysis_data_dragdrop_covariates = c('age', 'educ', 'black', 'hisp'), allowInputNoBinding_ = TRUE)
app$setInputs(analysis_data_button_columnAssignSave = "click")
app$setInputs(analysis_data_save = "click")
app$snapshot()
