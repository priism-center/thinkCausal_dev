app <- ShinyDriver$new("../../")
app$snapshotInit("load_data")

app$setInputs(nav = "Data")
app$uploadFile(analysis_data_upload = "../../data/lalonde.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
# app$waitForShiny()
# app$setInputs(analysis_data_dragdrop_treatment = 'treat')
# app$setInputs(analysis_data_dragdrop_response = 're78')
# app$setInputs(analysis_data_dragdrop_covariates = c('age', 'educ', 'black', 'hisp'))
app$setInputs(analysis_data_button_columnAssignSave = "click")
app$setInputs(analysis_data_save = "click")
app$snapshot()
