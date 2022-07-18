##### BETA -- work-in-progress #####

# install the required packages
renv::restore()

# load the necessary functions
sapply(list.files('R', full.names = TRUE), source)
# source('clean_auto_convert_logicals.R')
# source('clean_dummies_to_categorical.R')
# source('plot_exploration.R')
# source('clean_detect_column_types.R')
# source('clean_confounders_for_bart.R')

# set file name
#!! file <- filename

# read in the data
#!! csv X <- readr::read_csv(file, col_names = my_col_names)
#!! dta X <- readstata13::read.dta13(file = file)
#!! xlsx X <- openxlsx::read.xlsx(xlsxFile = file)
#!! txt X <- readr::read_delim(file = file, delim = my_delim, col_names = my_col_names)
#!! sav X <- Hmisc::spss.get(file = file)

# select columns and clean it
X <- X[, 2:6]
X <- clean_auto_convert_logicals(X)

# check overlap
plotBart::plot_overlap_vars(
  .data = X,
  treatment = treatment_col,
  confounders = overlap_select_var,
  plot_type = "density"
)

# fit modeltreatment_v <- X[, 1]
response_v <- X[, 2]
confounders_mat <- clean_confounders_for_bart(X[, 3:ncol(X)])
model_results <- bartCause::bartc(
  response = response_v,
  treatment = treatment_v,
  confounders = confounders_mat,
  estimand = 'ate',
  commonSup.rule = 'none',
  keepTrees = TRUE,
  seed = 2
)

# plot results and diagnostics
plotBart::plot_PATE(
  .model = model_results,
  type = 'Density',
  ci_80 = 'none',
  ci_95 = 'none',
  .mean = T,
  .median = F,
  reference = NULL
)

plotBart::plot_trace(model_results)


