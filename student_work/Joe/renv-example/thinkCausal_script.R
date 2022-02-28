##### BETA -- work-in-progress #####

# install.packages('renv')
renv::restore()

# load the necessary functions
purrr::walk(list.files('R', full.names = TRUE), source)

# select columns and rename
X <- readr::read_csv(file = 'data/lalonde.csv', col_names = TRUE)
X <- X[, c('treat', 're78', 'age', 'educ', 'black', 'hisp', 'married', 'nodegr', 're74', 're75', 'u74', 'u75')]
X <- clean_auto_convert_logicals(X)

colnames(X) <- c('treat', 're78', 'age', 'educ', 'black', 'hisp', 'married', 'nodegr', 're74', 're75', 'u74', 'u75')

# new column names
old_col_names <- colnames(X)
new_col_names <- paste0(c('Z', 'Y', rep('X', length(old_col_names)-2)), '_', old_col_names)
colnames(X) <- new_col_names





# run model
treatment_v <- X[, 1]
response_v <- X[, 2]
confounders_mat <- clean_confounders_for_bart(X[, 3:ncol(X)])
model_results <- bartCause::bartc(
response = response_v,
treatment = treatment_v,
confounders = confounders_mat,
estimand = 'ate',
commonSup.rule = 'sd',
keepTrees = T,
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

plotBart::plot_common_support(
.model = model_results,
rule = 'both'
)

### examples
# how to save a plot
p <- plotBart::plot_trace(model_results)
ggplot2::ggsave("myplot.png", p, width = 5, height = 5)

