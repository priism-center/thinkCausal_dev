### BART with confounders 

Unlike linear regression, BART does not assume the relationship between predictors and outcome is linear, but when the data is actually linear BART performs quite similarly to linear regression. If we fit a BART model to our observational data the estimated ATT is -4.9, the same as the true ATT!

Just as with linear regression, we are also making the strong assumption that we have included all confounders in the BART model. 
