## BART with confounders 

Unlike liner regression, BART does not assume the relationship between predictors and outcome is linear, but when the data is actually linear BART performs as well as linear regression. If we fit a BART model to our observational data the estimated ATT is -4.9, the same as the true ATT!

Treatment effect estimates from BART models are only unbiased if all confounders have been measured and included in the model. 
