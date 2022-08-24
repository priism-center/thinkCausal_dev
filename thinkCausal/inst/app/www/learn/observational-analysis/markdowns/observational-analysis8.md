## BART with confounders 

Unlike liner regression, BART does not assume the relationship between predictors and outcome is linear, but when the data is actually linear BART performs as well as a linear regression. If we fit a BART model to our observational data the estimated ATT is -4.9, the same as the true ATT!

Just like with linear regression, we are also making the strong assumption that we have included all confounders in the BART model. In this example we have, but in real data analysis this is a massive assumption.
