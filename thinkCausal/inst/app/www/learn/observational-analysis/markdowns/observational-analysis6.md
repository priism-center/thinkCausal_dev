## Difference in means

In observational studies, **when confounders like `age` are not adjusted for the estimated treatment effect will be biased**. If we use the difference in means  as our statistical model, we are not adjusting for the imbalance of age and our resulting estimate of -15.4 is extreamly biased from the true ATT. 

The difference in means was producs unbiased estimates when the treatment is randonly assigned because reandomization ensures that we can expect balance across all of our covariates (this includes both measured and non-measured covariates!). The difference in means statsitical model is biased for observational studies becuase *confounding variables* can exist and their imbalance is not accounted for by this statistical model. 
