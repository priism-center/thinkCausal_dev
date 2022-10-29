### Difference in means

If we use the difference in means as our statistical model, our estimate would be -15.4 which is extremely far from true ATT! But perhaps this is not surprising... when we use difference in means, we are not incorporating any information about age and the imbalance of age has not been accounted for.

The difference in means estimate produces unbiased estimates when the treatment is randomly assigned because randomization ensures that we can expect balance across all of our covariates and that there are no confounding variables because no covariates can possibly be associated with the treatment. Difference in means is biased for observational studies because it ignores the fact that the treatment and control groups might differ based on *confounding variables*.
