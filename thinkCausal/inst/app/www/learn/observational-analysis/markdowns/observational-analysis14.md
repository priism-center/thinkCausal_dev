## Regression with all confounders 

When the data is non-linear, linear regressions estimate for the ATT of -6.6 is noticeably different from the true ATT! While linear regression attempts to account for the imbalance of `age`, the model is not well fit to the data!

Linear regression assumes that the data is linear. In randomized studies, linear regression remains unbiased even when the data is non-linear because we can expect that the groups are balanced. However, when the treatment is not randomly assigned we can no longer expect balance between groups and need to rely more on our statistical model. In the case of linear regression, this means leaning on the assumption that our data is linear which may or may not be true!

It is tempting to think about transforming the data, adding polynomial terms or adding interactions to the regression to improve the fit but in practice this is not a sustainable solution. Imagine if we had 10, 20 or even 200 confounders we needed to account for! Identifying which variables need to have a polynomial or interaction term quickly becomes impossible. 
