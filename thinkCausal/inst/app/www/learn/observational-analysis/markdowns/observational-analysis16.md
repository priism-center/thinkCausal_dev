### Summary

When we estimate a treatment effect, statistical models not only give us predictions of the treatment effect that can tell us about the uncertainty of our estimate. In our example, when the treatment is not randomly assigned and the relationships are non-linear, the BART confidence interval is the only one that covers the true ATT! 

When the treatment effect is not randomly assigned, we have to make many more assumptions about our estimated causal effect. The difference in means estimator fails because it does not include the confounder age. Linear regression includes the confounder age but does not appropriately account for the non-linear relationship between age and the outcome. 

In our non-linear example, BART was still able to produce an unbiased estimate of the ATT that covered the true value! Keep in mind that in this example we assumed that we have measured and included all confounders in our BART model without all confounders estimated treatment effects could be biased even when using non-linear models!

