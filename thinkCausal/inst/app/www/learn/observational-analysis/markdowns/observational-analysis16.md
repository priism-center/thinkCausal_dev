## Comparing estimates

We'll compare the results again from our example with non-linear data. 

When we estimate a treatment effect, statistical models tell us about the uncertanty of our estimate. When the treatment is not randomly assigned and the data in non-linear, the estimate from BART is the only one that covers the true ATT! 

When the treatment effect is not randomly assigned, we have to make many more assumptions about our estimated causal effect. The difference in means estimator fails becuase it is does not include the confounder `age`. Linear regression includes the confounder `age` but does not appropriately account for the non-linear relationship between `age` and the outcome. 

In our non-linear example, BART was still able to produce an unbiased estimate of the ATT that covered the true value! Keep in mind that in this example we assumed that we have measured an included all confounders in our BART model without all confounders estimated treatment effects could be biased even when using non-linear models!
