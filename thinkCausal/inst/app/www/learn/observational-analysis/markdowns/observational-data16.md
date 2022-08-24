## Comparing statistical models: non-linear data

We'll compare the results again from our example with non-linear data. 

When we estimate a treatment effect, statisitcal models tell us about the uncertanty of our estimate. When the treatment is not randomly assigned, the estimate from BART is the only one that covers the true ATT! 

When the treatment effect is not randomly assigned, we have to make many more assumptions about our estimated causal effect. The difference in means estimator fails becuase it is does not include the counder `age`. Linear regression includes the confounder `age` but does not appropriatly account for the non-linear relationship between `age` and the outcome. 
