## Comparing estimates

Lets compare the results from our 3 statistical models. 

Difference in means is far off from the true ATT, and the uncertanty estimates provided by difference in means do not cover the true ATT. 

When the assumption of linearity and the assumption that all confounders are measured and included in the model are held, linear regression can estimate the true ATT even when treatment is not randomly assigned. The same is true for the BART model, when all all confounders are measured and included in the model estimates from a BART model are unbiased estimates of the true ATT. 

In the next section, we'll see what would happen if our data was no longer linear!
