## Our observed data

When we plot the data from our observational study, we can see that `age` is related to the treatment assignment (whether or not a runner wore HyperShoes) and `age` is also related to the outcome (Y running times). Variables like `age` that are associated with both the treatment and the outcome variables are *confounders*. As we will see later, when not appropriately adjusted for, *confounders bias estimates of the treatment effect*.

#### Choosing an Estimand
We also notice that only runners under 35 wore HyperShoes. There is complete overlap for the ATT, but there is not complete overlap for the ATE or ATC! Due to available overlap, we will make causal estimates about the ATT. 

Estimating the ATE requires making inferences about all available data points, however, there is not complete overlap across all the data points. We would be in the same situation if we were trying the estimate the ATC. We don't have enough available information to infer about counter-factuals for the ATE or ATC. However for the ATT, there is complete overlap and we have enough information to infer about what would have happened if those participants had not worn HyperShoes! 

