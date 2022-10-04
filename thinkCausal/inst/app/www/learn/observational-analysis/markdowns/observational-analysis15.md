### BART with all confounders 

BART does not assume the relationship between the covariates and the response are linear. The BART estimate of the ATT is -8.0 which is spot on!

The BART algorithm automatically learns about any non-linearities and accounts for these and for complex interactions between variables. Unlike linear regression and difference in means estimation of causal effects, the standard approach to BART for causal inference limits its inference to the part of the sample we are attempting to make inferences about and this decision is generally based on properties of the sample such as overlap. For instance in our example our BART model is only modeling the causal effect for the treatment group. This is because we are estimating the ATT and we limited our inference to the ATT because of overlap issues! In contrast, the standard difference-in-means and linear-regression estimates for the ATE, ATT and ATC are the same. BART is able to identify that causal effects for the group of people that received the treatment may be drastically different than the group that received the control!

Remember that BART is not magic. When the treatment is not randomly assigned, BART assumes that all confounders are included in the model!
