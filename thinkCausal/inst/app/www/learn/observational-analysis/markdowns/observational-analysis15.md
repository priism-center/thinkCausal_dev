## BART with all confounders 

BART does not assume the data is linear and the estimated ATT  of -8.0 from our BART model is spot on!

BART automatically learns any non-linearities and accounts for complex interactions between variables. Unlike linear regression and difference in means, BART limits its inference to the part of sample we are attempting to make inferences about. Notice that our BART model is only modeling the causal effect of the treatment group, this is because we are estimating the ATT! When using teh difference in means and linear regression, estimates for the ATE, ATT and ATC are the same. BART is able to identify that causal effects for the group of people that received the treatment may be drastically different than the group that received the control!

Remember that BART is not magjic, when the treatment is not randomly assigned, BART assumes that all confounders are included in the model!
