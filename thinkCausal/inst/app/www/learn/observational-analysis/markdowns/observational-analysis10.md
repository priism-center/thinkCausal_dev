## Non-linear data

The problem with the above example is that data in the social sciences is very rarely linear. The assumption of linearity is especially unrealistic when you need adjust for many variables. In practice, models can contain 20, 40 and in some cases 300 covariates! All of these variables can have complex interactions and hidden non-linear relationships with the outcome. 

Lets imagine that instead of the linear relationship between `age` and `Y (running times)` the relationship is noticeably non-linear. There is still lack of overlap for the ATE and ATC so we will stick with estimating the ATT.

While `age` is the only confounding variable in this example, `age`, the non-linear relationship between `age` and `Y (running times)` resembles the types of relationship that would exist if we collapsed many covariates in multi-dimensional space into a single summary variable. Put another way, this example is a collapsed version of what is often happening when you need to adjust for many variables that have complex relationships with one another. 



