### Non-linear response surface
 
The problem with the prior example is that we can't expect linear relationships between covariates and outcomes in social science data. The assumption of linearity is especially unrealistic when you need to adjust for many variables. In practice, models can contain 20, 40 and in some cases 200 or more covariates! All of these variables can have complex interactions and hidden non-linear relationships with the outcome. 

Let's imagine that instead of the linear relationship between `age` and `Y (running times)` the relationship is noticeably non-linear. There is still lack of overlap for the ATE and ATC so we will stick with estimating the ATT.

