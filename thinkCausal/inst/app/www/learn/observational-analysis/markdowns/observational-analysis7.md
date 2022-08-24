## Regression with all confounders 

When the data is linear and all confounders are included, estimates from a linear regression model are unbiased! Our linear regression adjusts accounts for the imbalance of `age` between the treatment and control groups and estimates the ATT is -5. 

In this example linear regression is a great solution to the challenges posed by observational studies but as we'll see later, estimates from linear regression will only be unbiased when the assumptions of a linear relationship between covariates and the outcome variable. 

We are also making the strong assumption that we have included all confounders in the linear regression model. In this example we have, but in real data analysis this is a massive assumption.

