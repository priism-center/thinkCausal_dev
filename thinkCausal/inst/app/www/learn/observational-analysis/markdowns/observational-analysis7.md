### Regression with all confounders 

When all confounders are included and the relationship between those confounders and the outcome is linear, a linear regression model can yield unbiased estimates! Our linear regression adjusts accounts for the imbalance of `age` between the treatment and control groups and estimates the ATT is -5.

In this example linear regression is a great solution to the challenges posed by observational studies but as we'll see later, estimates from linear regression will only be unbiased when there truly is a linear relationship between covariates and the outcome variable. 

We are also making the strong assumption that we have included all confounders in the linear regression model. In real data analysis this is a strong assumption and is not guaranteed to be satisfied.

