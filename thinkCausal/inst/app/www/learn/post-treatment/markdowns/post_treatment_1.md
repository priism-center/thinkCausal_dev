# Post-treatment variables

### Do not adjust for post-treatment variables

We use the term post-treatment variables to refer to a class of variables that can be affected by the treatment. *Typically these are measured after treatment assignment but it's possible for a variable measured before treatment implementation to reflect an impact of a treatment if the respondent anticipates that they will be exposed to it in the future.* Including a post-treatment variable as a predictor, covariate or input in a causal analysis can lead to biased estimates of the treatment effect and thus should generally be avoided.

#### Consider a simple example
A farmer wants to know if a new fertilizer will cause tomato plants to yield more fruit. The farmer conducted an experiment by randomly assigning half of their 400 plants to receive fertilizer and the other half to not receive fertilizer. 

At the end of the growing season – 6 months after treatment – the farmer measures how many pounds of fruit each plant produced. However, the farmer noticed that there also appear to be differences across the treatment and control group in the number of leafs on each plant so they also record the number of leafs. The variable `bugs` is a post-treatment variable! It was measured after the treatment was assigned and received and therefore the values of bugs may have been affected by receiving the fertilizer. Since bugs can affect the health of a plant the farmer feels it is important to control for this variable in analysis. But we know that it is post treatment. Let's investigate the trade offs.

