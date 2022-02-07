# How to handle post-treatment variables

## Do not adjust for post-treatment variables 

Including *post-treatment variables* in models for both randomized and observational studies generally is not a good idea and should be avoided. By *post-treatment variables*, we mean variables that can be effected by the treatment and our measured after treatment assignment. When post-treatment variables are included as a predictor or input in a causal analysis [estimates]() of the treatment effect can be [biased]()

### Consider a simple example 

A farmer wants to know if a non-toxic environmentally friendly pest-control method will cause plants to grow taller. The farmer conducted an experiment by randomly assigning half of their 400 plants to revive pest-control and the other half to receive no pest-control. Each plants treatment assignment was recorded with the variable `pest_control` . 

At the end of the growing season, 6 months later, the farmer measures the height of each plant with the variable `height` as well as whether or not there were any bug bites on each plant with the variable `bugs` (plant had no bugs = 0, plant had bugs = 1). The variable `bugs` is a post-treatment variable because it was measured after the treatment had been assigned and it is possible that values of `bugs` may be effected by receiving the pest-control treatment. 


The farmer is unsure if they should control for `bugs` when analyzing the results of the experiment. You can compare the results of an analyses that control for the post-treatment variable `bugs` or do not control for the post-treatment variable `bugs`.
