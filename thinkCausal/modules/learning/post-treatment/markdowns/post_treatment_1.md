
# Post-treatment variables

Including *post-treatment variables* in models for both randomized and observational studies generally is not a good idea and shoud be avoided. By *post-treatment variables*, we mean variables that can be effected by the treatment and our measured after treatment assignment. When post-treatment variables are included as a predictor or input in a causal analysis [estimates]() of the treatment effect can be [biased]()

<br>

## Consider a simple example

A farmer wants to know if a non-toxic environmentally friendly pest-control method will cause plants to grow taller. The farmer conducted an experiment by randomly assigning half of their 400 plants to revive pest-control and the other half to receive no pest-control. Each plants treatment assignment was recorded with the variable `pest_control` . 

At the end of the growing season, 6 months later, the farmer measures the height of each plant with the variable `height` as well as whether or not there were any bug bites on each plant with the variable `bugs` (plant had no bugs = 0, plant had bugs = 1). The variable `bugs` is a post-treatment variable because it was measured after the treatment had been assigned and it is possible that values of `bugs` may be effected by reviving the pest-control treatment. 


After all of the data is recorded, the farmers  data-set looks like this: 

< INSERT DATATABLE >


The farmer is unsure if they should control for `bugs` when analyzing the results of the experiment. Use the checkboxes below to compare the results of an analyses that control for the post-treatment variable `bugs` or do not control for the post-treatment variable `bugs`.

When `bugs` **is not included** in the analysis, the causal effect of `pest_control` is a comparison between the average height of plants that received the pest-control (colored in red) against the average height and plants that did not receive the pest-control(colored in blue). 

Plants that received the pest-control grew an average of 1.595 inches taller, than they would have grown had they not received the pest-control. 

When `bugs` **is included** in the analysis, the causal effect of `pest_control` is a comparison between the average height of plants that received the pest-control (colored in red) against the average height and plants that did not receive the pest-control(colored in blue) **that is made within groups of plants that had bugs and did not have bugs**. 

Plants that received the pest-control grew an average of -0.1778 inches taller, than they would have grown had they not received the pest-control. 

Including *post-treatment variables* in models for both randomized and observational studies generally is not a good idea and shoud be avoided. By *post-treatment variables*, we mean variables that can be effected by the treatment and our measured after treatment assignment. When post-treatment variables are included as a predictor or input in a statistical model, they can bias[link] estmates of how effective the treatment is.

## Consider a simple example: 

A farmer wants to know if a non-toxic environmentally friendly pest-control method will cause plants to grow taller. The farmer conducted an experiment by randomly assigning half of their 400 plants to revive pest-control and the other half to receive no pest-control. Each plants treatment assignment was recorded with the variable `pest_control` (plan has pest-control = 1, plant has no pest control = 0). 

At the end of the growing season, 6 months later, the farmer measures the height of each plant with the variable `height` as well as whether or not there were any bug bites on each plant with the variable `bugs` (plant had no bugs = 0, plant had bugs = 1). The variable `bugs` is a post-treatment variable because it was measured after the treatment had been assigned and it is possible that values of `bugs` may be effected by reciving the pest-control treatment. 


After all of the data is recorded, the farmers  data-set looks like this: 

