
## ATT and ATC 

Including post-treatment variables can drastically impact the results of experiments. The data for this example was simulated so we know that, on average, the pest-control caused plants to grow 1.52 inches taller than they would have grown without the pest-control. We can see that the analysis without the post-treatment variable `bugs` is very close to the true treatment effect. The analysis that includes the post-treatment variable `bugs` is far off from the true treatment effect and would lead an incorrect assessment of the non-toxic environmentally friendly pest-control!


<div id='estimands-plot-ATT'></div>

When `bugs` **is not included** in the analysis, the causal effect of `pest_control` is a comparison between the average height of plants that received the pest-control (colored in red) against the average height and plants that did not receive the pest-control(colored in blue). 

Plants that received the pest-control grew an average of 1.595 inches taller, than they would have grown had they not received the pest-control. 

When `bugs` **is included** in the analysis, the causal effect of `pest_control` is a comparison between the average height of plants that received the pest-control (colored in red) against the average height and plants that did not receive the pest-control(colored in blue) **that is made within groups of plants that had bugs and did not have bugs**. 

<div id='estimands-plot-ATC'></div>

A farmer wants to know if a non-toxic environmentally friendly pest-control method will cause plants to grow taller. The farmer conducted an experiment by randomly assigning half of their 400 plants to revive pest-control and the other half to receive no pest-control. Each plants treatment assignment was recorded with the variable `pest_control` (plan has pest-control = 1, plant has no pest control = 0). 

At the end of the growing season, 6 months later, the farmer measures the height of each plant with the variable `height` as well as whether or not there were any bug bites on each plant with the variable `bugs` (plant had no bugs = 0, plant had bugs = 1). The variable `bugs` is a post-treatment variable because it was measured after the treatment had been assigned and it is possible that values of `bugs` may be effected by reciving the pest-control treatment. 
