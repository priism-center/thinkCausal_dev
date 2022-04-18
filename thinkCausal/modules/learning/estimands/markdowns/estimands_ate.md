<h2 id="estimands-trigger-1">Average treatment effects</h2>

Including post-treatment variables can drastically impact the results of experiments. The data for this example was simulated so we know that, on average, the pest-control caused plants to grow 1.52 inches taller than they would have grown without the pest-control. We can see that the analysis without the post-treatment variable `bugs` is very close to the true treatment effect. The analysis that includes the post-treatment variable `bugs` is far off from the true treatment effect and would lead an incorrect assessment of the non-toxic environmentally friendly pest-control!

<br>

## Why are analyses with post-treatment variables biased?

To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength? The treatment variable `z` records whether or not an individual participated in the exercise program, the pre-treatment variable. Mid-way through the study, weekly trips to the gym was recorded for each individual with the post-treatment variable `gym`. 

Post-treatment variables, like `gym`, have different [potential outcomes]() that depend on the treatment variable. This is the primary problem of controlling for post-treatment variables. The table below shows the [potential outcomes]() and observed outcomes of `gym` for 2 individuals from our exercise study.In real contexts, we would not have access to both [potential outcomes]() but we can imagine them for the purposes of this example. Notice that two individuals differ in `z` but have the same value for `gym` despite haviving different [potential outcomes](). Controlling for `gym` is not a fair comparison of `strength` because we are not accounting for how `z` changes the value of `gym`. This is not a problem for pre-treatment varibales becuase thier value can not be changed or influenced by `z`. 

< INSERT DATA TABLE >
