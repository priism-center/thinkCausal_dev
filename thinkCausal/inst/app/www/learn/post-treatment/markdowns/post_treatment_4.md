Including post-treatment variables can drastically impact the results of both randomized experiments and observational studies. The data for this example was [simulated]() so we know that, on average, the pest-control caused plants to grow 1.52 inches taller than they would have grown without the pest-control. We can see that the analysis without the post-treatment variable `bugs` is very close to the true treatment effect. The analysis that includes the post-treatment variable `bugs` is far off from the true treatment effect and would lead an incorrect assessment of the non-toxic environmentally friendly pest-control!

Some fields would describe `bugs` as *mediator*, *process variable* or *intermediate outcome* meaning that the treatment appears to work by protecting pants from being eaten by bugs. *Mediator*, *process variable*, *intermediate outcome* are all post-treatment variables and should not be adjusted for.  

<br>

### Identifying post-treatment variables

Variables are coincided to be post-treatment if they can be influenced by the treatment variable. Consider another hypothetical example where you are tasked with evaluating if a high quality pre-school program causes higher reading levels among 2nd graders. Besides the treatment variable and outcome variable you collected several covariates. Before the pre-school program began you recorded parent(s) education and income. Long after the program ended you used administrative records to access childrens' birth weight, birth zip-code and childrens' reading level in 1st grade.

We can imagine data as existing on two timelines. The first timeline representing when variables were measured in relation to the treatment and the second representing when variables occurred relative to the treatment: 

