Including post-treatment variables can drastically impact the results of experiments. The data for this example was simulated so we know that, on average, the pest-control caused plants to grow 1.52 inches taller than they would have grown without the pest-control. We can see that the analysis without the post-treatment variable `bugs` is very close to the true treatment effect. The analysis that includes the post-treatment variable `bugs` is far off from the true treatment effect and would lead an incorrect assessment of the non-toxic environmentally friendly pest-control!

Some fields would describe `bugs` as *mediator*, *process variable* or *intermediate outcome* meaning that the treatment appears to work by protecting pants from being eaten by bugs. *Mediator*, *process variable*, *intermediate outcome* are all post-treatment variables and should not be adjusted for.  

<br>

## Identifying post-treatment variables in practice

Any variable that can be realistically influenced by the treatment is considered a post-treatment variable. Imagine a study that was evaluating the casual effect of a high quality pre-school education program on 2nd grade reading scores. When considering post-treatment variables, it does not matter if pre-school education program is randomly assigned ([randomized experiment]()) or self-selected ([observational study]()). Some information about children and their parent(s) was recorded before the program began, 1 month after the program began and 4 years after the program began when the children were in 2nd grade. 

We can think of these data as existing on a timeline:
