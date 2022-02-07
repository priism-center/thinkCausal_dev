
Pre-treatment variables are shown as green dots while post-treatment variables are shown as red dots. Variables measured before the treatment began (parents education, zip-code and time spent reading to child) are automatically not pot-treatment variables. Because the treatment has not happened yet, these variables cannot possible be influenced by the treatment. 

In this example birth-weight was recorded at the end of the study. Even though birth-weight was recorded after the program began, it is not a post-treatment variable and should still be included as a covariate because a child's birthweight happened *before* the treatment and it is impossible for a pre-school program to change a child's birth-weight. 

Self-esteem and math scores are post-treatment variables and should not be included in a causal analysis. Both of these variables are measured long-after the treatment was received and could be influenced by whether or not a child had high-quality pre-school. 

### Including parents income is a  complicated choice

Labeling this variable as pre-treatment or post-treatment is complicated and unclear. In this example, it was measured 1 month after the pre-school program began and it is possible that the pre-school program could effect parent(s) income through freeing up more time to work more hours or apply to higher paying jobs. Yet, parent's income is an important covariate. Parent(s) income likely has a large influence on the outcome excluding parent(s) income from the analysis would lead to [less efficient estimation of the treatment effect](). If access to the pre-school program was not [randomly assigned]() parent's income would be important to satisfy the [all confounders measured]() assumption.

*TO DO* what is our recommendations? 

<br>

## Why are analyses with post-treatment variables biased?

Post-treatment variables introduce bias because they have different [potential outcomes]() that depend on the treatment assignment, however, we are never able to observe all the [potential outcomes]() of a post-treatment variable. 

Consider another hypothetical example, does an exercise program increases muscle strength? The treatment variable `z` records whether or not an individual participated in the exercise program , the pre-treatment variable `baseline_weight` records each individuals weight in pounds before the exercise program began. Mid-way through the study, weekly trips to the gym was recorded for each individual with the post-treatment variable `gym`. 

It is easy to imagine how the [potential outcomes]() of `gym` vary for each person depending on whether or not they are in the exercise program. If the program is effective, those who participate in the program are likely to go to the gym more than they would have had they not participated in the program. The same cannot be said for `baseline_weight`, it is impossible for participation in the exercise program to change a persons pre-program weight. 

The table below shows 4 participants from the exercise study and allows you to see what would happen if we could create a parallel world where we could flip an individuals treatment assignment:
