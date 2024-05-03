In this example, `professional status` and `usa` are the only two covariates that we collected data on.**But there are many other variables that exist in the world that could be confounders!**. Perhaps hyperShoes are disproportionately marketed to younger male runners and runners with hyperShoes have a different `age` and `gender` make up than runners without hyperShoes. The variables `age` and `gender` are also very likely to predict running time. The possibility that `age` and `gender` are confounders we did not collect data on increases doubt in our last analysis that only adjusted for `professional status` and `usa`. 
<br>
<br>
**The ignorability assumption for observational studies is a leap of faith. We need to assume that we have collected data on all the confounders that exist and that we have included these variables in the model used to estimate causal effects.**
<br>
<br>
Many times there are confounding variables that have not collected data on. In these situations are estimated causal effects are likely biased. 

### Randomization
Randomized studies make a much weaker ignorability assumption. When the treatment is randomly assigned, no covariates predict the treatment. This means there are no confounders!!
<br>
<br>
Running randomized experiments is one way to overcome the challenges posed by the ignorability assumption for observational studies. 
<br>
To learn more about the weaker ignorability assumption for randomized studies you can check out the related learning module. 

### Identifying Confounders in thinkCausal
**When analyzing data from an observational study in `thinkCausal`, assume that any pre-treatment variable that can plausibly be related to the outcome variable is a confounder.** The only variables that should not be considered as confounding variables are post-treatment variables, instrumental variables, or ID/index variables. If you don't know what a post treatment variable is make sure to check out our learning module at the end of this article! 
<br>
<br>
Generally, the ignorability assumption is more believable when we have adjusted for more variables. Researchers are often hesitant to include many variables in a statistical model out of concern over including predictor variables that are highly correlated (co-linearity) or overfitting. When using `thinkCausal` these are not valid reasons to exclude variables from your analysis. 
<br>
<br>
`thinkCausal` uses Bayesian Additive Regression Trees (BART) to estimate your causal effects. One of the appealing features about BART is that it automatically prevents over fitting. Some causal analyses have adjusted for hundreds of variables. As long as you use a tool like `thinkCausal` that properly accounts for over fitting you should be not be concerned about including many variables in your analysis.
<br>
For a more detailed explanation about why colinerity is not generally a problem for causal inference check out our learning module on colinearity at the end of this module! 
<br>
<br>
### Practice Selecting Confounders

Use the exercise below to practice selecting confounders for an observational study. Suppose you are analyzing the causal effect of a pre-school program on a vocabulary test at age 4. The pre-school program was not randomly assigned meaning that this is an observational study. 
<br>
<br>
The data for this exercise was simulated so we know the true average treatment effect of the intervention is 4 points. 
<br>
<br>
The initial analysis shows results from using the difference in means estimator. Notice that the estimated ATE from difference in means estimator does not contain the true average treatment effect! This is becuase we have not adjusted for confounders. You can drag variables to include or exclude them from the analysis and see how your estimated results get closer or farther from the true ATE!
