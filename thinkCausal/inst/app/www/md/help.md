### Help

This is still under development

<br><br><br>


### Upload data

After clicking the browse button you can select any file available on your computer. 

thinkCausal can load in .csv, .txt, .xlsx (Excel), .spss (SPSS), .sav (SPSS), .dta (STATA) or .sas (SAS) files. 
<br>

If you are uploading a .txt file then the delimiter will also need to be specified (usually this is a tab or comma). 

thinkCausal assumes that variables are represented as columns and rows are values of corresponding variables. 

If you have nested or multi-level data, thinkCausal will assumes your data is in the long form. 

If your data does not include a header (variable/column names) row then uncheck the 'Data contains a header row' checkmark and you will be able to rename your columns on the 'Select data' page.



<!-- TODO: describe wide vs long data ] -->

If you are using the weblink, upload files are limited to 10mb. If you are using thinkCausal locally, this does not apply.

<br>


### Variable Selection
#### Select Outcome and Treatment

Does z cause y? y is the outcome variable and z is the treatment variable. 

To select an outcome from your uploaded data you can click the corresponding box and choose the outcome variable you are interested in studying. 

You will repeat this process for a treatment variable as well.

**What types of variables can be outcome variables?**

At this point in time, thinkCausal can analyze outcome variables that are either continuous variables or binary variables. 

**What types of variables can be a treatment?**

At this point in time, thinkCausal can only analyze binary treatment variables. In the future we plan to expand to accommodate additional treatment levels variables.

**Example: Choosing an outcome and treatment for the hyperShoe:**

Suppose a hypothetical sporting company has released a new shoe called the hyperShoe, you are interested in knowing whether or not wearing hyperShoes causes runners to run faster in the Detroit marathon than they would have ran had they not worn hyperShoes? 

In this example hyperShoes is the treatment variable and running times in the Detroit marathon is outcome variable. The hyperShoe is the cause we are studying and running time in the marathon is the effect of the cause we want to know!

**Learning Module: Potential Outcomes**

You can learn more about what a cause is, and the difference between treatment and outcome variables in the potential outcomes learning module: 

<a onclick="go_to_shiny_page('learn_potential_outcomes', true);" class="help-link">
  <span class="glyphicon glyphicon-info-sign"></span>  Learn more about potential outcomes
</a>

#### Study Design

Different study designs make different assumptions. thinkCausal needs to know the design of your study so that it can inform you about the assumptions you are making in your analysis. 

thinkCausal supports three different study designs: 
  - Observational Studies (Non-Random Treatment)
  - Completely Randomized Experiments
  - Block Randomized Experiments
  

If you treatment variable has not been randomly assigned your study design is an observational study. 

If your treatment variable has been randomly assigned across the whole data set your study design is a completely randomized experiment. 

If your treatment variable has been randomly assigned within groups (blocks) based on another variable or variables, your study design is a blocked randomized experiment.
  
  
**Observational Studies (Non-Random Treatment):**

In observational studies, the treatment variable is not randomly assigned and individuals may self select into receiving or not receiving the treatment. In thinkCausal studies without any sort of randomization of the treatment are considered observational studies.

Sometimes it is not possible to conduct a randomized study. Consider a study on the causal effects of attending collect on income at age 30. It would be unethical to randomly assign students to attend or not attend college. Yet, understanding the effect of college on income is an important policy question!

Observational studies have imbalance between the treatment group and the control group. In our college example, on average, students that attend college may come from wealthier families and students who do not attend college may come from poorer families. Differences in the average family wealth between students that attend and don't attend college is an example of imbalance. 

thinkCausal can estimate the causal effects of observational studies 




**Completely Randomized Experiment:**

In a completely randomized experiment, the treatment variable is randomly assigned. If everyone in the study has the same probability of receiving the treatment then the study is a completely randomized experiment. 

Consider the hyperShoe, a new type of running shoe released with the claim that wearing hyperShoes causes runners to run faster marathons. If we recruited a group of runners to participate in our study, and flipped a coin to determine if each runner would wear hyperShoes or not wear hyperShoes then this would be a completely randomized experiment because every runners has a .5 probability of wearing hyperShoes and a .5 probability of not wearing hyperShoes. 

The probability of receiving the treatment does not have to .5! If each runner was assigned a .4 probability of receiving the hyperShoe and a .6 probability of not receiving the hyperShoe our study would still be a completely randomized experiment. 

The important detail of what makes a study a completely randomized experiment is that **every** participant has the same probability of receiving the treatment and the same probability of receiving the control. 


**Block Randomized Experiment:**

In a block randomized experiment the treatment variable is randomly assigned within blocks defined by one or more other variables. 

Using the hyperShoes as our example, suppose a small subset of the runners in our sample are professional runners and the remainder of runners in our sample are amateur runners. 

In a block randomized experiment, the randomization of the treatment assignment takes place within groups (blocks) defined by another variable (in this example the other variable is professional status). In a block randomized experiment we could randomize 60% of the professional runners to receive the hyperShoes and 50% of the amateur runners to receive the hyperShoes. 

Notice that within the two blocks (professionals and amateurs) each runners has the same probability of receiving the treatment.

In block randomized experiments it is important to always adjust for the variable you have blocked on! 




#### Survey weights
Sometimes our data comes from surveys that are not representative of the population we are inferences about. If your dataset contains survey weights, indicate *yes* for the the Include survey weights input under Advanced Options.

If your dataset does not contrain survey weights leave this input as *no* which is the default option. 

<br>

#### Clustered and nested data
Risus in hendrerit gravida rutrum quisque non. Fermentum posuere urna nec tincidunt praesent semper feugiat nibh sed. Urna duis convallis convallis tellus id interdum. Et tortor at risus viverra adipiscing at in tellus. Tortor id aliquet lectus proin nibh nisl. Sapien faucibus et molestie ac.

<br>


#### Select covariates
**Which variables should I include?**

Include all **pre-treatment** variables that you believe could predict the outcome in the analysis. thinkCausal estimates casual effects using Bayesian Additive Regression Trees (BART), BART automatically prevents over-fitting and can accommodate many covariates. Some causal inference studies include over 300 variables as covariates. 

**Post-treatment variables should not be included as covariates in the model.** A post-treatment variable is any variable that could plausibly be be influenced by the treatment.

<a onclick="go_to_shiny_page('learn_post_treatment', true);" class="help-link">
  <span class="glyphicon glyphicon-info-sign"></span>  Learn more about post-treatment variables
</a>

**ID variables should not be included as covariates in the model.** ID variables are variables that represent row numbers or participant IDs and do not predict the outcome variable. 


**How do I select variables?**

Use the drag-and-drop to include additional variables in the analysis. You may click multiple variables to drag-and-drop a group of variables. 

You can move all the variables in your dataset by clicking "Move all covariates to include box". 

**After you have selected variables to include in the analysis click on the "Save variable selection & continue" button**. 

<!--
#### Group data

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Sit amet massa vitae tortor condimentum lacinia quis vel.
-->

<br>

### Verify data

On this page you can change variables names and check that thinkCausal has made reasonable decisions about whether a variable is modeled as a continuous predictor or an indicator variable. 

thinkCausal estimates causal effects with Bayesian Additive Regression Trees, a flexible machine learning model! For the BART model fit by thinkCausal the including a predictor as a continuous variable or as a categorical variable will not impact the results. For linear regression, decisions about modeling a variable as a continuous predictor or as a categorical variable can heavily influence the results. 

The defaults in thinkCausal will automatically include any variable with less than 15 categories as a categorical variable.

**Missing data:**

thinkCausal will alert you to the amount of missing data for each variable you have selected to include the analysis as well as the total number of rows containing missing data.

Currently, thinkCausal uses listwise deletion and any row with a missing value will be removed from the analysis. 

**reverting changes:**

The revert variable changes button will return the dataset names and variable types to the initial thinkCausal defaults for names and variable types. 

### Exploratory Data Visualization

Visualizing your data is an important part of the data analysis process. Exploratory data visualization may inform hypotheses about potential subgroup or moderation effects. 

These plots should not be used to inform variable selection. You should have already included any variable that is plausibly predictive of the outcome variable and is not a post-treatment variable.

In the plot type input you can choose between visualizing a scatter plot, histogram, bar plot, density plot and boxplots. 

<br>

### Balance

Balance is how comparable predictor variables are between the treatment and control groups. We can compare balance of the means, variance (continuous variables only) and covariance. The dashed center line represents the control group. Points represent how different the treatment group is in terms of means, variance or covariance for each predictor variable. 

thinkCausal will fit a flexible Bayesian Additive Regression Tree (BART) machine learning model to account for imbalanced data. Unlike in propensity score matching you will not need to check balance after the model is fit. 

When the treatment and control groups are **balanced** all points should be close to the center line. The further away a point is from the center line the more **imbalanced** the data is. 

In a completely randomized experiments the data is balanced in expectation, and we expect all points to be close to the center line. The figure below shows the balance plot for a completely randomized experiment on the effects of a job training program on real wages. 

<p align="center">
<img src="www/img/example_balance.png" width=90%>
</p>

<br>

### Overlap

Vitae auctor eu augue ut lectus. Vitae congue eu consequat ac felis donec et odio. Phasellus egestas tellus rutrum tellus pellentesque eu. Vitae congue mauris rhoncus aenean vel elit scelerisque. Congue nisi vitae suscipit tellus. Commodo ullamcorper a lacus vestibulum sed arcu non.

<br>

### Model

Nam libero justo laoreet sit amet cursus sit amet dictum. Lorem donec massa sapien faucibus et molestie ac feugiat.

Ut tristique et egestas quis ipsum suspendisse. Turpis egestas pretium aenean pharetra magna ac. Pellentesque id nibh tortor id aliquet. Tortor aliquam nulla facilisi cras fermentum odio. Sapien faucibus et molestie ac feugiat. Odio euismod lacinia at quis risus sed vulputate. Vitae auctor eu augue ut lectus. Vitae congue eu consequat ac felis donec et odio. 

Phasellus egestas tellus rutrum tellus pellentesque eu. Vitae congue mauris rhoncus aenean vel elit scelerisque. Congue nisi vitae suscipit tellus. Commodo ullamcorper a lacus vestibulum sed arcu non.

<br>

#### Common support
BART models use the uncertainty of counter factual uncertainty. When the posterior distribution of an individual's counterfactual prediction extends beyond a specified cut-point, that point likely has insufficient common support. 'bartCause' model offer the option to automatically remove points without common support from analyses. Cut-points are determined through one of two rules: the standard deviation (sd) or chi-squared (chi). 

Under the standard deviation rule, a point has weak common support if its posterior distribution of the counterfactual deviation is greater than the maximum posterior of the observed predictions with 1 standard deviation of the distribution of standard deviations for each individual's predicted outcome under the observed assignment. 

Under the chi-squared rule, a point is discarded if the variance between its counterfactual prediction over observed prediction are statistically different under a chi-squared distribution with 1 degree of freedom. 

For more details on discard rules see Hill and Su 2013.

<br>

#### Secondary analyses
Egestas dui id ornare arcu odio ut sem. Consectetur a erat nam at lectus urna duis. Turpis egestas sed tempus urna et pharetra pharetra massa. Et netus et malesuada fames. Senectus et netus et malesuada. Aliquet nibh praesent tristique magna sit amet purus.

<br>

### Diagnostics

Amet porttitor eget dolor morbi non arcu risus. Adipiscing at in tellus integer feugiat scelerisque varius. Nisl condimentum id venenatis a condimentum vitae sapien pellentesque. Risus commodo viverra maecenas accumsan lacus vel facilisis volutpat est. In iaculis nunc sed augue lacus viverra vitae. 

Etiam erat velit scelerisque in dictum non. Tristique nulla aliquet enim tortor at auctor. Id consectetur purus ut faucibus pulvinar. Egestas dui id ornare arcu odio ut sem. Consectetur a erat nam at lectus urna duis. Turpis egestas sed tempus urna et pharetra pharetra massa. Et netus et malesuada fames. Senectus et netus et malesuada. Aliquet nibh praesent tristique magna sit amet purus.

<br>

#### Trace plot
Trace plot shows the estimated effect over each iteration of the model fit. This is used to visually assess the convergence of Markov chain Monte Carlo (MCMC) sampling. Chains should be well mixed such that no single color is notably separate from others like the example below.

<p align="center">
<img src="www/img/example_trace.png" width=90%>
</p>

<br>

#### Common support
Id eu nisl nunc mi ipsum. Ut aliquam purus sit amet luctus venenatis lectus. Sed augue lacus viverra vitae. Mattis vulputate enim nulla aliquet porttitor. Risus quis varius quam quisque. Arcu odio ut sem nulla. Nunc sed id semper risus in hendrerit gravid

<p align="center">
<img src="www/img/example_common_support.png" width=90%>
</p>

<br>

#### Residual plot
Vel risus commodo viverra maecenas accumsan. Tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam faucibus purus in massa tempor nec feugiat nisl. Morbi tempus iaculis urna id volutpat lacus laoreet non curabitur.

<br>

### Results

Justo donec enim diam vulputate ut. Odio eu feugiat pretium nibh ipsum consequat nisl vel pretium. Congue eu consequat ac felis donec et. Diam vel quam elementum pulvinar etiam non quam. Vel risus commodo viverra maecenas accumsan. Tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam faucibus purus in massa tempor nec feugiat nisl. Morbi tempus iaculis urna id volutpat lacus laoreet non curabitur.

Ipsum dolor sit amet consectetur. Id eu nisl nunc mi ipsum. Ut aliquam purus sit amet luctus venenatis lectus. Sed augue lacus viverra vitae. Mattis vulputate enim nulla aliquet porttitor. Risus quis varius quam quisque. Arcu odio ut sem nulla. Nunc sed id semper risus in hendrerit gravid

<br>

### Subgroup analyses

Consequat semper viverra nam libero justo laoreet. Mauris nunc congue nisi vitae suscipit tellus mauris. Vel eros donec ac odio tempor. Sit amet consectetur adipiscing elit duis tristique sollicitudin nibh. Aliquam id diam maecenas ultricies mi eget mauris pharetra. 

<br>

#### Check
ICATEs are the difference in each individual's predicted outcome under the treatment and predicted outcome under the control averaged over the individual. Plots of ICATEs are useful to identify potential heterogeneous treatment effects between different individuals. 

<br>

#### Explore
This plot is useful for identifying potential moderating variables. Here, we fit a single regression tree on the BART model ICATEs to produce a variable importance plot. Tree depth may be set to depths 1, 2 or 3. Terminal nodes signal the Conditional Average Treatment effect within levels of moderation variables. Trees with different values across terminal nodes suggest strong treatment effect moderation.

<br>

#### Test
Vel risus commodo viverra maecenas accumsan. Tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam faucibus purus in massa tempor nec feugiat nisl. Morbi tempus iaculis urna id volutpat lacus laoreet non curabitur.


<br><br><br><br><br><br><br><br>


