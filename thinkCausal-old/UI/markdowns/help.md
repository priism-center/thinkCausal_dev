### Coming soon...

This help section is still under development

<br><br><br>

### Study design

The name of your treatment, the units of the outcome variable, and the participants in your study are all optional fields. These fields will populate the automatic interpretations at the end of the analysis.

Example inputs for each are:
- Treatment: "**job training program**"
- Units: "**dollars**"
- Participants: "**low-income households**"

<br>

#### Unsure about your causal estimand?
Nisl vel pretium lectus quam id leo. Vitae et leo duis ut diam. Varius vel pharetra vel turpis nunc eget lorem. Nisl purus in mollis nunc sed. Phasellus faucibus scelerisque eleifend donec pretium vulputate sapien. Semper risus in hendrerit gravida rutrum.

<a onclick="go_to_shiny_page('Causal estimands');">
  <span class="glyphicon glyphicon-info-sign"></span>  Learn more about causal estimands
</a>

<br>

#### Unsure about your study design?
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Sit amet massa vitae tortor condimentum lacinia quis vel. Commodo sed egestas egestas fringilla. Sed velit dignissim sodales ut. Ac odio tempor orci dapibus ultrices in iaculis nunc. Ut sem viverra aliquet eget sit amet tellus. Sit amet venenatis urna cursus eget nunc scelerisque viverra mauris. Fusce id velit ut tortor.

Ipsum dolor sit amet consectetur. Id eu nisl nunc mi ipsum. Ut aliquam purus sit amet luctus venenatis lectus. Sed augue lacus viverra vitae. Mattis vulputate enim nulla aliquet porttitor. Risus quis varius quam quisque. Arcu odio ut sem nulla. Nunc sed id semper risus in hendrerit gravida.

<br>

#### Survey weights
Diam vel quam elementum pulvinar etiam non quam. Vel risus commodo viverra maecenas accumsan. Tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam faucibus purus in massa tempor nec feugiat nisl. Morbi tempus iaculis urna id volutpat lacus laoreet non curabitur.

<br>

#### Clustered and nested data
Risus in hendrerit gravida rutrum quisque non. Fermentum posuere urna nec tincidunt praesent semper feugiat nibh sed. Urna duis convallis convallis tellus id interdum. Et tortor at risus viverra adipiscing at in tellus. Tortor id aliquet lectus proin nibh nisl. Sapien faucibus et molestie ac.

<br>

### Data
Data should be rectangular, wide data and can be a .csv, .txt, .xlsx, .spss, or .dta file. If you are uploading a .txt file then the delimiter will also need to be specified (usually this is a tab or comma). If your data does not include a header row then uncheck the 'Data contains a header row' checkmark and you will be able to rename your columns on the 'Select data' page.

<!-- TODO: describe wide vs long data ] -->

Upload files are limited to 10mb.

<br>

#### Upload data
Each column of your dataset must be matched to one of these roles depending on study design: Covariate, Treatment, Outcome, Block, Post-treatment, or Exclude. These roles are auto-populated based on the column name and column values. You can change the roles by dragging-and-dropping the column names to each respective bucket. Treatment and Response should contain only one column respectively. Please exclude any ID columns or other irrelevant columns from your dataset.

ID, index, and post-treatment variables are excluded from the analysis.

<a onclick="go_to_shiny_page('Post-treatment variables');">
  <span class="glyphicon glyphicon-info-sign"></span>  Learn more about post-treatment variables
</a>

<!--
#### Group data

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Sit amet massa vitae tortor condimentum lacinia quis vel.
-->

<br>

### Verify data

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Sit amet massa vitae tortor condimentum lacinia quis vel. Vitae congue mauris rhoncus aenean vel elit scelerisque. Congue nisi vitae suscipit tellus.

Commodo ullamcorper a lacus vestibulum sed arcu non. Ut tristique et egestas quis ipsum suspendisse. Turpis egestas pretium aenean pharetra magna ac. Pellentesque id nibh tortor id aliquet. Tortor aliquam nulla facilisi cras fermentum odio. Sapien faucibus et molestie ac feugiat. 

<br>

### EDA

Nisl tincidunt eget nullam non nisi est sit. Mus mauris vitae ultricies leo integer malesuada nunc vel. Nibh mauris cursus mattis molestie a iaculis. Nisl vel pretium lectus quam id leo. Vitae et leo duis ut diam. Varius vel pharetra vel turpis nunc eget lorem. Nisl purus in mollis nunc sed. Phasellus faucibus scelerisque eleifend donec pretium vulputate sapien. Semper risus in hendrerit gravida rutrum.

<br>

### Balance

In iaculis nunc sed augue lacus viverra vitae. Etiam erat velit scelerisque in dictum non. Tristique nulla aliquet enim tortor at auctor. Id consectetur purus ut faucibus pulvinar. Egestas dui id ornare arcu odio ut sem. Consectetur a erat nam at lectus urna duis. Tempus imperdiet nulla malesuada pellentesque elit eget gravida. Consectetur adipiscing elit ut aliquam purus sit amet.

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
<img src="img/example_trace.png" width=90%>
</p>

<br>

#### Common support
Id eu nisl nunc mi ipsum. Ut aliquam purus sit amet luctus venenatis lectus. Sed augue lacus viverra vitae. Mattis vulputate enim nulla aliquet porttitor. Risus quis varius quam quisque. Arcu odio ut sem nulla. Nunc sed id semper risus in hendrerit gravid

<p align="center">
<img src="img/example_common_support.png" width=90%>
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

