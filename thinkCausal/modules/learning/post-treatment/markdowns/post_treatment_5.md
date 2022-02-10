
<br>

All variables measured before the treatment are automatically pre-treatment variables and should be included as covariates. Variables measured after the treatment have the possibility to be post-treatment variables but this is not necessarily the case. Despite being collected long after the treatment, the variables `birth weight` and `birth zip-code` are pre-treatment variables because their values were determined before the pre-school program began. It is impossible to imagine a world in which receiving the pre-school program could change a child's `birth weight`. Demographic and biologically fixed variables like age or sex can not reasonably be influenced by the treatment and should be included as covariates

Variables that have been measured after the treatment and can plausibly be influenced by the treatment are considered post-treatment variables. This is the case for `1st grade reading level` which was measured after the treatment and can realistically be influenced by whether or not a child received high quality pre-school. 

It may not always be clear when variables were measured. Accidentally including post-treatment variables in your analysis will likely [bias]() the [estimate of your treatment effect](). In situations where a variable could be influenced by the treatment and you are unaware of when the variable was measured, we recommend seeking clarification from others or from consulting documentation from the data source.


### How does including post-treatment variables lead to bias?

Post-treatment variables lead to bias by creating unfair comparisons. Comparisons become unfair because post-treatment variables have different [potential outcomes]() that depend on the treatment variable. To illustrate this, imagine a hypothetical study on if an exercise program causes increased strength. We'll use variable `z` to represent whether or not an individual participated in the exercise program and variable `y` to represent strength at the end of the program. One week, midway through the program, the number of times each individual went to the gym to exercise was recorded with the variable `gym`.

It is easy to imagine how each individual would have a different value of `gym` depending upon whether or not they were participating in the exercise program. All participants in the study different [potential outcomes]() for both `y` and `gym`. A table of all observed and potential outcomes for 3 participants from the exercise study would look like this:

<br>
<table class="table">
  <thead>
    <tr>
      <th scope="col">individual</th>
      <th scope="col">z</th>
      <th scope="col">gym</th>
      <th scope="col">gym if z = 0</th>
      <th scope="col">gym if z = 1</th>
      <th scope="col">y</th>
      <th scope="col">y if z = 0</th>
      <th scope="col">y if z = 1</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Cong</td>
      <td>0</td>
      <td>1</td>
       <td>1</td>
      <td>3</td>
      <td>120</td>
      <td>120</td>
      <td>130</td>
    </tr>
    <tr>
      <td>Andi</td>
      <td>1</td>
      <td>3</td>
      <td>1</td>
      <th scope="row">3</th>
      <td>130</td>
      <td>120</td>
      <td>130</td>
    </tr>
    <tr>
      <td>Lindsey</td>
      <td>0</td>
      <td>3</td>
      <th scope="row">3</th>
       <td>5</td>
      <td>135</td>
      <td>135</td>
      <td>140</td>
    </tr>
  </tbody>
</table>
<br>

We can see that Cong and Andi would be a fair comparisons because they resemble each other across all potential outcomes for `y` and `gym`. Recall that we can never observe both potential outcomes and only have access to the observed value that is determined by the treatment(`z`). If we control for the **observed value** of `gym`, Cong and Andi no longer appear similar! Rather than making the fair comparison between Cong and Andi, a statistical model that controls for the **observed value** of `gym` would be making the unfair comparison between Andi and Lindsey, who have very different potential outcomes. This is not a problem for pre-treatment variables because it is impossible for them to depend upon the treatment. 

Suppose instead of controlling for the post-treatment variable `gym`, we had a pre-treatment variable `gym_pre` which measured the number of times each individual had gone to the gym the week before the exercise program began. Pre-treatment variables like `gym_pre` do not have different [potential outcomes]() that depend the treatment variable. Put another way, it is impossible for `gym_pre` to change depending on if an individual participated in the exercise program. 

<br>
<table class="table">
  <thead>
    <tr>
      <th scope="col">individual</th>
      <th scope="col">z</th>
      <th scope="col">gym_pre</th>
      <th scope="col">y</th>
      <th scope="col">y if z = 0</th>
      <th scope="col">y if z = 1</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Cong</td>
      <td>0</td>
      <th scope="row">0</th>
      <td>120</td>
      <td>120</td>
      <td>130</td>
    </tr>
    <tr>
      <td>Andi</td>
      <td>1</td>
      <th scope="row">0</th>
      <td>130</td>
      <td>120</td>
      <td>130</td>
    </tr>
    <tr>
      <td>Lindsey</td>
      <td>0</td>
      <td>2</td>
      <td>135</td>
      <td>135</td>
      <td>140</td>
    </tr>
  </tbody>
</table>
<br>

Pre-treatment variables do not have [potetnial outcomes]() that depend on the treatment variable. When we adjust for the pre-treatment variable `gym_pre` our statistical model would be making the fair comparison between Cong and Andi rather than the unfair comparison between Cong and Lindsey that arose when we controlled for a post-treatment variable. 

If you ever need to formally explain why post-treatment variables are problematic you can use the following logic: 

- For a causal estimate to be unbiased both potential outcomes of outcome variable $y$ need to be   independent of the treatment variable $z$. In formal notation this is stated as: 

$$ y^0, y^1 \prep z $$

- Let $X$ represent pre-treatment variables. When we control for or condition on $X$ both potential outcomes of outcome variable $y$ remain independent of treatment variable $z$. In formal notation this is stated as:

$$ y^0, y^1 \prep z|X $$

- Let $Q$ represent post-treatment variables. When we control for or condition on $Q$, generally, both potential outcomes of outcome variable $y$ are not independent of treatment variable $z$. In formal notation this is stated as


$$ y^0, y^1 \not\!\prep z|X,Q$$


### Recap

Including post-treatment variables as predictors in a causal analysis should be avoided. As we saw in the bugs example, including post-treatment variables can distort causal analyses and lead you to incorrect conclusions. Post-treatment variables are any variables that occurred after the treatment was assigned and could possibly be influenced by the treatment. Statistical models with post-treatment variables make unfair comparisons and are biased because models do not have access to the potential outcomes of those variables. 

### Related modules
You can learn more about bias is [here]() 

You can learn more about simulation is [here]() 

You can learn more about potential outcomes [here]()

You can learn more about what makes a comparison fair or unfair [here]()

### Citations and Readings


