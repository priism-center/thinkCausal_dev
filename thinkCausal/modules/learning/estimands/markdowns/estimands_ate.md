
<h2 id="estimands-trigger-1">State 1: Factuals</h2>

<p>We can plot the observed (factual) running timesoutcomes of all 9 runners in our study. In our study, rRunners 1, 4, 7 and 9 wore standard shoes while runners 2, 3, 5, 6, and 8 wore HyperShoes. </p>

<br><br><br><br><br><br><br><br><br><br><br><br>

<p><br></p>

<h2 id="estimands-trigger-2">State 2: Counterfactuals</h2>

<p>Because we have access to our potential outcomes time-machine, we can also plot each runner's unobserved (counterfactual) outcomes. When looking at counterfactual outcomes we see the running times runners 2,3,5,6 and 8 would have had if they were wearing sStandard sShoes which fills in the missing y0's.  We also seeand the running times runners 1, 4, 7 and 9 would have had if they were wearing HyperShoes which fills in the missing y1s.</p>

<br><br><br><br><br><br><br><br><br><br><br><br>

<h2 id="estimands-trigger-3">State 3: ICE</h2>

<p>To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength? The treatment variable <code>z</code> records whether or not an individual participated in the exercise program, the pre-treatment variable. Mid-way through the study, weekly trips to the gym was recorded for each individual with the post-treatment variable <code>gym</code>. </p>

<p><a id="estimands-runner-text">Runner 1 has an ICE of -1.69.</a> Post-treatment variables, like <code>gym</code>, have different <a href="">potential outcomes</a> that depend on the treatment variable. This is the primary problem of controlling for post-treatment variables. The table below shows the <a href="">potential outcomes</a> and observed outcomes of <code>gym</code> for 2 individuals from our exercise study.In real contexts, we would not have access to both <a href="">potential outcomes</a> but we can imagine them for the purposes of this example. Notice that two individuals differ in <code>z</code> but have the same value for <code>gym</code> despite haviving different <a href="">potential outcomes</a>. Controlling for <code>gym</code> is not a fair comparison of <code>strength</code> because we are not accounting for how <code>z</code> changes the value of <code>gym</code>. This is not a problem for pre-treatment varibales becuase thier value can not be changed or influenced by <code>z</code>. </p>

<br><br><br><br><br><br><br><br><br><br><br><br>

<h2 id="estimands-trigger-4">State 4: Mean of individual differences</h2>

<p>To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength? The treatment variable <code>z</code> records whether or not an individual participated in the exercise program, the pre-treatment variable. Mid-way through the study, weekly trips to the gym was recorded for each individual with the post-treatment variable <code>gym</code>. To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength? </p>

<br><br><br><br><br><br><br><br><br><br><br><br>

<h2 id="estimands-trigger-5">State 5: Differences of group means</h2>

<p>To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength? The treatment variable <code>z</code> records whether or not an individual participated in the exercise program, the pre-treatment variable. Mid-way through the study, weekly trips to the gym was recorded for each individual with the post-treatment variable <code>gym</code>. To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength? </p>

<br><br><br><br><br><br><br><br><br><br><br><br>

<h2 id="estimands-trigger-6">State 6: Table</h2>

<p>To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength? The treatment variable <code>z</code> records whether or not an individual participated in the exercise program, the pre-treatment variable. Mid-way through the study, weekly trips to the gym was recorded for each individual with the post-treatment variable <code>gym</code>. To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength? To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength? The treatment variable <code>z</code> records whether or not an individual participated in the exercise program, the pre-treatment variable. Mid-way through the study, weekly trips to the gym was recorded for each individual with the post-treatment variable <code>gym</code>. To demonstrate how adjusting for post-treatment variables introduces bias, consider another hypothetical example: does an exercise program increase muscle strength?  </p>

<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
