$$\text{weighted average} = \frac{(3.28*250) + (3.28*50) + (2.75*112) + (2.75*612)}{250 + 50 + 112 + 612} = 2.91$$

$$ATE = 2.91$$
<br>
<br>
The weighted average just multiplies each group specific ATE by the number of people in that group. Each of these terms are added together and then divided by the total number of runners. When you use `thinkCausal` to analyze you will never need to compute an weighted average by hand.
<br>
<br>
After conditioning on `professional status`, we now conclude that wearing hyperShoes causes runners to run 2.91 minutes *slower* than they would have finished without wearing hyperShoes. This completely changed our conclusion about hyperShoes. Our new estimate of 2.91 seems considerably more realistic than the company's originally reported estimated effect  of -42.17, **but our new estimate of 2.91 could still be incorrect!!**. There could be many other confounding variables besides `professional status` that we have not accounted for. **For our analysis to be unbiased, we need to account for every confounding variables that exist.** <br>
<br>
Many of the best runners in the world come from outside of the United States and in major marathon races runners from outside of the United States are expected to finish faster than runners from the United States. It is also possible that there are differences in hyperShoe use between runners from the United States and runners from other nations. This makes it likely that whether or not a runner is from the United States is another confounding variable and we are likely still violating ignorability assumption that all counfounders have been adjusted for. 

