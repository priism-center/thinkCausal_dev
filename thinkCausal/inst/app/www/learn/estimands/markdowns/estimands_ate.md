
<h2 id="estimands-trigger-1">Factuals</h2>

We'll start by recording the observed (factual) running times of all 10 runners in our study.
<br><br>
As shown, 5 of the runners wore HyperShoes while the other 5 runners wore standard shoes.


<br><br><br><br><br><br><br><br><br><br><br><br>

<p><br></p>

<h2 id="estimands-trigger-2">Counterfactuals</h2>

Calculating the ATE also requires the unobserved counterfactual running times of all 10 runners. The counterfactual running time is the time each runner would have had if they had worn the other shoe. <br><br>
This part may appear confusing, how can we use counterfactual outcomes if they aren't observed? We'll cover this later on but for now, imagine we had the impossible power to simultaneously observe factual and counterfactual outcomes at the same time.


<br><br><br><br><br><br><br><br><br><br><br><br>

<h2 id="estimands-trigger-3">Individual Causal Effects (ICE)</h2>

When we have both potential outcomes (a y1 and a y0) we can calculate a runners individual causal effect by taking the difference of an individual runner's y1 and y0. The individual causal effect tells us how much faster or slower the HyperShoes casued the runners finishing time to be. <br><br> You can hover over a point on the plot to see the Individual Causal Effect of the selected runner: <a id="estimands-runner-text">Runner 1 has an ICE of -1.69.</a>

<br><br><br><br><br><br><br><br><br><br><br><br>

<h2 id="estimands-trigger-4">The ATE is the average of ICEs</h2>

One way of calculating the ATE is by taking the average of all 10 runners Individual Causal Effect's. After averaging all 10 Individual Causal Effects we see that the ATE is -4.
<br><br>
You can hover over any point to explore a runner's Individual Causal Effect and how it related the the ATE.

<br><br><br><br><br><br><br><br><br><br><br><br>

<h2 id="estimands-trigger-5">The ATE is the average y1 - the average y0</h2>

Another way of calculating the ATE is by taking the difference between the average of all y1s and the average of all y0s.<br><br> It is important to note that y1 and y0 both include factual and counterfactual observations. The values in y1 are finishing times for all 10 runners if they wore HyperShoes and values in y0 are the finishing times of all 10 runners if they wore standard shoes.
<br><br>
Notice the ATE is the same as when we averaged Individual Causal Effects of all 10 runners.


<br><br><br><br><br><br><br><br><br><br><br><br>

<h2 id="estimands-trigger-6">Potential outcomes table</h2>

We can also think of the ATE within a potential outcomes table, you can hover over the graph or the table for a closer look! Using a potential outcomes table has the benefit of displaying the data in way that may be more conducive to making calculations by hand.
<br><br>
The average treatment effect is a summary of the entire samples within person comparisons. Remember you can calculate the ATE by taking the average of all 10 runner Individual Causal Effects or by taking the difference of the average y1 and average y0.
<br><br>
Remember that calculating the ATE requires knowing each runner's factual and counterfactual observations. For now, keep imagining that you have access to both potential outcomes (both y1 and y0) for all 10 runners.


<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>

