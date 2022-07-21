
## Subsetting by treatment assignment

Sometimes researchers are only interested in summarizing causal effects for a particular set of observations. For instance, it may make sense to focus on groups deemed most likely to receive the treatment or on observations where we have the most data for estimating a treatment effect. In observational studies, where the treatment is not randomly assigned, individuals that received the treatment are often very different from individuals that did not receive the treatment. These differences may influence average causal effects!

### The Average Treatment Effect on the Treated (ATT)

Lets return to our original sample of 10 runners. Imagine that the 5 runners who wore HyperShoes (colored in purple) were all professional runners. This explains why 

You may have noticed that the runners who wore HyperShoes as their factual outcome 


were noticeably faster than the runners who did not wear HyperShoes. This is true for both of there potential outcomes y0 and y1! 

Sometimes researchers are only interested in summarizing causal effects for a particular set of observations. In observational studies, where the treatment is not randomly assigned, individuals that received the treatment are often very different from individuals that did not receive the treatment. It may make sense to focus on group that received the treatment.

Let’s return to our original sample of 10 runners from the HyperShoe study. In our study, 5 runners received the treatment and wore HyperShoes while 5 did not receive the treatment and wore standard shoes. Suppose that all 5 runners that wore HyperShoes are semi-professional runners while only 2 of the 5 runners wearing standard shoes are semi-professionals and the remaining 3 are amateur-runners.

The average treatment effect on the treated (ATT) is the average causal effect of the sample with received the treatment (where z = 1 and y = y1 in potential outcomes notation). In our example, the ATT is the average casual effect for the runners who wore HyperShoes for their factual outcome.

We can calculate the ATT by taking averaging the individual causal effect of the 5 runners that received the treatment (z = 1) or by taking the difference between the average y1 and the average y0 for the group of runners that received the treatment (z = 1).


<div id='estimands-plot-ATT'></div>

## Subsetting by the control group

In our last example we were interested in a causal question that only pertained to the treated group. We can ask similar questions about the control group. Imagine a different scenario, the governing body that organized marathon races is concerned that runners without HyperShoes are put at a disadvantage. How would the governing body know if runners without HyperShoes would have ran faster had they worn HyperShoes?

The average treatment effect on the control (ATC) is the average causal effect of the sample that did not receive the treatment (where z = 0). Calculating the ATC would answer if the runners without HyperShoes were at a disadvantage

Why do we only care about comparisons within the control group? Here we want to know if the group of runners without HyperShoes is disadvantaged without using HyperShoes. It is possible that the HyperShoes only have an effect on the runners that happened to be in the treated group (z = 1) to see if the runners without HyperShoes were disadvantaged we would need to compare the factual and counterfactual outcomes (y1 and y0) for the runners without HyperShoes (z = 0).

<div id='estimands-plot-ATC'></div>

