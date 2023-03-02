
## Subsetting by treatment assignment

Sometimes researchers are only interested in summarizing causal effects for a particular set of observations. For instance, it may make sense to focus on groups deemed most likely to receive the treatment or on observations where we have the most data for estimating a treatment effect. In observational studies, where the treatment is not randomly assigned, individuals that received the treatment are often very different from individuals that did not receive the treatment. These differences may influence average causal effects!

Let’s return to our original sample of 10 runners from the HyperShoe study. In our study, 5 runners received the treatment and wore HyperShoes while 5 did not receive the treatment and wore standard shoes.

### The Average Treatment Effect on the Treated (ATT)
The average treatment effect on the treated (ATT) is the average causal effect of the sample that actually received the treatment (Z = 1 in potential outcomes notation). We can calculate the ATT the same way as we did the ATE, but for the ATT we only include potential outcomes from runners in the treatment group (Z = 1). 

**Notice we are only averaging over the potential outcomes of runners in the treatment group!**

<div id='estimands-plot-ATT'></div>

## Subsetting by the control group

The average treatment effect on the control (ATC) is the average causal effect of the sample with received the treatment (Z = 0 in potential outcomes notation). We can calculate the ATC the same way as we did the ATE, but for the ATC we only include potential outcomes from runners in the control group (Z = 0). 

**Notice we are only averaging over the potential outcomes of runners in the control group!**

<div id='estimands-plot-ATC'></div>


## Practice
