# Causal estimands

Let's revisit the HyperShoe example from the potential outcomes module; a new high performance running shoe called the HyperShoe is released with the claim that wearing the shoe causes faster marathon running times than standard running shoes. We have collected data from 10 runners. 5 of the runners wore HyperShoes and 5 of the runners wore standard shoes.

What might we want to learn about the effect of the shoes on these runners? Ideally we'd like to know, for each runner, if the HyperShoe would reduce their running time in a given race. But this is an extremely difficult task. What if we instead considered the effect of the HyperShoe across all ten runners? 

The **average treatment effect (ATE)** provides the average of all of the individual level causal effects of HyperShoes across all 10 of the runners in our sample. We saw in the potential outcomes module that there can be individual differences in the causal effect of HyperShoes. Some runners had a huge benefit from HyperShoes while others had no benefit at all. The average treatment effect masks these individual level differences and instead gives us an idea of the general trend. We work with average treatment effects because they can be estimated with more precision.

**The average treatment effect still relies on within-person comparisons.** This is different from simply comparing the average running times between the observed treatment group and the observed control group. We'll walk through calculating the ATE to make these ideas more concrete.

<br>
