Running a randomized experiment is the most effective way to ensure balance and overlap between the treatment and control groups. You may have heard that confounders can lead to biased and incorrect causal conclusions. A confounder is a variable that is associated with **both** the treatment variable and the outcome variable. When a treatment is randomly assigned, we would not expect any variables to be associated with the treatment, therefor, we do not expect any confounders in our experiment!  This has profound implications for how we analyze randomized experiments: we can make fewer assumptions compared to when we are analyzing data from observational studies.

Let's consider a hypothetical randomized study where 200 runners were randomly assigned to the treatment condition, wearing high performance HyperShoes, or the control condition, wearing standard shoes. The goal of our study is to understand if HyperShoes cause runners to run faster than they would have ran had they worn standard shoes. In addition to outcome variable Y (Running Time) and treatment variable Z (HyperShoes), each runners age was recorded. 

<br>

#### Causal Estimands for Randomized Experiments
Before we begin analyzing our data, we'll need to consider which causal estimand we are estimating. When the treatment is randomly assigned, we expect the Average Treatment Effect (ATE), Average Treatment Effect of the Treated (ATT) and Average Treatment Effect of the Control (ATC) to be equal. 

<br>
<br>
