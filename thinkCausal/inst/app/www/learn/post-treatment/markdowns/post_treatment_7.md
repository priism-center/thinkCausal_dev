## How does including post-treatment variables create bias?

Comparisons that include post-treatment variables are unfair because post-treatment variables have different potential outcomes that depend on the treatment variable. We’ll look at the fertilizer example to illustrate why this is problematic. 

Let’s suppose that receiving fertilizer caused plants to produce 2 pounds more tomatoes than they would have produced without receiving the fertilizer. Imagine that we could access the potential outcomes of both `bugs` and `pounds of fruit` for 3 of the plants from our study. We would be able to see both the values of `bugs` and `pounds of fruit` we observed as well as what those values would have been had each plant had a different value of `fertilizer`.

When we can see all potential outcomes, it is clear that comparing Plant 1 & 2 is fair because they are equal across all potential outcomes. In contrast, comparing Plant 1 & 3 is not a fair comparison because they are different across potential outcomes.

<br>
<table class="table">
  <thead>
    <tr>
      <th scope="col">Plant</th>
      <th scope="col">z</th>
      <th scope="col">bugs</th>
      <th scope="col">bugs if z = 0</th>
      <th scope="col">bugs if z = 1</th>
      <th scope="col">pounds of fruit</th>
      <th scope="col">pounds of fruit if z = 0</th>
      <th scope="col">pounds of fruit if z = 1</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>1</td>
      <td>0</td>
      <td>1</td>
      <th scope="row">1</th>
      <th scope="row">0</th>
      <td>4</td>
      <th scope="row">4</th>
      <th scope="row">6</th>
    </tr>
    <tr>
      <td>2</td>
      <td>1</td>
      <td>0</td>
      <th scope="row">1</th>
      <th scope="row">0</th>
      <td>6</td>
      <th scope="row">4</th>
      <th scope="row">6</th>
    </tr>
    <tr>
      <td>3</td>
      <td>1</td>
      <td>1</td>
      <td>1</td>
       <td>1</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
    </tr>
  </tbody>
</table>
<br>


Recall that we can never observe both potential outcomes and only have access to the observed value determined by the treatment (`z`). If we control for the observed value of `bugs`, Plant 2 and Plant 3 no longer appear similar! Rather than making a fair comparison between Plant 1 & Plant 2, a statistical model that controls for the observed value of `bugs` would make the unfair comparisons between Plant 1 & Plant 3 which we know is an unfair comparison! 

<br>
<table class="table">
  <thead>
    <tr>
      <th scope="col">Plant</th>
      <th scope="col">z</th>
      <th scope="col">bugs</th>
      <th scope="col">bugs if z = 0</th>
      <th scope="col">bugs if z = 1</th>
      <th scope="col">pounds of fruit</th>
      <th scope="col">pounds of fruit if z = 0</th>
      <th scope="col">pounds of fruit if z = 1</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>1</td>
      <td>0</td>
      <th scope="row">1</th>
      <th scope="row">1</th>
      <th scope="row">0</th>
      <td>4</td>
      <th scope="row">4</th>
      <th scope="row">6</th>
    </tr>
    <tr>
      <td>2</td>
      <td>1</td>
      <td>0</td>
      <td>1</td>
      <td>0</td>
      <td>6</td>
      <td>4</td>
      <td>6</td>
    </tr>
    <tr>
      <td>3</td>
      <td>1</td>
      <th scope="row">1</th>
      <th scope="row">1</th>
      <th scope="row">1</th>
      <td>1</td>
      <th scope="row">1</th>
      <th scope="row">3</th>
    </tr>
  </tbody>
</table>
<br>

This is not a problem for pre-treatment variables because it is impossible for them to depend upon the treatment. Put another way, pre-treatment variables do not have potential outcomes. Suppose instead of controlling for the post-treatment variable `bugs`, we had a pre-treatment variable `bugs_pre` which measured whether or not plants had bugs before the experiment began. A table with all potential outcomes for an analysis with only pre-treatment variables would look like this: 

<br>
<table class="table">
  <thead>
    <tr>
      <th scope="col">Plant</th>
      <th scope="col">z</th>
      <th scope="col">bugs_pre</th>
      <th scope="col">pounds of fruit</th>
      <th scope="col">pounds of fruit if z = 0</th>
      <th scope="col">pounds of fruit if z = 1</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>1</td>
      <td>0</td>
      <th scope="row">0</th>
      <td>4</td>
      <td>4</td>
      <td>6</td>
    </tr>
    <tr>
      <td>2</td>
      <td>1</td>
      <th scope="row">0</th>
      <td>6</td>
      <td>4</td>
      <td>6</td>
    </tr>
    <tr>
      <td>3</td>
      <td>1</td>
       <td>1</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
    </tr>
  </tbody>
</table>
<br>


Note that there are no columns for bugs_pre if z = 0 or bugs_pre if z = 1, the only variable that has potential outcomes is the outcome `pounds of fruit`. 
<br>
<br>
<br>

## Identifying post-treatment variables

Knowing which variables are and are not post-treatment can be challenging. **Post-treatment variables are any variable that has potential outcomes that depend on the treatment variable**. If it is impossible for a variable to have potential outcomes that depend on the treatment that variable is not a post-treatment variable. Generally, there are two strategies you can use to help sort out which variables are pre-treatment and which are post-treatment: 

**Strategy 1: Was the variable measured before the treatment was received?**

Any variable that was measured before the treatment, intervention or exposure was received is automatically not a post-treatment variable. If the variable was measured before the treatment was received it is impossible for that variable to be effected by the treatment. 

In our last example `bugs_pre` is not a post-treatment variable because it was measured before any of the plants received the new fertilizer. 

**Strategy 2: Is it reasonable to conclude that the variable could not be influenced by the treatment?**

Let's think back to our example with the farmer, suppose the farmer's plants consist of 2 varieties of tomatoes: Sungold and Roma but the farmer forgot to record the variety of each plant until the end of the experiment. Even though the variable `variety` was measured after the treatment had been administered `variety` is still not a post-treatment variable!

Since it is impossible that the treatment `fertilizer` could change a Sungold tomato plant into a Roma tomato plant the famrmer could include `variety` as a covaraite in the analysis!

<br>
