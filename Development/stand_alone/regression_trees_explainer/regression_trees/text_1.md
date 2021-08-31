# BART vs. regression trees

Bayesian Additive Regression Trees (BART) often leads to comparison to random forest and other regression tree methods. This explainer piece describes the difference between BART and standard regression trees

## What are regression trees?

Binary decision trees create an interpretable decision making framework for making a single prediction. Suppose a patient comes into your clinic with chest pain and you wish to diagnose them with either a heart attack or not a heart attack. A simple framework of coming to that diagnosis could look like the following. 

<p align="center">
<img src="diagram.png" width=40%>
</p>

Note that each split results in two outcomes (binary) and every possible condition leads to a terminal node. The model's splits can also be visualized has partitioning the parameter space. Since the decision tree makes binary splits along a parameter, the resulting boundaries will always be rectangular.

<p align="center">
<img src="diagram_parameters.png" width=40%>
</p>

Try splitting the parameter space below to optimize the number of correct classifications. Refresh the data to see how your splits perform on new data. All data is generated from the same data generating function.

<br><br>