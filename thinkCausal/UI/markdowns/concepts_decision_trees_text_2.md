<br>

In practice, the splits are determined algorithmically via an impurity index. With each split, the algorithm maximizes the purity of the resulting splits. If a potential split results in classes `[HA, HA]` and [`NHA`, `NHA`] then that is chosen over another potential split that results `[HA, NHA]` and `[NHA, HA]`. At each node, all possible splits are tested and the split that maximizes purity is chosen.

For classification problems, a commonly used metric is [Gini impurity](https://en.wikipedia.org/wiki/Decision_tree_learning#Gini_impurity). Gini impurity is `2 * p * (1 - p)` where `p` is the fraction of elements labeled as the class of interest. A value of `0` is a completely homogeneous vector while `0.5` is the inverse. The vector `[NHA, HA, NHA]` has a Gini value of `2 * 1/3 * 2/3 = 0.444`. Since the data is split into two vectors, the value is weighted by the respective lengths of the two vectors. 



## Regression trees

Et pharetra pharetra massa massa ultricies mi quis hendrerit dolor. Vel quam elementum pulvinar etiam non quam lacus suspendisse. Duis convallis convallis tellus id interdum velit laoreet.

## Expanding beyond two dimensions

Convallis a cras semper auctor neque vitae tempus quam. Sed libero enim sed faucibus turpis in eu mi. 

## Decision trees for causal inference

Nunc pulvinar sapien et ligula ullamcorper malesuada proin libero. Ipsum faucibus vitae aliquet nec. Tristique senectus et netus et malesuada. Elementum curabitur vitae nunc sed velit dignissim sodales ut eu. Mattis rhoncus urna neque viverra justo. Mattis aliquam faucibus purus in.



