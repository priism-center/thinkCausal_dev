<br>

In practice, the splits are determined algorithmically via an impurity index. With each split, the algorithm maximizes the purity of the resulting splits. If a potential split results in classes `[HA, HA]` and [`NHA`, `NHA`] then that is chosen over another potential split that results `[HA, NHA]` and `[NHA, HA]`. At each node, all possible splits are tested and the split that maximizes purity is chosen.

For classification problems, a commonly used metric is [Gini impurity](https://en.wikipedia.org/wiki/Decision_tree_learning#Gini_impurity). Gini impurity is `2 * p * (1 - p)` where `p` is the fraction of elements labeled as the class of interest. A value of `0` is a completely homogeneous vector while `0.5` is the inverse. The vector `[NHA, HA, NHA]` has a Gini value of `2 * 1/3 * 2/3 = 0.444`. Since the data is split into two vectors, the value is weighted by the respective lengths of the two vectors. 



## What is random forest?

Eget duis at tellus at urna. In arcu cursus euismod quis viverra nibh. At erat pellentesque adipiscing commodo. In hac habitasse platea dictumst quisque sagittis purus sit amet. Tortor pretium viverra suspendisse potenti nullam ac tortor vitae purus. Interdum velit laoreet id donec ultrices. Vitae turpis massa sed elementum. Dui nunc mattis enim ut. In pellentesque massa placerat duis ultricies lacus sed turpis tincidunt. Nunc sed velit dignissim sodales ut eu sem integer.


## How is BART different?

Donec enim diam vulputate ut pharetra sit. Morbi tincidunt augue interdum velit euismod in pellentesque massa placerat. Et pharetra pharetra massa massa ultricies mi quis hendrerit dolor. Vel quam elementum pulvinar etiam non quam lacus suspendisse. Duis convallis convallis tellus id interdum velit laoreet. Convallis a cras semper auctor neque vitae tempus quam. Sed libero enim sed faucibus turpis in eu mi. At ultrices mi tempus imperdiet. Nunc pulvinar sapien et ligula ullamcorper malesuada proin libero. Ipsum faucibus vitae aliquet nec. Tristique senectus et netus et malesuada. Elementum curabitur vitae nunc sed velit dignissim sodales ut eu. Mattis rhoncus urna neque viverra justo. Mattis aliquam faucibus purus in.


## Conclusion

Nibh sed pulvinar proin gravida. Cursus in hac habitasse platea. Sapien nec sagittis aliquam malesuada bibendum arcu vitae. Tortor at risus viverra adipiscing. Nisl rhoncus mattis rhoncus urna neque viverra justo nec ultrices. Nibh sed pulvinar proin gravida hendrerit lectus. Pretium nibh ipsum consequat nisl vel pretium lectus. Mi bibendum neque egestas congue quisque egestas. 
