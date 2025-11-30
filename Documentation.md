# Documentation

Detailed documentation for each class of the r-clustering-variables package

## ClustVarKMeans *Simple K-Means Clustering for Variables (Educational Implementation)*
### Description
R6 class which proposes a simplified version of K-means for variable clustering
### Parameters
K : integer, number of clusters (default: 3)
method : distance method used, either "correlation" or "euclidean" (default: "correlation")
max_iter : integer, maximum number of iterations for convergence (default: 100)
nstart : integer, number of random starts used to initialize centroids (default: 10)
seed : optional seed for reproducibility

### Functions and methods

#### Fit 
##### Description
Fits the K-means model on variables.
Premptively tests each the arguments and generates an error message in the apppropriate case  : X must be a data.frame or a matrix, all the variables must be numeric, 
X must not contain missing values and K must be between 2 and the number of variables in X.
Prepares the data by normalizing, so all variables are comparable.
Computes the distance matrix between variables, either based on the correlation or the euclidean method (depending on the parameter used).

The function uses mutli-starts for stability, with a first center determined randomly.
The function then uses the LLOYD-MAX algorithm for K-Means for the assignment of the variables to the clusters and their updates, before storing the best results.

##### Usage
model <- ClustVarKMeans$new(K = 3, method = "correlation")
model$fit(X)

##### Arguments
X : A data.frame or matrix with numeric variables to cluster.
##### Value
Returns the object itself (invisibly) for method chaining.


#### Predict
##### Description
Predicts cluster membership for new variables.
Pre-emptively checks that the model as previously been fitted, aand that the new variables respects all the criteria as for the fit() function.

##### Usage
model <- ClustVarKMeans$new(K = 3, method = "correlation")
model$predict(newdata, scaling="...")

##### Arguments
newdata : A data.frame or matrix with numeric variables to classify (observations as rows)
scaling : How to scale 'newdata': either `"self"` (scale newdata independently), `"training"` (use training scaling), or `"none"` (no scaling)

##### Value
Returns : a data.frame with variable names, assigned clusters, and distances

#### Print
##### Description
Prints brief model information

##### Usage
model <- ClustVarKMeans$new(K = 3, method = "correlation")
print(model)

##### Value
Returns the clusters and which variables are comprised in them.


#### Summary
##### Description
Prints detailed model summary

##### Usage
Model <- ClustVarKMeans$fit(X, K=3, method="correlation",...)
summary(Model)

##### Value
Presents :
* the number of clusters,
* the distance method used,
* the size of each cluster,
* the variables attributed to each cluster
* a data.frame representing the medoids (center variable of each cluster
* the inertia of the model
  

#### get_center_variables
##### Description
Gets the center variable (medoid) for each cluster.

##### Usage
Model <- ClustVarKMeans$fit(X, K=3, method="correlation",...)
Model$get_center_variables()

##### Value
Returns a character vector of center variable names


#### elbow_method
##### Description
Automatically determines optimal K using the distance-to-line method

##### Usage
ClustVarKMeans$elbow_method(X, K_min, K_max, plot=TRUE)

##### Arguments
X : A data.frame or matrix with numeric variables
K_min : integer, minimum number of clusters to test (default: 2)
K_max : integer, maximum number of clusters to test (default: 10)
plot : boolean, indicates whether to plot the elbow curve (default: TRUE)

##### Value
Retuns the optimal numbers of clusters.
In the case where "plot=TRUE", the function also returns a plot to visualize the evolution of the model's inertia according to the number of clusters.

#### plot *Draw the clustering results (heatmap or representativeness)*
##### Description
Generates visual representations of the clustering results using \code{ggplot2}.
This function requires the \code{ggplot2} and \code{reshape2} packages to be installed.


##### Usage
Model <- ClustVarKmeans$fit(X)
Model$plot(type=c("heatmap", "representativeness"))

##### Arguments
type : Character string, indicating the type of plot to generate: either "heatmap" or "representativeness"
  
##### Value
* With the argument "type="heatmap"", the function displays a correlation matrix heatmap with variables ordered by cluster, with colors varying based on the metrics used. This is useful for visually confirming intra-cluster cohesion and inter-cluster separation. 
* With the argument "type="representativeness"", the function displays a bar chart showing each variable's similarity (absolute correlation or normalized similarity, depending on the method used in the fit() function) to its assigned cluster center.. This helps identify the core variables within each cluster.



## ClustVarHAC *Hierarchical Agglomerative Clustering (HAC) of variables*

### Description
This class wraps the HAC algorithm for clustering numeric variables. It uses the base R function stats::hclust() on a distance matrix computedbetween the variables.

### Parameters
K : Integer, the desired number of clusters.(Default: K=2)
method : Character string, the distance metric to use: "correlation" (1 - |r|) or "euclidean" (on transposed data).(Default : method="correlation")
linkage_method : Character string, the agglomeration (linkage) method for stats::hclust : e.g., "ward.D2", "complete", "average".(Default : linkage_method="ward.D2")
  

### Functions and methods
#### Fit
##### Description
Fits the HAC model by calculating the distance matrix between variables and performing hierarchical clustering.
Premptively test each the arguments and generates an error message in the apppropriate case  : X must be a data.frame or a matrix, all the variables must be numeric, 
X must not contain missing values and K must be between 2 and the number of variables in X.
Prepare the data by normalizing, so all variables are comparable.
Compute the distance matrix between variables, either based on the correlation or the euclidean method (depending on the parameter used).
Apply stats::hclust and cut the tree based on K.

##### Usage
ClustVarHAC$fit(X, K, method, linkage_method)

##### Arguments
X : A data.frame or matrix containing only numeric variables to cluster.

##### Value
Returns the object itself (invisibly) for method chaining.


#### Predict *attach variables in X (illustrative variables) to the best cluster*
##### Description
Predicts the cluster membership for new, illustrative numeric variables based on their mean absolute correlation to the existing clusters.

##### Usage
ClustVarHAC$predict(newdata)

##### Arguments
newdata : A data.frame or matrix of new numeric variables (must have the same number of observations as the training data, used for the fit function).

##### Value
Returns a data.frame with the variable name, assigned cluster ID, and the maximal average absolute correlation score.


#### Plot
##### Description
Plot  different visualizations for the HAC model.
This function requires the ggdendro, ggplot2 and reshape2 packages to be installed.

##### Usage
Model <- ClustVarHAC$fit(X)
model$plot(type =c("dendrogram", "heights", "heatmap", "representativeness"),... )

##### Arguments
type : character string, designing the type of plot: "dendrogram", "heights", "heatmap", or "representativeness" (Default : type="dendrogram").

##### Value
Generates a gg2plot, depending on the chosen "type" :
* "dendrogram" : displays the hierarchical tree with the cut line for the selected K
* "heights" : plots the fusion heights of the agglomerative steps (similar to a scree plot). This helps in visually selecting the optimal \code{K} based on the "elbow" rule.
* "heatmap" : displays the correlation matrix of the variables, ordered by their cluster assignment. This confirms intra-cluster cohesion.
* "representativeness" : bIdentic stylear chart showing each variable's similarity (correlation or normalized distance) to its calculated cluster centroid. Variables with high scores are considered the most representative of their cluster.


#### Print
##### Description
Prints a concise summary of the HAC model parameters and status.
##### Usage
Model <- ClustVarHAC$fit(X)
print(Model)

##### Value
Returns  : 
* the number of cluster K
* the distance method used
* the linkage method used
* if the model has been fitted or not
* the number of variables in each cluster.


#### Summary
##### Description
Displays a detailed summary including model parameters, the final cluster composition, and the cophenetic correlation.

##### Usage
Model <- ClustVarHAC$fit(X)
summary(Model)

##### Value
Returns :
* the number of cluster K
* the distance method used
* the linkage method used
* if the model has been fitted or not
* the number of variables in each cluster.
* the list of variables in each cluster


## ClustVarACM *Clustering for categorical variables using MCA*

### Description
This R6 class implements the iterative algorithm for clustering categorical variables using MCA (Multiple Correspondence Analysis) as a method to define the cluster's synthetic axis. It aims to maximize the association (measured by Khi-squared) between each variable and its cluster's axis.

### Parameters
K : integer, number of clusters (Default : K= 3).
max_iter : integer, maximal number of iterations for the clustering algorithm (Default : 30).
tol : numeric, tolerance for stop criterion (Default : 1e-4).

    
### Functions and methods
#### Fit
##### Description
Fits the clustering model on the active variables.
Premptively test each the arguments and generates an error message in the apppropriate case  : X must be a data.frame, all the variables must be categorical(type factor), 
X must not contain missing values and K must be between 2 and the number of variables in X.
Clusters are initialized using a balanced round-robin initialization, then shuffle, to reduce the chance of empty clusters at the start.
The axis of each cluster is calculated thanks to the synthetic variable of each cluster.
For clusters with multiple variables: the function uses the first principal component (FPC) from FactoMineR::MCA.
Then the algorithm reallocates each variable through Khi² test score calculation and assigning each variable to the cluster with the maximum association score.

The algorithm determines a criterion Q which constitutes the sum of the maximum association scores. It indicates the inertia of th model and therefore, intra-cluster quality.

##### Usage
ClustVarACM$fit(X)

##### Arguments
X : A data.frame with categorical variables (factors) to cluster.

##### Value
Returns the object itself (invisibly) for method chaining.


#### Print
##### Description
Prints a succinct summary of the model.
##### Usage
Model <- ClustVarACM$fit(X)
print(Model)

##### Value
Returns  : 
* the number of variables
* the number of clusters
* the final criterion Q (the model's inertia)


#### Summary
##### Description
Displays a detailed summary of the clustering results.

##### Usage
Model <- ClustVarACM$fit(X)
summaru(Model)

##### Value
Returns : 
* the number of variables
* the number of clusters
* the final criterion Q (the model's inertia)
* the average quality of the clustering (=Q/p, where p=number of variables)
* the number of iterations actualy performed by the algorithm
* a data frame representing each variable and the cluster to which it has benn allocated
* a synthesis of the number of variables per cluster


#### Select_K
##### Description
Automatic selection for the optimal number of clusters K using the elbow method on the Q criterion.

##### Usage
ClustVarACM$select_K(X, K_grid = 2:6, threshold=0.1)

##### Arguments
X : A data.frame with categorical variables (factors) to cluster.
K_grid : Integer vector of K values to test (e.g., 2:6).
threshold : Numeric, tolerance for the relative gain in Q (default 0.1, or 10%).

##### Value
Returns a list containing the data.frame of the results (K and Q values) and the suggested optimal K, with a plot visualizing Q convergence.


#### Predict
##### Description
Predicts cluster membership for new illustrative variables.
The new variables are not used to modify the clusters (non-active).

##### Usage
Model <- ClustVarACM$fit(X, K)
model$predict(newdata)

##### Arguments
newdata : A data.frame with categorical variables to classify. It must have the same number of observations as the training data used with the fit() function.

##### Value
Returns A data.frame with variable names and their assigned cluster.


#### Plot
##### Description
Plots visualizations for the ACM clustering model.
The plots visualize the association (measured by Cramer's V or 1 - p.value) between each variable and the generated clusters.
This function requires the ggplot2, ggrepel (for biplot), and reshape2 packages.

##### Usage
Model <- ClustVarACM$fit(X)
Model$plot(type = c("biplot", "representativeness", "heatmap"), axes = c(1, 2))

##### Arguments
type : Character string indicating the type of plot to generate. Must be one of: "biplot", "representativeness" or "heatmap".
axes : Numeric vector of length 2, specifying the cluster axes to plot (e.g.,c(1, 2) for the first two cluster dimensions in the biplot).

##### Value
Generates a plot or display depending on the selected type : 
* "biplot" : A scatter plot projecting variables onto two specified cluster axes, visualizing their associations (e.g., to Cluster 1 vs Cluster 2).
* "representativeness" : A bar chart showing the association strength (Cramer's V or Score) of each variable to its assigned cluster. Useful for identifying core variables.
* "heatmap" : A matrix plot visualizing the association strength between all variables and all clusters. Variables are typically ordered by their cluster group.
