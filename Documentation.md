# Documentation

Detailed documentation for each class of the r-clustering-variables package

## ClustVarKMeans 
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
Fit the K-means model on variables.
Premptively test each the arguments and generates an error message in the apppropriate case  : X must be a data.frame or a matrix, all the variables must be numeric, 
X must not contain missing values and K must be between 2 and the number of variables in X.
Prepare the data by normalizing, so all variables are comparable.
Compute the distance matrix between variables, either based on the correlation or the euclidean method (depending on the parameter used).

The function uses mutli-starts for stability, with a first center determined randomly.
The function then uses the LLOYD-MAX algorithm for K-Means for the assignment of the variables to the clusters and their updates, before storing the best results.

##### Usage
ClustVarKMeans.fit(X, K=3, method="correlation",...)

##### Arguments
X : A data.frame or matrix with numeric variables to cluster.
##### Value
Returns the object itself (invisibly) for method chaining.


#### Predict
##### Description
Predict cluster membership for new variables.
Pre-emptively checks that the model as previously been fitted, aand that the new variables respects all the criteria as for the fit() function.

##### Usage
ClustVarKMeans.predict(newdata, scaling="...")

##### Arguments
newdata : A data.frame or matrix with numeric variables to classify (observations as rows)
scaling : How to scale 'newdata': either `"self"` (scale newdata independently), `"training"` (use training scaling), or `"none"` (no scaling)

##### Value
Returns : a data.frame with variable names, assigned clusters, and distances

#### Print
##### Description
Print brief model information

##### Usage
Model <- ClustVarKMeans.fit(X, K=3, method="correlation",...)
Model.print()

##### Value
Returns the clusters and which variables are comprised in them.


#### Summary
##### Description
Print detailed model summary

##### Usage
Model <- ClustVarKMeans.fit(X, K=3, method="correlation",...)
Model.summary()

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
Model <- ClustVarKMeans.fit(X, K=3, method="correlation",...)
Model.get_center_variables()

##### Value
Returns a character vector of center variable names


#### elbow_method
##### Description
Automatically determine optimal K using the distance-to-line method

##### Usage
ClustVarKMeans.elbow_method(X, K_min, K_max, plot=TRUE)

##### Arguments
X : A data.frame or matrix with numeric variables
K_min : integer, minimum number of clusters to test (default: 2)
K_max : integer, maximum number of clusters to test (default: 10)
plot : boolean, indicates whether to plot the elbow curve (default: TRUE)

##### Value
Retuns the optimal numbers of clusters.
In the case where "plot=TRUE", the function also returns a plot to visualize the evolution of the model's inertia according to the number of clusters.


## ClustVarHAC *details*

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
ClustVarHAC.fit(X, K, method, linkage_method)

##### Arguments
X : A data.frame or matrix containing only numeric variables to cluster.

##### Value
Returns the object itself (invisibly) for method chaining.

#### Predict *attach variables in X (illustrative variables) to the best cluster*
##### Description
Predicts the cluster membership for new, illustrative numeric variables based on their mean absolute correlation to the existing clusters.

##### Usage
ClustVarHAC.predict(newdata)

##### Arguments
newdata : A data.frame or matrix of new numeric variables (must have the same number of observations as the training data, used for the fit function).

##### Value
Returns a data.frame with the variable name, assigned cluster ID, and the maximal average absolute correlation score.


#### Plot
##### Description
Draws the dendrogram resulting from the HAC.

##### Usage
Model <- ClustVarHAC.fit(X)
model.plot(K)

##### Arguments
K: integer, the number of clusters to highlight on the plot (defaults to the model's K).

##### Value
Generates a dendrogram, representing the clustered variables.


#### Print
##### Description
Prints a concise summary of the HAC model parameters and status.
##### Usage
Model <- ClustVarHAC.fit(X)
Model.print()

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
Model <- ClustVarHAC.fit(X)
Model.summary()

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
Fit the clustering model on the active variables.
Premptively test each the arguments and generates an error message in the apppropriate case  : X must be a data.frame, all the variables must be categorical(type factor), 
X must not contain missing values and K must be between 2 and the number of variables in X.
Clusters are initialized using a balanced round-robin initialization, then shuffle, to reduce the chance of empty clusters at the start.
The axis of each cluster is calculated thanks to the synthetic variable of each cluster.
For clusters with multiple variables: the function uses the first principal component (FPC) from FactoMineR::MCA.
Then the algorithm reallocates each variable through Khi² test score calculation and assigning each variable to the cluster with the maximum association score.

The algorithm determines a criterion Q which constitutes the sum of the maximum association scores. It indicates the inertia of th model and therefore, intra-cluster quality.

##### Usage
ClustVarACM.fit(X)

##### Arguments
X : A data.frame with categorical variables (factors) to cluster.

##### Value
Returns the object itself (invisibly) for method chaining.


#### Function *details*
##### Description
##### Usage
##### Arguments
##### Value
