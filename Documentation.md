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
X must not contain missing values and K must be between and the number of variables in X.
Prepare the data by normalizing, so all variables are comparable.
Compute the distance matrix between variables, either based on the correlation or the euclidean method (depending on the parameter used).

The function uses mutli-starts for stability, with a first center determined randomly.
The function then uses the LLOYD-MAX algorithm for K-Means for the assignment of the variables to the clusters and their updates, before storing the best results.

##### Usage
ClustVarKMeans.fit(X, K=3, method="correlation",...)

##### Arguments
X : A data.frame or matrix with numeric variables to cluster.
##### Value
Invisible result.


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
### Parameters

### Functions and methods
#### Function *details*
##### Description
##### Usage
##### Arguments
##### Value

## ClustVarACM *details*

### Description
### Parameters

### Functions and methods
#### Function *details*
##### Description
##### Usage
##### Arguments
##### Value
