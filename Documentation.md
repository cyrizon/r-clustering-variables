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
Compute the distance matrix between variables, either based on the correlation or the euclidean method (depending on the parameter used)

##### Usage
##### Arguments
X : A data.frame or matrix with numeric variables to cluster
##### Value

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
