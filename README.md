# r-clustering-variables

# Installation
```r
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")

remotes::install_github("cyripon/r-clustering-variables")
```

# Package Presentation
This package contains three R6 classes developped in order to cluster variables : 
- ClustVarKMeans : A simplified, educational version of K-Means for variable clustering. Illustrates basic concepts with medoids and an integrated elbow method. Variables are grouped based on their similarity (correlation or euclidean distance).
- ClustVarHAC : This class wraps the HAC algorithm for clustering numeric variables. It uses the base R function stats::hclust() on a distance matrix computed between the variables.
- ClustVarACM : this one implements the iterative algorithm for clustering categorical variables using MCA (Multiple Correspondence Analysis) as a method to define the cluster's synthetic axis. It aims to maximize the association (measured by Khi-squared) between each variable and its cluster's axis.

The RShiny application allows the user to use each algorithm and its methods and function without having to download the package nor to develop.
Authors : Cyrille PECNIK, Constantin REY-COQUAIS, Anne-Camille VIAL
