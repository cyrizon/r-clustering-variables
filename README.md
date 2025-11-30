# clustVarACC: Variable Clustering with R6 Classes

[![R](https://img.shields.io/badge/R-%3E%3D4.1-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## 📋 Overview

The **clustVarACC** package provides three R6-based algorithms for clustering **variables** (rather than observations). Variable clustering is useful for:

- **Reducing multicollinearity** in regression models
- **Feature engineering**: creating representative variables for each group
- **Exploratory data analysis**: understanding variable relationships and structure
- **Dimension reduction**: grouping redundant variables together

## 🎯 Algorithms

The package includes three complementary clustering approaches:

### 1. **ClustVarKMeans** - K-Means for Numeric Variables

- Fast and efficient for large datasets
- Uses correlation or Euclidean distance
- Integrated elbow method for automatic K selection
- K-means++ initialization for stability

### 2. **ClustVarHAC** - Hierarchical Agglomerative Clustering

- Reveals hierarchical structure of variable relationships
- Multiple linkage methods (Ward, complete, average, single)
- Visual K selection via dendrogram
- Ideal for exploratory analysis

### 3. **ClustVarACM** - Multiple Correspondence Analysis Clustering

- Designed for **categorical variables**
- Uses MCA to define cluster synthetic axes
- Maximizes χ² associations between variables and cluster axes
- Iterative convergence algorithm

## 📦 Installation

```r
# Install from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Standard installation
remotes::install_github("cyrizon/r-clustering-variables")

# Or install with vignettes for full documentation
remotes::install_github("cyrizon/r-clustering-variables", build_vignettes = TRUE)
```

## 🚀 Quick Start

```r
library(clustVarACC)

# Example with numeric variables (K-Means)
data(mtcars)
X <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]

model <- ClustVarKMeans$new(K = 3, method = "correlation")
model$fit(X)

# View results
print(model)
model$plot(type = "heatmap")
model$plot(type = "representativeness")

# Automatic K selection
optimal_k <- model$elbow_method(X, K_max = 8, plot = TRUE)
```

### ⚠️ Data Preparation

Before clustering, ensure your data has the correct variable types:

**K-Means/HAC** (numeric algorithms):

```r
# Verify all columns are numeric
all(sapply(your_data, is.numeric))

# Keep only numeric columns
numeric_data <- your_data[, sapply(your_data, is.numeric)]
```

**ACM** (categorical algorithm):

```r
# Verify all columns are factors
all(sapply(your_data, is.factor))

# Convert all columns to factors
your_data[] <- lapply(your_data, as.factor)
```

📖 **For detailed data preparation and troubleshooting**, see the package vignette.

## 📖 Documentation

For comprehensive usage instructions, examples, and best practices, see the package vignette:

```r
# Install with vignettes (recommended)
remotes::install_github("cyrizon/r-clustering-variables", build_vignettes = TRUE)

# Then view the vignette
vignette("intro_package", package = "clustVarACC")
browseVignettes("clustVarACC")
```

**Note**: Building vignettes adds a few minutes to installation time but provides comprehensive documentation. Alternatively, view the vignette source directly: `vignettes/intro_package.Rmd` in the repository.

The vignette covers:

- Detailed examples for all three algorithms (K-Means, HAC, ACM)
- Automatic K selection with elbow method
- Visualization options (heatmaps, dendrograms, biplots)
- Prediction on new variables
- Method comparison and best practices
- Troubleshooting guide

## 🎨 Interactive Shiny Application

The package includes a user-friendly Shiny app for interactive analysis without coding:

```r
library(clustVarACC)
shiny::runApp(system.file("shinyApp", package = "clustVarACC"))
```

**Features:**

- Data upload and preview
- All three algorithms with parameter tuning
- Automatic K detection
- Multiple visualization types
- Clustering metrics (silhouette, homogeneity, separation)
- Prediction on new variables
- Export results (CSV) and plots (ZIP)

See `inst/shinyApp/README.md` for detailed app documentation.

## 🔧 Key Features

- **Unified R6 interface**: All algorithms share `fit()`, `predict()`, `plot()` methods
- **Flexible distance metrics**: Correlation (pattern-based) or Euclidean (scale-sensitive)
- **Rich visualizations**: Heatmaps, dendrograms, biplots, representativeness plots
- **Automatic K selection**: Elbow method with curvature detection (K-Means)
- **Method chaining**: Concise syntax with chainable methods
- **Prediction**: Classify new variables into existing clusters
- **Interactive app**: Explore algorithms without coding

## 📚 Example Datasets

The package includes several example datasets:

- `mtcars`, `iris`: Standard R datasets (numeric)
- `HairEyeColor`: Categorical data for ACM
- `tea_data`: 300 observations, 18 categorical variables (tea consumption patterns)
- College data: Available via Shiny app example loader

## 👥 Authors

**Cyrille PECNIK**, **Constantin REY-COQUAIS**, **Anne-Camille VIAL**

Developed as part of M2 SISE 2025-2026 project at Université Lyon 2.

**Supervisor**: Ricco RAKOTOMALALA

## 📄 License

MIT License - see LICENSE file for details

## 🐛 Issues & Contributions

Report bugs or suggest features on [GitHub Issues](https://github.com/cyrizon/r-clustering-variables/issues).
