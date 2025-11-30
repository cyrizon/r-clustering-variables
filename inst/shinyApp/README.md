# Variable Clustering - Shiny Application

Interactive web application for exploring variable clustering algorithms without coding.

## 🚀 How to Run

### From Installed Package (Recommended)

```r
library(clustVarACC)
shiny::runApp(system.file("shinyApp", package = "clustVarACC"))
```

### From Development Mode

```r
# From package root directory
pkg_root <- rprojroot::find_package_root_file()
devtools::load_all(pkg_root)
shiny::runApp(file.path(pkg_root, "inst/shinyApp"))

# Or simply from RStudio
# Open ui.R or server.R and click "Run App"
```

## 📋 Features

### 1. **Data Loading**

- Upload CSV/TSV files with automatic separator detection
- Load example dataset (College data)
- Support for numeric and categorical variables
- Interactive data preview with sorting and filtering

### 2. **Algorithm Selection**

Three complementary algorithms:

- **K-Means**: Fast clustering for numeric variables
- **HAC**: Hierarchical clustering with dendrogram visualization
- **ACM**: Multiple Correspondence Analysis for categorical variables

### 3. **Variable Selection**

- Dynamic variable filtering based on selected algorithm
- Multi-select checkbox interface
- Automatic type detection (numeric/categorical)

### 4. **Clustering Configuration**

- **Distance Method** (K-Means/HAC): Correlation or Euclidean
- **Linkage Method** (HAC): Ward, Complete, Average, Single
- **K Selection**: Manual (2-10) or automatic (elbow method)
- **ACM Parameters**: Max iterations, convergence tolerance

### 5. **Results Visualization**

- Cluster assignments with color-coded labels
- Cluster size distribution
- Variable representativeness (similarity to cluster center)
- Correlation/association heatmaps ordered by clusters
- **HAC-specific**: Dendrogram and fusion heights
- **ACM-specific**: MCA biplot
- **K-Means**: Elbow plot for optimal K selection

### 6. **Clustering Metrics**

- **Homogeneity**: Intra-cluster cohesion
- **Separation**: Inter-cluster distance
- **Silhouette**: Variable assignment quality
- **Cophenetic correlation** (HAC): Dendrogram faithfulness
- **Q criterion** (ACM): Overall partition quality

### 7. **Prediction**

- Classify new variables into existing clusters
- Upload new dataset for prediction
- Interactive results table with distances

### 8. **Export**

- **Download Results**: CSV file with cluster assignments
- **Download Plots**: ZIP file with all generated visualizations
  - Cluster sizes, heatmap, representativeness
  - Algorithm-specific plots (dendrogram, biplot, elbow)
  - High-resolution PNG images (1200x800 or 1400x1000)

## 🎯 Usage Instructions

### Step 1: Load Data

- Click "Choose CSV/TSV File" to upload your dataset
- OR click "Load Example Data" to use the College dataset
- Configure separator (comma, semicolon, or tab)
- Verify data loaded correctly in "Data" tab

### Step 2: Select Variables

- Choose which numeric variables to include in clustering
- At least 2 variables must be selected

### Step 3: Configure Clustering

- **Algorithm**: Choose K-Means, HAC, or ACM based on your data type
- **Distance Method** (numeric algorithms):
  - **Correlation**: Groups variables with similar patterns (recommended)
  - **Euclidean**: Groups variables with similar values
- **Linkage Method** (HAC only): Ward, Complete, Average, or Single
- **Number of Clusters (k)**: Manually set 2-10 clusters
- **Auto-detect K** (K-Means only): Elbow method for optimal cluster count

### Step 4: Run Clustering

- Click "Run Clustering" button
- Wait for processing (progress bar will show)
- Results appear automatically in "Results" tab

### Step 5: Explore Results

- **Results Tab**: View cluster assignments and detailed summary
- **Visualizations Tab**: Explore multiple plots (sizes, heatmap, representativeness, etc.)
- **Metrics Tab**: Evaluate clustering quality with homogeneity, separation, silhouette
- **Predict Tab**: Classify new variables into existing clusters (optional)

### Step 6: Export

- **Download Results**: CSV file with variable-cluster assignments
- **Download Plots**: ZIP file with all visualizations (PNG format, high resolution)

## 📊 Understanding Results

### Cluster Assignments

Variables in the same cluster have similar behavior:

- **K-Means/HAC with Correlation**: Similar correlation patterns
- **K-Means/HAC with Euclidean**: Similar value distributions
- **ACM**: Strong associations (χ²) with cluster synthetic axis

### Visualizations

**Heatmap** (all algorithms):

- Variables ordered by cluster with separator lines
- K-Means/HAC: Correlation matrix (red = positive, blue = negative)
- ACM: Cramér's V association matrix

**Representativeness** (all algorithms):

- Bar plot showing how well each variable represents its cluster
- Higher values = more representative of cluster center/axis

**Dendrogram** (HAC only):

- Hierarchical tree structure of variable relationships
- Cut line shows where clusters are formed

**Biplot** (ACM only):

- MCA visualization of variable categories and clusters
- Proximity indicates association strength

**Elbow Plot** (K-Means auto-k):

- Within-cluster variance by K value
- "Elbow" point suggests optimal cluster count

### Clustering Metrics

**Homogeneity**: Mean similarity within clusters (higher = more cohesive)

**Separation**: Mean distance between clusters (higher = better separated)

**Silhouette**: Quality of assignments (-1 to 1, >0.5 is good)

**Cophenetic** (HAC): How well dendrogram preserves original distances

**Q Criterion** (ACM): Sum of χ² associations (higher = stronger partition)

## 🔧 Requirements

### R Packages (automatically installed with clustVarACC)

- `shiny`, `shinyjs`, `DT` - Web interface
- `R6` - Object-oriented framework
- `FactoMineR` - MCA for ACM algorithm
- `ggplot2`, `reshape2`, `ggdendro` - Visualizations
- `pheatmap` - Heatmaps
- `data.table` - Fast data loading

### Data Requirements

- **Format**: CSV, TSV, or Parquet
- **Variables**: At least 2 variables of appropriate type
  - K-Means/HAC: numeric variables
  - ACM: categorical variables (factors)
- **Missing values**: Not supported (remove or impute beforehand)
- **Prediction**: New data must have same number of observations as training data

## 📚 More Information

### Package Documentation

```r
# View comprehensive vignette
vignette("intro_package", package = "clustVarACC")

# Algorithm help pages
?ClustVarKMeans
?ClustVarHAC
?ClustVarACM
```

### Source Code

- K-Means: `R/algorithms/KMeansVariablesR6.R`
- HAC: `R/algorithms/HACVariablesR6.R`
- ACM: `R/algorithms/ACMVariablesR6.R`
- Shiny app: `inst/shinyApp/`

## 🎓 Academic Use

This application was developed as part of the M2 SISE 2025-2026 project at Université Lyon 2.

**Authors**: Anne-Camille VIAL, Constantin REY-COQUAIS, Cyrille PECNIK

**Supervisor**: Ricco RAKOTOMALALA
