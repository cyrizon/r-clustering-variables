# Variable Clustering - Shiny Application

## 🚀 How to Run

### Option 1: From RStudio

1. Open `ui.R` or `server.R` in RStudio
2. Click the "Run App" button at the top

### Option 2: From R Console

```r
library(shiny)
runApp("inst/shinyApp")
```

### Option 3: From Package

```r
# After installing the package
library(clustVarACC)
# Run the app (requires implementation in package)
```

## 📋 Features

### ✅ Implemented Features

1. **Data Loading**

   - Upload CSV/TSV files with custom separators
   - Load example dataset (College data)
   - Automatic detection of numeric variables
   - Data preview with interactive table

2. **Variable Selection**

   - Select which variables to include in clustering
   - Multi-select checkbox interface
   - Automatic filtering of non-numeric variables

3. **Clustering Configuration**

   - Choose distance method: Correlation or Euclidean
   - Manual k selection (2-10 clusters)
   - Auto-detect optimal k using:
     - Silhouette method
     - Gap statistic method
   - Visual feedback for optimal k

4. **Results Visualization**

   - Cluster assignments with color coding
   - Cluster size barplot
   - Variable distribution pie chart
   - Correlation heatmap ordered by clusters
   - Optimal k selection plot (when auto-detect is used)
   - Detailed model summary

5. **Prediction**

   - Classify new variables into existing clusters
   - Upload new dataset for prediction
   - View prediction results in interactive table

6. **Export**

   - Download clustering results as CSV
   - Download plots as PNG images

7. **Help & Documentation**
   - Step-by-step usage guide
   - Explanation of distance methods
   - Use cases and interpretation tips

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

- **Distance Method**:
  - **Correlation**: Groups variables with similar patterns (recommended for most cases)
  - **Euclidean**: Groups variables with similar values
- **Number of Clusters (k)**: Manually set 2-10 clusters
- **Auto-detect**: Let the algorithm find optimal k automatically

### Step 4: Run Clustering

- Click "Run Clustering" button
- Wait for processing (progress bar will show)
- Results appear automatically in "Results" tab

### Step 5: Explore Results

- **Results Tab**: View cluster assignments and summary statistics
- **Visualizations Tab**: Explore plots and heatmaps
- **Predict Tab**: Classify new variables (optional)

### Step 6: Export

- Download results as CSV file
- Download plots as PNG images

## 📊 Understanding Results

### Cluster Assignments

Variables in the same cluster have similar behavior across observations:

- **Correlation method**: Similar correlation patterns with other variables
- **Euclidean method**: Similar value distributions

### Correlation Heatmap

- Variables are ordered by cluster
- Black lines separate clusters
- Red colors = positive correlation
- Blue colors = negative correlation
- Blocks along diagonal = within-cluster correlations

### Optimal k Selection

- **Silhouette**: Higher values = better-defined clusters
- **Gap Statistic**: Peak indicates optimal number of clusters

## 🔧 Requirements

### R Packages

- `shiny` - Web application framework
- `shinyjs` - JavaScript operations in Shiny
- `DT` - Interactive tables
- `R6` - Object-oriented programming
- `cluster` - Silhouette and gap statistic (optional, for auto k detection)

### Data Requirements

- CSV or TSV format
- At least 2 numeric variables
- No missing values (NA) in selected variables
- For prediction: same number of rows as training data

## 🐛 Troubleshooting

### "No numeric variables found"

- Make sure your dataset contains numeric columns
- Check that separator is correctly configured

### "X must have the same number of observations"

- For prediction, new dataset must have same number of rows
- Variables can be different, but observations must match

### "Package 'cluster' not available"

- Auto k detection requires the `cluster` package
- Install with: `install.packages("cluster")`
- Or use manual k selection instead

### App won't start

- Make sure all required packages are installed
- Check that R working directory is correct
- Verify file paths in `server.R` line 9

## 💡 Tips

1. **Start with auto-detect k**: Let the algorithm suggest optimal number of clusters
2. **Try both methods**: Correlation usually works better for variable clustering
3. **Check heatmap**: Verify that clusters make sense visually
4. **Export results**: Save results for further analysis in R or Excel
5. **Use prediction**: Classify new variables collected later

## 📚 More Information

For technical details about the K-means algorithm:

- See `../../R/algorithms/KMeansVariablesR6.R`
- Check documentation: `?KMeansVariablesR6`

## 🎓 Academic Use

This application was developed as part of the M2 SISE 2025-2026 project at Université Lyon 2.

**Authors**: Anne-Camille VIAL, Constantin REY-COQUAIS, Cyrille PECNIK

**Supervisor**: Ricco RAKOTOMALALA
