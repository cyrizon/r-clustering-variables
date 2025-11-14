# Launch Shiny App - Variable Clustering
# Run this script to start the application

cat("\n")
cat("================================================\n")
cat("  Variable Clustering - Shiny Application      \n")
cat("================================================\n\n")

# Check if shiny is installed
if (!requireNamespace("shiny", quietly = TRUE)) {
    cat("Installing required package: shiny...\n")
    install.packages("shiny")
}

# Check other required packages
packages <- c("shinyjs", "DT", "R6")
for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        cat("Installing required package:", pkg, "...\n")
        install.packages(pkg)
    }
}

# Load shiny
library(shiny)

cat("Starting application...\n\n")
cat("The app will open in your default web browser.\n")
cat("To stop the app, press Ctrl+C or close the R session.\n\n")

# Run the app
runApp("inst/shinyApp", launch.browser = TRUE)
