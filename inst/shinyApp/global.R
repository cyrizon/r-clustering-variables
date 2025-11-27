# =============================================================================
# Global Configuration for Shiny App
# =============================================================================

message("========================================")
message("Loading Shiny App Global Configuration")
message("========================================")

# Load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("DT")) install.packages("DT")
if (!require("R6")) install.packages("R6")
if (!require("FactoMineR")) install.packages("FactoMineR")

library(shiny)
library(shinyjs)
library(DT)
library(R6)
library(FactoMineR)

# Set options
options(shiny.maxRequestSize = 300 * 1024^2) # Max upload size: 300MB

# =============================================================================
# Load Algorithm Classes
# =============================================================================

# Load algorithm classes with error handling
has_kmeans <- FALSE
has_hac <- FALSE
has_acm <- FALSE

tryCatch(
    {
        source("../../R/ClustVarKMeans.R")
        has_kmeans <<- TRUE
        message("✓ ClustVarKMeans loaded successfully")
    },
    error = function(e) {
        message(paste("✗ Failed to load KMeans class:", e$message))
        has_kmeans <<- FALSE
    }
)

tryCatch(
    {
        source("../../R/ClustVarHAC.R")
        has_hac <<- TRUE
        message("✓ ClustVarHAC loaded successfully")
    },
    error = function(e) {
        message(paste("✗ Failed to load HAC class:", e$message))
        has_hac <<- FALSE
    }
)

tryCatch(
    {
        source("../../R/ClustVarACM.R")
        has_acm <<- TRUE
        message("✓ ClustVarACM loaded successfully")
    },
    error = function(e) {
        message(paste("✗ Failed to load ACM class:", e$message))
        has_acm <<- FALSE
    }
)

# Load helper modules
message("\nLoading helper modules...")
source("../../R/alg_helpers.R")
source("R/data_handlers.R")
source("R/clustering_logic.R")
source("R/visualizations.R")
source("R/algo_parameter_helpers.R")
source("R/predictions.R")
message("✓ Helper modules loaded")

# Summary
message("\n========================================")
message("Global Configuration Summary:")
message(paste("  KMeans available:", has_kmeans))
message(paste("  HAC available:", has_hac))
message(paste("  ACM available:", has_acm))
message("========================================\n")
