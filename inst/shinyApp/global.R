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

# Check if running from installed package or dev environment
is_installed <- requireNamespace("clustVarACC", quietly = TRUE)

has_kmeans <- FALSE
has_hac <- FALSE
has_acm <- FALSE

if (is_installed) {
    # Load from installed package
    message("\nLoading classes from installed package...")
    library(clustVarACC)
    has_kmeans <- exists("ClustVarKMeans")
    has_hac <- exists("ClustVarHAC")
    has_acm <- exists("ClustVarACM")
    message("✓ Classes loaded from package")
} else {
    # Load from source files (development mode)
    message("\nLoading classes from source files (dev mode)...")

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

    # Load helper modules (dev mode only)
    message("\nLoading helper modules...")
    tryCatch(source("../../R/alg_helpers.R"), error = function(e) message("✗ alg_helpers.R not found"))
}

# Load Shiny app helper modules (always from app directory)
message("\nLoading Shiny app modules...")
source("R/data_handlers.R")
source("R/clustering_logic.R")
source("R/visualizations.R")
source("R/algo_parameter_helpers.R")
source("R/predictions.R")
message("✓ Shiny helper modules loaded")

# Summary
message("\n========================================")
message("Global Configuration Summary:")
message(paste("  KMeans available:", has_kmeans))
message(paste("  HAC available:", has_hac))
message(paste("  ACM available:", has_acm))
message("========================================\n")
