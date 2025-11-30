# =============================================================================
# Global Configuration for Shiny App
# =============================================================================

message("========================================")
message("Loading Shiny App Global Configuration")
message("========================================")

# =============================================================================
# Load Required Packages
# =============================================================================

# Check if running from installed package or dev environment
is_installed <- requireNamespace("clustVarACC", quietly = TRUE)

# Core Shiny packages (always needed)
required_packages <- c("shiny", "shinyjs", "DT")

for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
}

# Set options
options(shiny.maxRequestSize = 300 * 1024^2) # Max upload size: 300MB

# =============================================================================
# Load Algorithm Classes
# =============================================================================

has_kmeans <- FALSE
has_hac <- FALSE
has_acm <- FALSE

if (is_installed) {
    # Load from installed package (includes all dependencies)
    message("\nLoading classes from installed package...")
    library(clustVarACC)
    has_kmeans <- exists("ClustVarKMeans")
    has_hac <- exists("ClustVarHAC")
    has_acm <- exists("ClustVarACM")
    message("✓ Classes loaded from package")
} else {
    # Development mode: load dependencies and source files
    message("\nDevelopment mode: Loading dependencies and source files...")

    # Load R6 and other algorithm dependencies
    dev_packages <- c("R6", "FactoMineR", "ggplot2", "reshape2", "ggdendro")
    for (pkg in dev_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            message(paste("Installing missing package:", pkg))
            install.packages(pkg)
        }
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }

    # Load algorithm classes from source
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
    tryCatch(
        {
            source("../../R/alg_helpers.R")
            message("✓ alg_helpers.R loaded successfully")
        },
        error = function(e) {
            message(paste("✗ alg_helpers.R not found:", e$message))
        }
    )
} # Load Shiny app helper modules (always from app directory)
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
