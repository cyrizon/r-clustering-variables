# =============================================================================
# Global Configuration for Shiny App
# =============================================================================

# Load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("DT")) install.packages("DT")
if (!require("R6")) install.packages("R6")

library(shiny)
library(shinyjs)
library(DT)
library(R6)

# Set options
options(shiny.maxRequestSize = 30*1024^2)  # Max upload size: 30MB
