# =============================================================================
# VISUALIZATION HELPERS FOR SHINY APP
# =============================================================================
# Wrapper functions around R6 plot methods to maintain consistent Shiny interface
# =============================================================================

#' Plot cluster sizes as barplot
#' @param clusters List of character vectors (variable names per cluster)
#' @return NULL (side effect: creates plot)
plot_cluster_sizes <- function(clusters) {
  sizes <- sapply(clusters, length)
  names(sizes) <- paste0("Cluster ", seq_along(sizes))

  par(mar = c(5, 5, 3, 2))
  bp <- barplot(sizes,
    col = hcl.colors(length(sizes), "Set 2"),
    main = "Number of Variables per Cluster",
    ylab = "Number of Variables",
    xlab = "Cluster",
    border = NA,
    ylim = c(0, max(sizes) * 1.2)
  )

  # Add value labels
  text(bp, sizes, labels = sizes, pos = 3, cex = 1.2, font = 2)
}

#' Plot k selection metric (silhouette, gap statistic, or elbow)
#' @param k_plot_data data.frame with columns k, value, type
#' @param optimal_k Integer optimal k value (optional)
#' @return NULL (side effect: creates plot)
plot_k_selection <- function(k_plot_data, optimal_k = NULL) {
  par(mar = c(5, 5, 3, 2))

  # Determine if this is an elbow plot (decreasing values)
  is_elbow <- k_plot_data$type[1] == "Within-cluster SS"

  plot(k_plot_data$k, k_plot_data$value,
    type = "b", pch = 19, col = "#667eea",
    lwd = 2, cex = 1.5,
    xlab = "Number of Clusters (k)",
    ylab = k_plot_data$type[1],
    main = paste("Optimal k Selection -", k_plot_data$type[1]),
    xaxt = "n"
  )

  axis(1, at = k_plot_data$k)
  grid(col = "gray80")

  # Highlight optimal k
  if (!is.null(optimal_k)) {
    abline(v = optimal_k, col = "red", lty = 2, lwd = 2)
    yval <- k_plot_data$value[k_plot_data$k == optimal_k]
    if (length(yval) == 1 && !is.na(yval)) {
      points(optimal_k, yval, col = "red", pch = 19, cex = 2)

      # Position label based on plot type
      if (is_elbow) {
        # For elbow plot, put label at top
        text(optimal_k, par("usr")[4] * 0.95,
          paste("k* =", optimal_k),
          col = "red", font = 2, pos = 4
        )
      } else {
        # For silhouette/gap, put label at top
        text(optimal_k, par("usr")[4] * 0.95,
          paste("Optimal k =", optimal_k),
          col = "red", font = 2
        )
      }
    }
  }
}

# =============================================================================
# WRAPPERS FOR R6 PLOT METHODS
# =============================================================================
# These functions wrap the R6 plot() methods to provide a consistent interface
# for the Shiny application
# =============================================================================

#' Plot variable representativeness (wrapper for R6 method)
#' @param model Fitted clustering model (R6 object with plot method)
#' @return NULL (side effect: creates plot)
plot_representativeness <- function(model) {
  if (is.null(model)) {
    plot.new()
    text(0.5, 0.5, "No model available.", cex = 1.3)
    return(invisible(NULL))
  }
  model$plot(type = "representativeness")
}

#' Plot correlation or association heatmap (wrapper for R6 method)
#' @param model Fitted clustering model (R6 object with plot method)
#' @return NULL (side effect: creates plot)
plot_heatmap <- function(model) {
  if (is.null(model)) {
    plot.new()
    text(0.5, 0.5, "No model available.", cex = 1.3)
    return(invisible(NULL))
  }
  model$plot(type = "heatmap")
}

#' Plot HAC dendrogram (wrapper for R6 method)
#' @param model Fitted HAC model (ClustVarHAC R6 object)
#' @return NULL (side effect: creates plot)
plot_dendrogram <- function(model) {
  if (is.null(model)) {
    plot.new()
    text(0.5, 0.5, "No HAC model available.", cex = 1.3)
    return(invisible(NULL))
  }
  model$plot(type = "dendrogram")
}

#' Plot HAC fusion heights (wrapper for R6 method)
#' @param model Fitted HAC model (ClustVarHAC R6 object)
#' @return NULL (side effect: creates plot)
plot_heights <- function(model) {
  if (is.null(model)) {
    plot.new()
    text(0.5, 0.5, "No HAC model available.", cex = 1.3)
    return(invisible(NULL))
  }
  model$plot(type = "heights")
}

#' Plot ACM biplot (wrapper for R6 method)
#' @param model Fitted ACM model (ClustVarACM R6 object)
#' @param axes Vector of two integers indicating which axes to plot (default: c(1, 2))
#' @return NULL (side effect: creates plot)
plot_acm_biplot <- function(model, axes = c(1, 2)) {
  if (is.null(model)) {
    plot.new()
    text(0.5, 0.5, "No ACM model available.", cex = 1.3)
    return(invisible(NULL))
  }
  model$plot(type = "biplot", axes = axes)
}
