# =============================================================================
# Visualization Functions
# Handles plotting of cluster sizes, distributions, correlation heatmaps, and k selection metrics
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

#' Plot cluster distribution as pie chart
#' @param clusters List of character vectors (variable names per cluster)
#' @return NULL (side effect: creates plot)
plot_cluster_distribution <- function(clusters) {
  sizes <- sapply(clusters, length)
  labels <- paste0("Cluster ", seq_along(sizes), "\n(", sizes, ")")
  
  par(mar = c(2, 2, 3, 2))
  pie(sizes,
    labels = labels,
    col = hcl.colors(length(sizes), "Set 2"),
    main = "Variable Distribution Across Clusters",
    border = "white",
    cex = 1.1
  )
}

#' Plot correlation heatmap ordered by clusters
#' @param data data.frame Full dataset
#' @param clusters List of character vectors (variable names per cluster)
#' @param selected_vars Character vector of selected variable names
#' @return NULL (side effect: creates plot)
plot_correlation_heatmap <- function(data, clusters, selected_vars) {
  X <- data[, selected_vars, drop = FALSE]
  cor_mat <- cor(X)
  
  # Order by clusters
  cluster_order <- unlist(clusters)
  cor_mat_ordered <- cor_mat[cluster_order, cluster_order]
  
  # Plot heatmap
  par(mar = c(10, 10, 4, 2))
  image(seq_len(ncol(cor_mat_ordered)),
    seq_len(nrow(cor_mat_ordered)),
    t(cor_mat_ordered[rev(seq_len(nrow(cor_mat_ordered))), ]),
    col = hcl.colors(50, "RdBu", rev = TRUE),
    xlab = "", ylab = "",
    main = "Correlation Matrix (ordered by clusters)",
    axes = FALSE
  )
  
  axis(1,
    at = seq_len(ncol(cor_mat_ordered)),
    labels = colnames(cor_mat_ordered),
    las = 2, cex.axis = 0.8
  )
  axis(2,
    at = seq_len(nrow(cor_mat_ordered)),
    labels = rev(rownames(cor_mat_ordered)),
    las = 2, cex.axis = 0.8
  )
  
  # Add cluster boundaries
  cum_sizes <- cumsum(sapply(clusters, length))
  for (i in seq_along(cum_sizes)[-length(cum_sizes)]) {
    abline(v = cum_sizes[i] + 0.5, col = "black", lwd = 2)
    abline(
      h = nrow(cor_mat_ordered) - cum_sizes[i] + 0.5,
      col = "black", lwd = 2
    )
  }
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
             col = "red", font = 2, pos = 4)
      } else {
        # For silhouette/gap, put label at top
        text(optimal_k, par("usr")[4] * 0.95, 
             paste("Optimal k =", optimal_k), 
             col = "red", font = 2)
      }
    }
  }
}
