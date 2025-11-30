# =============================================================================
# VISUALIZATION HELPERS (LEGACY / HYBRID)
# =============================================================================
# NOTE:
# The heatmap and distribution functions below are DEPRECATED.
# Please use the internal R6 methods instead:
#   model$plot(type = "heatmap")
#   model$plot(type = "representativeness")
#
# Only 'plot_cluster_sizes' and 'plot_k_selection' are still used by Shiny.
# =============================================================================


#' Plot eta² heatmap for ACM clustering
#' @param acm_model ClustVarACM fitted model
#' @return NULL (side effect: creates plot)
plot_eta2_heatmap <- function(acm_model) {
  eta2_mat <- acm_model$eta2_matrix
  if (is.null(eta2_mat)) {
    plot.new()
    text(0.5, 0.5, "No eta² matrix available.", cex = 1.3)
    return(invisible(NULL))
  }
  # Heatmap
  par(mar = c(6, 8, 4, 2))
  image(1:ncol(eta2_mat), 1:nrow(eta2_mat), t(eta2_mat[nrow(eta2_mat):1, ]),
    col = hcl.colors(100, "YlOrRd", rev = FALSE),
    axes = FALSE,
    xlab = "Cluster",
    ylab = "Variable",
    main = expression(paste("Heatmap of ", eta^2, " (variable vs cluster)"))
  )
  axis(1, at = 1:ncol(eta2_mat), labels = colnames(eta2_mat), las = 2, cex.axis = 0.9)
  axis(2, at = 1:nrow(eta2_mat), labels = rev(rownames(eta2_mat)), las = 2, cex.axis = 0.9)
  # Add values
  for (i in 1:ncol(eta2_mat)) {
    for (j in 1:nrow(eta2_mat)) {
      val <- eta2_mat[j, i]
      text(i, nrow(eta2_mat) - j + 1, sprintf("%.2f", val), cex = 0.8)
    }
  }
}

#' Plot chi²-based score heatmap for ACM clustering
#' @param acm_model ClustVarACM fitted model (must contain `score_matrix` or `eta2_matrix`)
#' @param p_threshold Numeric significance threshold (p-value); cells with p <= threshold will be outlined
#' @param show_p Logical, if TRUE show original p-values in cells; otherwise show score (1 - p)
#' @return NULL (side effect: creates plot)
plot_chi2_heatmap <- function(acm_model, p_threshold = 0.05, show_p = FALSE) {
  # prefer score_matrix (1 - p), but try to reconstruct from eta2_matrix if absent
  score_mat <- acm_model$score_matrix
  if (is.null(score_mat)) {
    plot.new()
    text(0.5, 0.5, "No chi² score matrix available.", cex = 1.2)
    return(invisible(NULL))
  }

  # If the matrix has column/row names, use them; otherwise create labels
  var_names <- rownames(score_mat)
  if (is.null(var_names)) var_names <- paste0("V", seq_len(nrow(score_mat)))
  cluster_names <- colnames(score_mat)
  if (is.null(cluster_names)) cluster_names <- paste0("C", seq_len(ncol(score_mat)))

  # Convert score (1 - p) to numeric between 0 and 1
  sm <- as.matrix(score_mat)
  sm[is.infinite(sm) | is.na(sm)] <- 0
  sm[sm < 0] <- 0
  sm[sm > 1] <- 1

  par(mar = c(6, 8, 4, 2))
  image(1:ncol(sm), 1:nrow(sm), t(sm[nrow(sm):1, , drop = FALSE]),
    col = hcl.colors(100, "YlOrRd"), axes = FALSE,
    xlab = "Cluster", ylab = "Variable",
    main = "Association Heatmap: 1 - p-value (chi²)"
  )
  axis(1, at = 1:ncol(sm), labels = cluster_names, las = 2, cex.axis = 0.9)
  axis(2, at = 1:nrow(sm), labels = rev(var_names), las = 2, cex.axis = 0.9)

  # Optionally annotate cells with values (score or p-value)
  for (i in 1:ncol(sm)) {
    for (j in 1:nrow(sm)) {
      val <- sm[j, i]
      lab <- if (show_p) {
        # show original p-value = 1 - score
        pv <- 1 - val
        sprintf("%.3f", pv)
      } else {
        sprintf("%.2f", val)
      }
      text(i, nrow(sm) - j + 1, lab, cex = 0.8)
    }
  }

  # Outline significant cells (p <= p_threshold) if show_p FALSE or TRUE (we compute p=1-score)
  p_mat <- 1 - sm
  sig_idx <- which(p_mat <= p_threshold, arr.ind = TRUE)
  if (nrow(sig_idx) > 0) {
    for (r in seq_len(nrow(sig_idx))) {
      row_i <- sig_idx[r, 1]
      col_i <- sig_idx[r, 2]
      # draw rectangle around cell (x=col_i, y= nrow - row_i + 1)
      rect(col_i - 0.5, nrow(sm) - row_i + 0.5 - 0.999, col_i + 0.5, nrow(sm) - row_i + 1.5 - 0.999,
        border = "black", lwd = 2)
    }
  }

  invisible(NULL)
}
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
  is_numeric <- sapply(X, is.numeric)
  if (!all(is_numeric)) {
    plot.new()
    text(0.5, 0.5, "Heatmap only available for numeric variables.", cex = 1.3)
    return(invisible(NULL))
  }
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
