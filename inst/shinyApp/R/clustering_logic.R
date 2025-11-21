# =============================================================================
# Clustering Workflow Logic
# Handles complete clustering process including auto k-detection
# =============================================================================

#' Run complete clustering workflow
#' @param X data.frame Selected variables for clustering
#' @param algorithm Character Algorithm name ("kmeans", "hac", "acm")
#' @param params List of parameters (k, method, seed, auto_k, k_method, max_k)
#' @return List with model, results, optimal_k, k_plot_data
run_clustering_workflow <- function(X, algorithm = "kmeans", params = list()) {
  # Extract parameters with defaults
  auto_k <- if (!is.null(params$auto_k)) params$auto_k else FALSE
  method <- if (!is.null(params$method)) params$method else "correlation"
  seed <- if (!is.null(params$seed) && !is.na(params$seed)) as.integer(params$seed) else NULL
  k_manual <- if (!is.null(params$k)) params$k else 3

  optimal_k <- NULL
  k_plot_data <- NULL
  k_to_use <- k_manual

  # Auto-detect optimal k if requested
  if (auto_k && algorithm == "kmeans") {
    k_method <- if (!is.null(params$k_method)) params$k_method else "elbow"
    max_k <- if (!is.null(params$max_k)) params$max_k else 10
    # Ensure max_k doesn't exceed number of variables
    max_k <- min(max_k, ncol(X))

    # Create temporary model for k selection
    temp_model <- create_model(
      algorithm = algorithm,
      params = list(
        method = method,
        seed = seed
      )
    )

    if (k_method == "elbow") {
      # Elbow method with automatic curvature detection
      optimal_k <- temp_model$elbow_method(
        X,
        k_max = max_k,
        plot = FALSE
      )

      # Generate inertia values for plot (start at k=2, minimum valid)
      inertias <- numeric(max_k)
      inertias[1] <- NA # k=1 is not valid for clustering
      for (k in 2:max_k) {
        temp_k_model <- create_model(
          algorithm = algorithm,
          params = list(k = k, method = method, seed = seed)
        )
        temp_k_model$fit(X)
        inertias[k] <- temp_k_model$inertia
      }

      k_plot_data <- data.frame(
        k = 1:max_k,
        value = inertias,
        type = "Inertia (Within-cluster SS)"
      )

      k_to_use <- optimal_k
    } else if (requireNamespace("cluster", quietly = TRUE)) {
      optimal_k <- temp_model$suggest_k_automatic(
        X,
        max_k = max_k,
        method = k_method
      )

      # Generate k selection plot data
      if (k_method == "silhouette") {
        k_plot_data <- compute_silhouette_data(
          X,
          temp_model,
          max_k = max_k,
          method = method
        )
      }

      k_to_use <- optimal_k
    } else {
      warning("Package 'cluster' not available. Using manual k value.")
    }
  }

  # Create and fit final model
  model <- create_model(
    algorithm = algorithm,
    params = list(
      k = k_to_use,
      method = method,
      seed = seed
    )
  )

  model$fit(X)

  # Extract results
  results <- list(
    clusters = model$clusters,
    k = k_to_use,
    algorithm = algorithm,
    method = method,
    n_vars = ncol(X),
    model_summary = capture.output(model$summary())
  )

  return(list(
    model = model,
    results = results,
    optimal_k = optimal_k,
    k_plot_data = k_plot_data
  ))
}

#' Compute silhouette data for k selection plot
#' @param X data.frame
#' @param temp_model Model object with fit() method
#' @param max_k Maximum k to test
#' @param method Distance method
#' @return data.frame with k, value, type columns
compute_silhouette_data <- function(X, temp_model, max_k = 8, method = "correlation") {
  sil_data <- numeric(max_k - 1)

  # Calculate distance matrix once
  X_norm <- scale(X)
  if (method == "correlation") {
    cor_mat <- cor(X_norm)
    dist_mat <- as.dist(1 - abs(cor_mat))
  } else {
    dist_mat <- dist(t(X_norm))
  }

  for (k in 2:max_k) {
    # Create new model for this k
    k_model <- create_model(
      algorithm = "kmeans",
      params = list(k = k, method = method, seed = temp_model$seed)
    )
    k_model$fit(X)

    # Get cluster assignments
    cluster_vec <- rep(NA, ncol(X))
    for (i in seq_along(k_model$clusters)) {
      cluster_vec[match(k_model$clusters[[i]], colnames(X))] <- i
    }

    # Calculate silhouette
    sil <- cluster::silhouette(cluster_vec, dist_mat)
    sil_data[k - 1] <- mean(sil[, 3])
  }

  return(data.frame(
    k = 2:max_k,
    value = sil_data,
    type = "Average Silhouette Width"
  ))
}
