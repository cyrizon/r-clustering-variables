# =============================================================================
# Clustering Workflow Logic
# Handles complete clustering process including auto k-detection
# =============================================================================

#' Run complete clustering workflow
#' @param X data.frame Selected variables for clustering
#' @param algorithm Character Algorithm name ("kmeans", "hac", "acm")
#' @param params List of parameters (k, method, seed, auto_k, max_k, nstart)
#' @return List with model, results, optimal_k, k_plot_data
run_clustering_workflow <- function(X, algorithm = "kmeans", params = list()) {
  # Extract parameters with defaults
  auto_k <- if (!is.null(params$auto_k)) params$auto_k else FALSE
  method <- if (!is.null(params$method)) params$method else "correlation"
  nstart <- if (!is.null(params$nstart) && !is.na(params$nstart)) as.integer(params$nstart) else 10
  seed <- if (!is.null(params$seed) && !is.na(params$seed)) as.integer(params$seed) else NULL
  k_manual <- if (!is.null(params$k)) params$k else 3

  optimal_k <- NULL
  k_plot_data <- NULL
  k_to_use <- k_manual

  # Auto-detect optimal k if requested
  if (auto_k && algorithm == "kmeans") {
    max_k <- if (!is.null(params$max_k)) params$max_k else 10
    # Ensure max_k doesn't exceed number of variables
    max_k <- min(max_k, ncol(X))

    # Create temporary model for k selection
    temp_model <- create_model(
      algorithm = algorithm,
      params = list(
        method = method,
        nstart = nstart,
        seed = seed
      )
    )

    # Elbow method with automatic curvature detection
    optimal_k <- temp_model$elbow_method(
      X,
      K_max = max_k,
      plot = FALSE
    )

    # Generate inertia values for plot (start at k=2, minimum valid)
    inertias <- numeric(max_k)
    inertias[1] <- NA # k=1 is not valid for clustering
    for (k in 2:max_k) {
      temp_k_model <- create_model(
        algorithm = algorithm,
        params = list(k = k, method = method, nstart = nstart, seed = seed)
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
  }

  # Create and fit final model
  model <- create_model(
    algorithm = algorithm,
    params = c(
      list(k = k_to_use, method = method, nstart = nstart, seed = seed),
      params[!names(params) %in% c("k", "method", "nstart", "seed", "auto_k", "max_k")]
    )
  )

  # Fit the model with data (uniform interface for all algorithms)
  # For ACM (categorical algorithm), ensure variables are factors; coerce if needed
  if (algorithm == "acm") {
    X[] <- lapply(X, function(col) if (!is.factor(col)) as.factor(col) else col)
  }

  model$fit(X)

  # Extract and format clusters
  clusters_formatted <- model$clusters

  # Convert clusters to named list format if needed
  if (algorithm == "acm") {
    # ACM returns a vector of cluster assignments
    clusters_formatted <- split(names(X), model$clusters)
    names(clusters_formatted) <- paste0("Cluster_", seq_along(clusters_formatted))
  } else if (algorithm == "hac") {
    # HAC returns a list, ensure proper naming
    if (!is.null(names(clusters_formatted))) {
      names(clusters_formatted) <- paste0("Cluster_", names(clusters_formatted))
    } else {
      names(clusters_formatted) <- paste0("Cluster_", seq_along(clusters_formatted))
    }
  }

  # Extract results
  results <- list(
    clusters = clusters_formatted,
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
