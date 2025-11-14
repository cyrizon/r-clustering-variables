# =============================================================================
# Clustering Workflow Logic
# Handles complete clustering process including auto k-detection
# =============================================================================

#' Run complete clustering workflow
#' @param X data.frame Selected variables for clustering
#' @param algorithm Character Algorithm name ("kmeans", "hac", "acm")
#' @param params List of parameters (k, method, dist_strategy, nstart, seed, auto_k, k_method, max_k)
#' @return List with model, results, optimal_k, k_plot_data
run_clustering_workflow <- function(X, algorithm = "kmeans", params = list()) {
  # Extract parameters with defaults
  auto_k <- if (!is.null(params$auto_k)) params$auto_k else FALSE
  method <- if (!is.null(params$method)) params$method else "correlation"
  dist_strategy <- if (!is.null(params$dist_strategy)) params$dist_strategy else "pam"
  nstart <- if (!is.null(params$nstart) && !is.na(params$nstart)) as.integer(params$nstart) else 25
  seed <- if (!is.null(params$seed) && !is.na(params$seed)) as.integer(params$seed) else NULL
  k_manual <- if (!is.null(params$k)) params$k else 3
  
  optimal_k <- NULL
  k_plot_data <- NULL
  k_to_use <- k_manual
  
  # Auto-detect optimal k if requested
  if (auto_k && algorithm == "kmeans") {
    k_method <- if (!is.null(params$k_method)) params$k_method else "silhouette"
    max_k <- if (!is.null(params$max_k)) params$max_k else 8
    
    # Create temporary model for k selection
    temp_model <- create_model(
      algorithm = algorithm,
      params = list(
        method = method,
        dist_strategy = dist_strategy,
        nstart = nstart,
        seed = seed
      )
    )
    
    if (requireNamespace("cluster", quietly = TRUE)) {
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
          method = method, 
          dist_strategy = dist_strategy
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
      dist_strategy = dist_strategy,
      nstart = nstart,
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
#' @param dist_strategy Distance strategy
#' @return data.frame with k, value, type columns
compute_silhouette_data <- function(X, temp_model, max_k = 8, method = "correlation", dist_strategy = "pam") {
  sil_data <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    temp_model$k <- k
    temp_model$fit(X)
    
    if (method == "correlation") {
      cor_mat <- cor(scale(X))
      dist_mat <- 1 - abs(cor_mat)
      
      # Use silhouette based on the strategy
      if (dist_strategy == "pam") {
        pam_res <- temp_model$model$pam
        if (!is.null(pam_res)) {
          sil <- cluster::silhouette(pam_res$clustering, as.dist(dist_mat))
        } else {
          sil <- cluster::silhouette(temp_model$model$cluster, as.dist(dist_mat))
        }
      } else if (dist_strategy == "mds") {
        kdim <- min(max(2, temp_model$k), max(2, ncol(scale(X)) - 1))
        coords <- stats::cmdscale(as.dist(dist_mat), k = kdim)
        sil <- cluster::silhouette(temp_model$model$cluster, dist(coords))
      } else {
        sil <- cluster::silhouette(temp_model$model$cluster, as.dist(dist_mat))
      }
    } else {
      sil <- cluster::silhouette(temp_model$model$cluster, dist(t(scale(X))))
    }
    
    sil_data[k - 1] <- mean(sil[, 3])
  }
  
  return(data.frame(
    k = 2:max_k,
    value = sil_data,
    type = "Silhouette Width"
  ))
}
