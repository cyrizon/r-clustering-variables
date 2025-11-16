# =============================================================================
# Algorithm Parameter Preparation
# Handles algorithm-specific parameter preparation
# =============================================================================

#' Prepare parameters for the selected clustering algorithm
#' @param algorithm Character Algorithm name ("kmeans", "hac", "acm")
#' @param input Shiny input object containing UI values
#' @return Named list of parameters ready for create_model() and run_clustering_workflow()
prepare_algo_parameters <- function(algorithm, input) {
  # Common parameters across all algorithms
  params <- list(
    method = input$method,
    dist_strategy = if (!is.null(input$dist_strategy)) input$dist_strategy else "pam"
  )
  
  # Algorithm-specific parameters
  if (algorithm == "kmeans") {
    params$k <- if (!is.null(input$k)) input$k else 3
    params$nstart <- if (!is.null(input$nstart) && !is.na(input$nstart)) as.integer(input$nstart) else 25
    params$seed <- if (!is.null(input$seed) && !is.na(input$seed)) as.integer(input$seed) else NULL
    params$auto_k <- if (!is.null(input$auto_k)) input$auto_k else FALSE
    params$k_method <- if (!is.null(input$k_method)) input$k_method else "silhouette"
    params$max_k <- if (!is.null(input$max_k)) input$max_k else 8
    params$elbow_threshold <- if (!is.null(input$elbow_threshold)) input$elbow_threshold else 0.1
    
  } else if (algorithm == "hac") {
    # Ici mettre les options spécifiques pour HAC
    params$k <- if (!is.null(input$hac_k)) input$hac_k else 3
    # params$linkage <- if (!is.null(input$hac_linkage)) input$hac_linkage else "average"
    # params$cut_height <- if (!is.null(input$hac_cut)) input$hac_cut else NULL
    
  } else if (algorithm == "acm") {
    # Ici mettre les options spécifiques pour ACM
    params$k <- if (!is.null(input$acm_k)) input$acm_k else 3
    # params$affinity <- if (!is.null(input$acm_affinity)) input$acm_affinity else "cosine"
    # params$threshold <- if (!is.null(input$acm_threshold)) input$acm_threshold else 0.5
  }
  
  return(params)
}

#' Get default k value for algorithm (fallback when no k specified)
#' @param algorithm Character Algorithm name
#' @return Integer default k value
get_default_k <- function(algorithm) {
  switch(algorithm,
    "kmeans" = 3,
    "hac" = 3,
    "acm" = 3,
    3  # fallback
  )
}
