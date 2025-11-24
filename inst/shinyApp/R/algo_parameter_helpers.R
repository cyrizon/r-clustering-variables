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
    method = input$method
  )

  # Algorithm-specific parameters
  if (algorithm == "kmeans") {
    params$k <- if (!is.null(input$k)) input$k else 3
    params$nstart <- if (!is.null(input$nstart) && !is.na(input$nstart)) as.integer(input$nstart) else 10
    params$seed <- if (!is.null(input$seed) && !is.na(input$seed)) as.integer(input$seed) else NULL
    params$auto_k <- if (!is.null(input$auto_k)) input$auto_k else FALSE

    params$max_k <- if (!is.null(input$max_k)) input$max_k else 10
  } else if (algorithm == "hac") {
    params$k <- if (!is.null(input$hac_k)) input$hac_k else 3
    params$linkage_method <- if (!is.null(input$hac_linkage)) input$hac_linkage else "ward.D2"
  } else if (algorithm == "acm") {
    params$k <- if (!is.null(input$acm_k)) input$acm_k else 3
    params$max_iter <- if (!is.null(input$acm_max_iter)) input$acm_max_iter else 30
    params$tol <- if (!is.null(input$acm_tol)) input$acm_tol else 1e-4
    params$verbose <- if (!is.null(input$acm_verbose)) input$acm_verbose else FALSE
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
    3 # fallback
  )
}
