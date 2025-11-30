## Helper functions for algorithm instantiation
#' Create clustering model instance
#'
#' Factory function to create instances of clustering algorithms
#'
#' @param algorithm Character Algorithm name ("kmeans", "hac", "acm")
#' @param params List of algorithm-specific parameters
#' @return An R6 object instance of the specified clustering algorithm
#' @export
create_model <- function(algorithm = "kmeans", params = list()) {
  alg <- if (is.null(algorithm)) "kmeans" else algorithm
  method <- if (!is.null(params$method)) params$method else NULL
  k <- if (!is.null(params$k)) params$k else NULL
  nstart <- if (!is.null(params$nstart)) params$nstart else 10
  seed <- if (!is.null(params$seed)) params$seed else NULL

  if (alg == "kmeans") {
    if (!exists("ClustVarKMeans", mode = "any")) {
      stop("ClustVarKMeans class not found. Make sure the implementation is loaded.")
    }
    return(ClustVarKMeans$new(
      K = k,
      method = method,
      nstart = nstart,
      seed = seed
    ))
  }

  if (alg == "hac") {
    if (!exists("ClustVarHAC", mode = "any")) {
      stop("ClustVarHAC class not found. Make sure the implementation is loaded.")
    }
    linkage_method <- if (!is.null(params$linkage_method)) params$linkage_method else "ward.D2"
    return(ClustVarHAC$new(
      K = k,
      method = method,
      linkage_method = linkage_method
    ))
  }

  if (alg == "acm") {
    if (!exists("ClustVarACM", mode = "any")) {
      stop("ClustVarACM class not found. Make sure the implementation is loaded.")
    }
    max_iter <- if (!is.null(params$max_iter)) params$max_iter else 30
    tol <- if (!is.null(params$tol)) params$tol else 1e-4
    return(ClustVarACM$new(
      K = k,
      max_iter = max_iter,
      tol = tol
    ))
  }

  stop("Unknown algorithm selected: ", alg)
}
