## Helper functions for algorithm instantiation
create_model <- function(algorithm = "kmeans", params = list()) {
  alg <- if (is.null(algorithm)) "kmeans" else algorithm
  method <- if (!is.null(params$method)) params$method else NULL
  k <- if (!is.null(params$k)) params$k else NULL
  nstart <- if (!is.null(params$nstart)) params$nstart else 10
  seed <- if (!is.null(params$seed)) params$seed else NULL

  if (alg == "kmeans") {
    if (!exists("KMeansVariablesR6", mode = "any")) {
      stop("KMeansVariablesR6 class not found. Make sure the implementation is loaded.")
    }
    return(KMeansVariablesR6$new(
      k = k,
      method = method,
      nstart = nstart,
      seed = seed
    ))
  }

  if (alg == "hac") {
    if (!exists("HACVariablesR6", mode = "any")) {
      stop("HACVariablesR6 class not found. Make sure the implementation is loaded.")
    }
    linkage_method <- if (!is.null(params$linkage_method)) params$linkage_method else "ward.D2"
    return(HACVariablesR6$new(
      k = k,
      method = method,
      linkage_method = linkage_method
    ))
  }

  if (alg == "acm") {
    if (!exists("ACMVariablesR6", mode = "any")) {
      stop("ACMVariablesR6 class not found. Make sure the implementation is loaded.")
    }
    max_iter <- if (!is.null(params$max_iter)) params$max_iter else 30
    tol <- if (!is.null(params$tol)) params$tol else 1e-4
    verbose <- if (!is.null(params$verbose)) params$verbose else FALSE
    return(ACMVariablesR6$new(
      k = k,
      max_iter = max_iter,
      tol = tol,
      verbose = verbose
    ))
  }

  stop("Unknown algorithm selected: ", alg)
}
