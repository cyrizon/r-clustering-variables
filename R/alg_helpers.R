## Helper functions for algorithm instantiation
create_model <- function(algorithm = "kmeans", params = list()) {
  alg <- if (is.null(algorithm)) "kmeans" else algorithm
  method <- if (!is.null(params$method)) params$method else NULL
  k <- if (!is.null(params$k)) params$k else NULL
  seed <- if (!is.null(params$seed)) params$seed else NULL

  if (alg == "kmeans") {
    if (!exists("KMeansVariablesR6", mode = "any")) {
      stop("KMeansVariablesR6 class not found. Make sure the implementation is loaded.")
    }
    return(KMeansVariablesR6$new(
      k = k,
      method = method,
      seed = seed
    ))
  }

  if (alg == "hac") {
    if (!exists("HACVariablesR6", mode = "any")) {
      stop("HACVariablesR6 class not found. Ici mettre les options pour HAC.")
    }
    return(HACVariablesR6$new(
      k = k,
      method = method
    ))
  }

  if (alg == "acm") {
    if (!exists("ACMVariablesR6", mode = "any")) {
      stop("ACMVariablesR6 class not found. Ici mettre les options pour ACM.")
    }
    return(ACMVariablesR6$new(
      k = k,
      method = method
    ))
  }

  stop("Unknown algorithm selected: ", alg)
}
