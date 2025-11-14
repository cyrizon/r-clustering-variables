## Helper functions for algorithm instantiation
create_model <- function(algorithm = "kmeans", params = list()) {
  alg <- if (is.null(algorithm)) "kmeans" else algorithm
  method <- if (!is.null(params$method)) params$method else NULL
  dist_strategy <- if (!is.null(params$dist_strategy)) params$dist_strategy else NULL
  k <- if (!is.null(params$k)) params$k else NULL
  nstart <- if (!is.null(params$nstart)) params$nstart else 25
  seed <- if (!is.null(params$seed)) params$seed else NULL

  if (alg == "kmeans") {
    if (!exists("KMeansVariablesR6", mode = "any")) {
      stop("KMeansVariablesR6 class not found. Make sure the implementation is loaded.")
    }
    return(KMeansVariablesR6$new(
      k = k,
      method = method,
      dist_strategy = ifelse(is.null(dist_strategy), "pam", dist_strategy),
      nstart = nstart,
      seed = seed
    ))
  }

  if (alg == "hac") {
    if (!exists("HACVariablesR6", mode = "any")) {
      stop("HACVariablesR6 class not found. Ici mettre les options pour HAC.")
    }
    return(HACVariablesR6$new(
      k = k,
      method = method,
      dist_strategy = ifelse(is.null(dist_strategy), "pam", dist_strategy)
    ))
  }

  if (alg == "acm") {
    if (!exists("ACMVariablesR6", mode = "any")) {
      stop("ACMVariablesR6 class not found. Ici mettre les options pour ACM.")
    }
    return(ACMVariablesR6$new(
      k = k,
      method = method,
      dist_strategy = ifelse(is.null(dist_strategy), "pam", dist_strategy)
    ))
  }

  stop("Unknown algorithm selected: ", alg)
}
