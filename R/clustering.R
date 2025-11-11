#' Classe R6 pour l'orchestration du clustering de variables
#'
#' Cette classe permet d'orchestrer le clustering de variables via les différents algorithmes disponibles.
#'
ClusteringOrchestratorR6 <- R6::R6Class(
    "ClusteringOrchestratorR6",
    public = list(
        algo = NULL,
        algorithm_type = NULL,
        initialize = function(data = NULL, k, method = "correlation", algorithm = "acm",
                              linkage_method = "ward.D2", max_iter = 30, tol = 1e-4, verbose = TRUE) {
            # Charger la classe appropriée selon le type d'algorithme
            if (algorithm == "acm") {
                source_r6 <- file.path("R", "algorithms", "ACMVariablesR6.R")
                if (file.exists(source_r6)) {
                    source(source_r6)
                } else {
                    stop("Le fichier ACMVariablesR6.R est introuvable.")
                }
                # Charger aussi la fonction utilitaire
                source_utils <- file.path("R", "utils.R")
                if (file.exists(source_utils)) {
                    source(source_utils)
                }
                self$algo <- ClustVarACM$new(data = data, K = k, max_iter = max_iter, tol = tol, verbose = verbose)
                self$algorithm_type <- "acm"
            } else if (algorithm == "kmeans") {
                source_r6 <- file.path("R", "algorithms", "KMeansVariablesR6.R")
                if (file.exists(source_r6)) {
                    source(source_r6)
                } else {
                    stop("Le fichier KMeansVariablesR6.R est introuvable.")
                }
                self$algo <- KMeansVariablesR6$new(k = k, method = method)
                self$algorithm_type <- "kmeans"
            } else if (algorithm == "hac") {
                source_r6 <- file.path("R", "algorithms", "HACVariablesR6.R")
                if (file.exists(source_r6)) {
                    source(source_r6)
                } else {
                    stop("Le fichier HACVariablesR6.R est introuvable.")
                }
                self$algo <- HACVariablesR6$new(k = k, distance_method = method, linkage_method = linkage_method)
                self$algorithm_type <- "hac"
            } else {
                stop("Algorithme non reconnu. Utilisez 'acm', 'kmeans' ou 'hac'.")
            }
        },
        fit = function(X = NULL) {
            if (self$algorithm_type == "acm") {
                self$algo$fit()
            } else {
                self$algo$fit(X)
            }
            invisible(self)
        },
        get_clusters = function() {
            self$algo$clusters
        },
        predict = function(X) {
            self$algo$predict(X)
        }
    )
)
