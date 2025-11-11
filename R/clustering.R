#' Classe R6 pour l'orchestration du clustering de variables
#'
#' Cette classe permet d'orchestrer le clustering de variables via KMeansVariablesR6 ou HACVariablesR6.
#'
ClusteringOrchestratorR6 <- R6::R6Class(
    "ClusteringOrchestratorR6",
    public = list(
        algo = NULL,
        algorithm_type = NULL,
        initialize = function(k, method = "correlation", algorithm = "kmeans", linkage_method = "ward.D2") {
            # Charger la classe appropriée selon le type d'algorithme
            if (algorithm == "kmeans") {
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
                stop("Algorithme non reconnu. Utilisez 'kmeans' ou 'hac'.")
            }
        },
        fit = function(X) {
            self$algo$fit(X)
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
