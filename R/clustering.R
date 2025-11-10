#' Classe R6 pour l'orchestration du clustering de variables
#'
#' Cette classe permet d'orchestrer le clustering de variables via KMeansVariablesR6.
#'
ClusteringOrchestratorR6 <- R6::R6Class(
    "ClusteringOrchestratorR6",
    public = list(
        algo = NULL,
        initialize = function(data, k, method = "correlation") {
            # Charger la classe KMeansVariablesR6
            source_r6 <- file.path("R", "algorithms", "KMeansVariablesR6.R")
            if (file.exists(source_r6)) {
                source(source_r6)
            } else {
                stop("Le fichier KMeansVariablesR6.R est introuvable.")
            }
            self$algo <- KMeansVariablesR6$new(data, k, method)
        },
        fit = function() {
            self$algo$fit()
            invisible(self)
        },
        get_clusters = function() {
            self$algo$get_clusters()
        },
        get_centers = function() {
            self$algo$get_centers()
        }
    )
)
