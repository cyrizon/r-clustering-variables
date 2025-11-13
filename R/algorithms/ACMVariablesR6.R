library(R6)
library(FactoMineR)

# Cette classe permet d'encapsuler l'algorithme de  clustering de variables catégorielles via ACM
ClustVarACM <- R6::R6Class(
    classname = "ClustVarACM",
    public = list(
        # Attributs
        data = NULL,
        K = NULL,
        clusters = NULL,
        axes_list = NULL,
        Q_trace = NULL,
        Q_final = NULL,
        eta2_matrix = NULL,
        tol = NULL,
        max_iter = NULL,
        verbose = NULL,

        # Constructeur
        initialize = function(data, K, max_iter = 30, tol = 1e-4, verbose = TRUE) {
            self$data <- data
            self$K <- K
            self$max_iter <- max_iter
            self$tol <- tol
            self$verbose <- verbose
            self$clusters <- sample(1:K, ncol(data), replace = TRUE)
            if (verbose) cat("Objet ClustVarACM initialisé avec", ncol(data), "variables et", K, "clusters.\n")
        },

        # Fit : modélisation 
        fit = function() {
            p <- ncol(self$data)
            Q_old <- 0
            self$Q_trace <- c()

            # Etape 0 : Contrôle
            

            # Étape 1 : Calcul des axes (ACM)
            for (iter in 1:self$max_iter) {
                self$axes_list <- vector("list", self$K)
                for (k in 1:self$K) {
                    vars_k <- names(self$data)[self$clusters == k]
                    if (length(vars_k) == 0) {
                        self$axes_list[[k]] <- NULL
                    } else if (length(vars_k) == 1) {
                        self$axes_list[[k]] <- scale(as.numeric(as.factor(self$data[[vars_k[1]]])), center = TRUE, scale = FALSE)
                    } else {
                        acm_k <- FactoMineR::MCA(self$data[, vars_k, drop = FALSE], ncp = 1, graph = FALSE)
                        self$axes_list[[k]] <- acm_k$ind$coord[, 1]
                    }
                }

                # Étape 2 : Réallocation
                self$eta2_matrix <- matrix(0,
                    nrow = p, ncol = self$K,
                    dimnames = list(names(self$data), paste0("Cluster_", 1:self$K))
                )
                for (j in 1:p) {
                    for (k in 1:self$K) {
                        if (!is.null(self$axes_list[[k]])) {
                            self$eta2_matrix[j, k] <- eta2_manual(self$data[[j]], self$axes_list[[k]])
                        } else {
                            self$eta2_matrix[j, k] <- NA
                        }
                    }
                }
                new_clusters <- apply(self$eta2_matrix, 1, which.max)

                # Étape 3 : Calcul du critère Q
                Q_new <- sum(sapply(1:p, function(j) self$eta2_matrix[j, new_clusters[j]]), na.rm = TRUE)
                self$Q_trace <- c(self$Q_trace, Q_new)
                if (self$verbose) cat(sprintf("Itération %2d : Q = %.4f\n", iter, Q_new))

                # Critère d'arrêt
                if (abs(Q_new - Q_old) < self$tol) {
                    if (self$verbose) cat("→ Convergence atteinte (variation de Q < tolérance)\n")
                    break
                }

                self$clusters <- new_clusters
                Q_old <- Q_new
            }

            self$Q_final <- Q_old
            invisible(self)
        },

        # Méthode de test / évaluation
        test = function() {
            cat("---- Résumé du modèle ----\n")
            cat("Nombre de clusters :", self$K, "\n")
            cat("Critère Q final :", round(self$Q_final, 4), "\n")
            cat("Qualité moyenne (Q/p) :", round(self$Q_final / ncol(self$data), 4), "\n\n")
            cat("Partition finale des variables :\n")
            print(data.frame(Variable = names(self$data), Cluster = self$clusters))

            invisible(list(
                clusters = self$clusters,
                Q = self$Q_final,
                Q_normalized = self$Q_final / ncol(self$data)
            ))
        },

        #  Méthode d'affichage du critère Q
        plot_Q = function() {
            if (is.null(self$Q_trace)) stop("Le modèle n'a pas encore été ajusté (fit() non exécuté).")
            plot(self$Q_trace,
                type = "b", pch = 19, col = "blue",
                xlab = "Itération", ylab = "Critère Q",
                main = "Évolution du critère Q"
            )
        }
    )
)
