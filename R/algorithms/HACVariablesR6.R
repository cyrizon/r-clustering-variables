#' Classe R6 pour le Clustering Hiérarchique Agglomératif (HAC) de Variables
#'
#' Cette classe encapsule l'algorithme HAC pour le clustering de variables.
#' Elle utilise la fonction de base R hclust().

HACVariablesR6 <- R6::R6Class(
    "HACVariablesR6",
    public = list(
        # --- PROPRIÉTÉS (Champs) ---
        k = NULL, # Nombre de clusters désiré (pour couper l'arbre)
        method = NULL, # Méthode de calcul de distance ("correlation" ou "euclidean")
        linkage_method = NULL, # Méthode d'agrégation (linkage) pour hclust ("ward.D2", "complete", etc.)
        model = NULL, # Stocke l'objet hclust résultant du fit
        dist_matrix = NULL, # Matrice de distance utilisée pour hclust
        clusters = NULL, # Liste nommée des variables par cluster
        fitted = FALSE, # Statut du fit du modèle
        data_fit = NULL, # Données (normalisées et transposées) utilisées pour le fit

        # --- CONSTRUCTEUR ---
        initialize = function(k = 2, method = "correlation", linkage_method = "ward.D2") {
            # Le constructeur définit les paramètres initiaux
            self$k <- k
            self$method <- method
            self$linkage_method <- linkage_method
        },

        # --- MÉTHODE FIT : Ajustement du modèle sur X ---
        fit = function(X) {
            # 1. Vérifications et Pré-traitement
            if (!is.data.frame(X) && !is.matrix(X)) stop("X doit être un data.frame ou une matrice.")
            if (!all(sapply(X, is.numeric))) stop("Toutes les variables doivent être numériques.")
            if (anyNA(X)) stop("Les données ne doivent pas contenir de NA.")

            # 2. Pré-traitement : Normalisation (centrage et réduction)
            X_norm <- scale(X)

            # 3. Calculer la Matrice de Distance entre les VARIABLES
            if (self$method == "correlation") {
                # Distance basée sur la corrélation absolue (1 - |r|)
                dist_mat <- stats::as.dist(1 - abs(stats::cor(X_norm)))
            } else if (self$method == "euclidean") {
                # Distance euclidienne sur la transposée (variables comme lignes)
                dist_mat <- stats::dist(t(X_norm))
            } else {
                stop("Méthode de distance non reconnue. Utilisez 'correlation' ou 'euclidean'.")
            }

            # Stocke les données normalisées transposées pour un usage ultérieur (p. ex. predict)
            self$data_fit <- t(X_norm)
            # Stocke la matrice de distance pour le calcul de la corrélation cophénétique
            self$dist_matrix <- dist_mat

            # 4. Appliquer HAC (hclust)
            hcl <- stats::hclust(dist_mat, method = self$linkage_method)

            # 5. Stocker les Résultats
            self$model <- hcl

            # Couper l'arbre pour obtenir les clusters (coupe basée sur k)
            if (self$k > 1 && self$k < ncol(X)) {
                cluster_assignment <- stats::cutree(hcl, k = self$k)
                self$clusters <- split(colnames(X), cluster_assignment)
            } else {
                warning("Le nombre de clusters 'k' doit être entre 2 et le nombre de variables")
                self$clusters <- NULL
            }

            self$fitted <- TRUE
            invisible(self)
        },

        # Predict : Rattache les variables dans X (variables illustratives) au meilleur cluster
        # X doit contenir les mêmes observations (lignes) que les données d'entraînement, dans le même ordre.
        predict = function(X) {
            if (!self$fitted) stop("Le modèle doit être ajusté avec $fit() avant la prédiction.")
            if (!is.data.frame(X) && !is.matrix(X)) stop("X doit être un data.frame ou une matrice.")
            if (!all(sapply(X, is.numeric))) stop("Toutes les variables doivent être numériques.")

            # VÉRIFICATION CRITIQUE (Doit être gérée en externe/par l'utilisateur mais bon d'avertir)
            if (nrow(X) != ncol(self$data_fit)) {
                stop("X doit avoir le même nombre d'observations (lignes) que les données utilisées pour l'ajustement.")
            }

            # 1. Normalisation (identique à fit)
            # La normalisation doit être faite indépendamment sur X car ce sont de nouvelles variables.
            X_pred_norm <- scale(X)

            # 2. Extraire les noms des variables originales
            original_vars <- unlist(self$clusters)
            illustrative_vars <- colnames(X)

            # 3. Combiner les données (originales et illustratives) pour le calcul de corrélation
            # On a besoin des variables originales en colonnes (t(self$data_fit))
            # et des nouvelles variables en colonnes (X_pred_norm).
            X_all <- cbind(t(self$data_fit), X_pred_norm)

            # 4. Calculer la matrice de corrélation entre les variables illustratives et TOUTES les variables originales
            # Corréler les variables X (lignes) avec les variables originales (colonnes)
            cor_mat_all <- stats::cor(X_all)
            cor_mat_pred <- cor_mat_all[illustrative_vars, original_vars, drop = FALSE]

            cluster_pred <- data.frame(
                variable = illustrative_vars,
                cluster = NA,
                avg_correlation = NA
            )

            # 5. Affectation : Attribuer au cluster avec la plus haute corrélation absolue moyenne
            for (i in seq_along(illustrative_vars)) {
                var_name <- illustrative_vars[i]

                # Calculer la corrélation absolue moyenne aux variables de chaque cluster existant
                avg_corrs <- sapply(self$clusters, function(cluster_vars) {
                    # Prendre les valeurs de corrélation pour la variable illustrative actuelle (var_name)
                    # uniquement pour les variables appartenant au cluster (cluster_vars)
                    mean(abs(cor_mat_pred[var_name, cluster_vars]))
                })

                best_cluster <- which.max(avg_corrs)

                cluster_pred[i, "cluster"] <- best_cluster
                cluster_pred[i, "avg_correlation"] <- max(avg_corrs) # Indicateur numérique
            }

            return(cluster_pred)
        },

        # plot : Trace l'arbre de clustering hiérarchique (Dendrogramme)
        plot = function(k = self$k) {
            if (!self$fitted) stop("Le modèle doit être ajusté avec $fit() avant le traçage.")

            # L'argument horiz=TRUE est transmis directement à plot.hclust
            # Le système R génère souvent des avertissements si les autres paramètres
            # (comme xlab, ylab) sont utilisés en même temps, car ils sont gérés différemment.

            # Solution pour le rendu horizontal : utiliser le paramètre standard de plot.hclust
            # Note : on simplifie les labels pour réduire le risque d'avertissements de transmission.

            plot(self$model,
                main = paste("HAC des Variables (Lien:", self$linkage_method, ")"),
                xlab = "Distance",
                ylab = "Variables",
                sub = "",
                horiz = TRUE # C'est la façon correcte de le faire
            )

            # Correction de l'erreur de namespace (utilisation de stats::)
            if (k > 1 && k < length(self$model$labels)) {
                stats::rect.hclust(self$model, k = k, border = 2:(k + 1))
            }
            invisible(self)
        },

        # Print : Affiche les informations succinctes sur le modèle
        print = function() {
            cat("Modèle HACVariablesR6\n")
            cat("--------------------\n")
            cat(
                "k (Coupe):", self$k,
                "| Distance:", self$method,
                "| Lien:", self$linkage_method,
                "| Ajusté:", self$fitted, "\n"
            )
            if (self$fitted && !is.null(self$clusters)) {
                cat("Variables par cluster (k =", self$k, ") :\n")
                # Affiche le nombre de variables dans chaque cluster
                print(sapply(self$clusters, length))
            }
            invisible(self)
        },

        # Summary : Affiche les informations détaillées sur le modèle
        summary = function() {
            cat("Résumé HACVariablesR6\n")
            cat("----------------------\n")
            cat("Paramètres du Modèle:\n")
            cat("  Nombre de clusters (k):", self$k, "\n")
            cat("  Méthode de distance:", self$method, "\n")
            cat("  Méthode de lien:", self$linkage_method, "\n")
            cat("  Statut:", ifelse(self$fitted, "Ajusté", "Non Ajusté"), "\n")

            if (self$fitted && !is.null(self$clusters)) {
                cat("\nComposition des Clusters (k =", self$k, "):\n")

                # Liste les variables dans chaque cluster (informations détaillées)
                for (cluster_name in names(self$clusters)) {
                    vars <- self$clusters[[cluster_name]]
                    cat("  Cluster", cluster_name, "(N =", length(vars), "):", paste(vars, collapse = ", "), "\n")
                }

                cat("\nGuide des Outils d'Interprétation:\n")
                cat("  - Pour visualiser le dendrogramme (structure arborescente): appelez $plot().\n")
                cat("  - Pour prédire de nouvelles variables: appelez $predict(X_new).\n")
            }
            invisible(self)
        }
    )
)
