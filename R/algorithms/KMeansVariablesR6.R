# Cette classe permet d'encapsuler l'algorithme de clustering K-means sur variables.

KMeansVariablesR6 <- R6::R6Class(
    "KMeansVariablesR6",
    public = list(
        # Propriétés
        k = NULL,
        method = NULL,
        model = NULL,
        clusters = NULL,
        centers = NULL,
        fitted = FALSE,
        data_fit = NULL,

        # Constructeur
        initialize = function(k = 2, method = "correlation") {
            self$k <- k
            self$method <- method
        },

        # Fit : modélisation sur X
        fit = function(X) {
            if (!is.data.frame(X) && !is.matrix(X)) stop("X doit être un data.frame ou une matrice.")
            if (!all(sapply(X, is.numeric))) stop("Toutes les variables doivent être numériques.")
            if (anyNA(X)) stop("Les données ne doivent pas contenir de NA.")
            if (self$k < 2 || self$k > ncol(X)) stop("k doit être entre 2 et le nombre de variables.")
            # Normalisation automatique (centrer-réduire)
            X_norm <- scale(X)
            self$data_fit <- X_norm
            # Calcul de la matrice de distance
            if (self$method == "correlation") {
                dist_mat <- as.dist(1 - abs(cor(X_norm)))
            } else if (self$method == "euclidean") {
                dist_mat <- dist(t(X_norm))
            } else {
                stop("Méthode non reconnue.")
            }
            km <- kmeans(as.matrix(as.matrix(dist_mat)), centers = self$k)
            self$model <- km
            self$clusters <- split(colnames(X_norm), km$cluster)
            self$centers <- km$centers
            self$fitted <- TRUE
            invisible(self)
        },

        # Predict : rattache chaque variable de X à un cluster
        predict = function(X) {
            if (!self$fitted) stop("Le modèle doit être ajusté avec $fit() avant de prédire.")
            if (!is.data.frame(X) && !is.matrix(X)) stop("X doit être un data.frame ou une matrice.")
            if (!all(sapply(X, is.numeric))) stop("Toutes les variables doivent être numériques.")
            # Normalisation identique à fit
            X_norm <- scale(X)
            var_names <- colnames(X_norm)
            # Calcul de la matrice de distance entre variables
            if (self$method == "correlation") {
                dist_mat <- as.dist(1 - abs(cor(X_norm)))
            } else if (self$method == "euclidean") {
                dist_mat <- dist(t(X_norm))
            } else {
                stop("Méthode non reconnue.")
            }
            mat <- as.matrix(dist_mat)
            clusters_pred <- rep(NA, length(var_names))
            distances_pred <- rep(NA, length(var_names))
            for (i in seq_along(var_names)) {
                # Calcul de la distance à chaque centre
                dists <- apply(self$centers, 1, function(center) {
                    mean(abs(mat[i, ] - center))
                })
                clusters_pred[i] <- which.min(dists)
                distances_pred[i] <- min(dists)
            }
            df <- data.frame(
                variable = var_names,
                cluster = clusters_pred,
                distance = distances_pred
            )
            return(df)
        },

        # Print : informations succinctes
        print = function() {
            cat("KMeansVariablesR6 model\n")
            cat("k:", self$k, "| method:", self$method, "| fitted:", self$fitted, "\n")
            if (self$fitted) {
                cat("Variables par cluster :\n")
                print(self$clusters)
            }
        },

        # Summary : informations détaillées
        summary = function() {
            cat("KMeansVariablesR6 summary\n")
            cat("Nombre de clusters:", self$k, "\n")
            cat("Méthode de distance:", self$method, "\n")
            if (self$fitted) {
                cat("Variables par cluster :\n")
                print(self$clusters)
                cat("Centres des clusters :\n")
                print(self$centers)
            } else {
                cat("Modèle non ajusté.\n")
            }
        },

        # Suggest_k : méthode du coude pour aider à choisir k
        suggest_k = function(X, max_k = 10, plot = TRUE) {
            if (!is.data.frame(X) && !is.matrix(X)) stop("X doit être un data.frame ou une matrice.")
            if (!all(sapply(X, is.numeric))) stop("Toutes les variables doivent être numériques.")
            if (anyNA(X)) stop("Les données ne doivent pas contenir de NA.")
            n_vars <- ncol(X)
            max_k <- min(max_k, n_vars - 1)
            wss <- numeric(max_k)
            if (self$method == "correlation") {
                dist_mat <- as.dist(1 - abs(cor(X)))
            } else if (self$method == "euclidean") {
                dist_mat <- dist(t(X))
            } else {
                stop("Méthode non reconnue.")
            }
            mat <- as.matrix(as.matrix(dist_mat))
            for (k in 1:max_k) {
                km <- kmeans(mat, centers = k)
                wss[k] <- km$tot.withinss
            }
            if (plot) {
                plot(1:max_k, wss,
                    type = "b", pch = 19, frame = FALSE,
                    xlab = "Nombre de clusters k", ylab = "Somme des carrés intra-clusters",
                    main = "Méthode du coude pour choisir k"
                )
            }
            return(wss)
        },

        # Suggest_k_automatic : recherche automatique du k optimal
        suggest_k_automatic = function(X, max_k = 10, method = "silhouette") {
            # --- Vérifications de base ---
            if (!is.data.frame(X) && !is.matrix(X)) {
                stop("X doit être un data.frame ou une matrice.")
            }
            if (!all(sapply(X, is.numeric))) {
                stop("Toutes les variables doivent être numériques.")
            }
            if (anyNA(X)) {
                stop("Les données ne doivent pas contenir de valeurs manquantes (NA).")
            }

            # --- Préparation des données : clustering sur les variables ---
            X_vars <- t(scale(X)) # transposition => variables deviennent lignes
            n_vars <- nrow(X_vars)
            max_k <- min(max_k, floor(n_vars / 2)) # éviter k > nombre de variables/2

            if (method == "silhouette") {
                # --- Méthode de la silhouette ---
                if (!requireNamespace("cluster", quietly = TRUE)) {
                    stop("Le package 'cluster' est requis pour la méthode silhouette.")
                }

                sil_widths <- numeric(max_k - 1)
                for (k in 2:max_k) {
                    km <- kmeans(X_vars, centers = k, nstart = 25)
                    sil <- cluster::silhouette(km$cluster, dist(X_vars))
                    sil_widths[k - 1] <- mean(sil[, 3])
                }

                optimal_k <- which.max(sil_widths) + 1

                if (interactive()) {
                    plot(2:max_k, sil_widths,
                        type = "b", pch = 19, frame = FALSE,
                        xlab = "Nombre de clusters k",
                        ylab = "Largeur moyenne de silhouette",
                        main = "Méthode de silhouette (clustering de variables)"
                    )
                    abline(v = optimal_k, col = "red", lty = 2)
                }

                message("✅ Nombre optimal de clusters (silhouette) :", optimal_k)
                return(optimal_k)
            } else if (method == "gap") {
                # --- Méthode du gap statistic ---
                if (!requireNamespace("cluster", quietly = TRUE)) {
                    stop("Le package 'cluster' est requis pour la méthode gap statistic.")
                }

                set.seed(123) # reproductibilité
                gap_stat <- cluster::clusGap(X_vars,
                    FUN = function(x, k) kmeans(x, centers = k, nstart = 25),
                    K.max = max_k, B = 50
                )

                optimal_k <- cluster::maxSE(gap_stat$Tab[, "gap"],
                    gap_stat$Tab[, "SE.sim"],
                    method = "Tibs2001SEmax"
                )

                if (interactive()) {
                    plot(gap_stat, main = "Méthode du gap statistic (clustering de variables)")
                    abline(v = optimal_k, col = "red", lty = 2)
                }

                message("✅ Nombre optimal de clusters (gap statistic) :", optimal_k)
                return(optimal_k)
            } else {
                stop("Méthode non reconnue. Utilisez 'silhouette' ou 'gap'.")
            }
        }
    )
)
