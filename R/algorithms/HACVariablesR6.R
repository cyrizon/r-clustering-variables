#' R6 class for Hierarchical Agglomerative Clustering (HAC) of variables
#'
#' This class wraps the HAC algorithm for variable clustering.
#' It uses the base R function hclust().

HACVariablesR6 <- R6::R6Class(
    "HACVariablesR6",
    public = list(
        # --- PROPERTIES (Fields) ---
        k = NULL, # Desired number of clusters (for cutting the tree)
        method = NULL, # Distance calculation method ("correlation" or "euclidean")
        linkage_method = NULL, # Agglomeration (linkage) method for hclust ("ward.D2", "complete", etc.)
        model = NULL, # Stores the hclust object resulting from fit
        dist_matrix = NULL, # Distance matrix used for hclust
        clusters = NULL, # Named list of variables per cluster
        fitted = FALSE, # Fit status of the model
        data_fit = NULL, # Data (normalized and transposed) used for fitting

        # --- CONSTRUCTOR ---
        initialize = function(k = 2, method = "correlation", linkage_method = "ward.D2") {
            # Constructor: set initial parameters
            self$k <- k
            self$method <- method
            self$linkage_method <- linkage_method
        },

        # --- fit METHOD: Fit the model on X ---
        fit = function(X) {
            # 1. Checks and preprocessing
            if (!is.data.frame(X) && !is.matrix(X)) stop("X must be a data.frame or matrix.")
            if (!all(sapply(X, is.numeric))) stop("All variables must be numeric.")
            if (anyNA(X)) stop("Data must not contain NA.")

            # 2. Preprocessing: normalization (center and scale)
            X_norm <- scale(X)

            # 3. Calculer la Matrice de Distance entre les VARIABLES
            if (self$method == "correlation") {
                # Distance based on absolute correlation (1 - |r|)
                dist_mat <- stats::as.dist(1 - abs(stats::cor(X_norm)))
            } else if (self$method == "euclidean") {
                # Euclidean distance on the transpose (variables as rows)
                dist_mat <- stats::dist(t(X_norm))
            } else {
                stop("Unrecognized distance method. Use 'correlation' or 'euclidean'.")
            }

            # Store normalized transposed data for later use (e.g., predict)
            self$data_fit <- t(X_norm)
            # Store the distance matrix for cophenetic correlation calculation
            self$dist_matrix <- dist_mat

            # 4. Apply HAC (hclust)
            hcl <- stats::hclust(dist_mat, method = self$linkage_method)

            # 5. Store the results
            self$model <- hcl

            # Cut the tree to obtain clusters (cut based on k)
            if (self$k > 1 && self$k < ncol(X)) {
                cluster_assignment <- stats::cutree(hcl, k = self$k)
                self$clusters <- split(colnames(X), cluster_assignment)
            } else {
                warning("The number of clusters 'k' must be between 2 and the number of variables")
                self$clusters <- NULL
            }

            self$fitted <- TRUE
            invisible(self)
        },

        # Predict: attach variables in X (illustrative variables) to the best cluster
        # X must contain the same observations (rows) as the training data, in the same order.
        predict = function(X) {
            if (!self$fitted) stop("Model must be fitted with $fit() before prediction.")
            if (!is.data.frame(X) && !is.matrix(X)) stop("X must be a data.frame or matrix.")
            if (!all(sapply(X, is.numeric))) stop("All variables must be numeric.")

            # CRITICAL CHECK (Should be handled externally/by the user but good to warn)
            if (nrow(X) != ncol(self$data_fit)) {
                stop("X must have the same number of observations (rows) as the data used for fitting.")
            }

            # 1. Normalization (same as in fit)
            # Normalization must be done independently on X because these are new variables.
            X_pred_norm <- scale(X)

            # 2. Extraire les noms des variables originales
            original_vars <- unlist(self$clusters)
            illustrative_vars <- colnames(X)

            # 3. Combine original and illustrative data for correlation computation
            # We need original variables as columns (t(self$data_fit))
            # and new variables as columns (X_pred_norm).
            X_all <- cbind(t(self$data_fit), X_pred_norm)

            # 4. Compute correlation matrix between illustrative variables and ALL original variables
            # Correlate variables X (rows) with original variables (columns)
            cor_mat_all <- stats::cor(X_all)
            cor_mat_pred <- cor_mat_all[illustrative_vars, original_vars, drop = FALSE]

            cluster_pred <- data.frame(
                variable = illustrative_vars,
                cluster = NA,
                avg_correlation = NA
            )

            # 5. Assignment: assign to the cluster with the highest mean absolute correlation
            for (i in seq_along(illustrative_vars)) {
                var_name <- illustrative_vars[i]

                # Compute the mean absolute correlation to variables of each existing cluster
                avg_corrs <- sapply(self$clusters, function(cluster_vars) {
                    # Take the correlation values for the current illustrative variable (var_name)
                    # uniquement pour les variables appartenant au cluster (cluster_vars)
                    mean(abs(cor_mat_pred[var_name, cluster_vars]))
                })

                best_cluster <- which.max(avg_corrs)

                cluster_pred[i, "cluster"] <- best_cluster
                cluster_pred[i, "avg_correlation"] <- max(avg_corrs) # Numeric indicator
            }

            return(cluster_pred)
        },

        # plot: Draw the hierarchical clustering tree (dendrogram)
        plot = function(k = self$k) {
            if (!self$fitted) stop("Model must be fitted with $fit() before plotting.")

            # The horiz=TRUE argument is passed directly to plot.hclust
            # R may produce warnings when other parameters (like xlab, ylab)
            # are used simultaneously because they are handled differently.

            # For horizontal rendering: use the standard plot.hclust parameter
            # Note: simplify labels to reduce the risk of plotting parameter warnings.

            plot(self$model,
                main = paste("HAC of Variables (Linkage:", self$linkage_method, ")"),
                xlab = "Distance",
                ylab = "Variables",
                sub = "",
                horiz = TRUE
            )

            # Fix for namespace issue (use stats::)
            if (k > 1 && k < length(self$model$labels)) {
                stats::rect.hclust(self$model, k = k, border = 2:(k + 1))
            }
            invisible(self)
        },

        # Print: display concise model information
        print = function() {
            cat("HACVariablesR6 Model\n")
            cat("--------------------\n")
            cat(
                "k (Cut):", self$k,
                "| Distance:", self$method,
                "| Linkage:", self$linkage_method,
                "| Fitted:", self$fitted, "\n"
            )
            if (self$fitted && !is.null(self$clusters)) {
                cat("Variables per cluster (k =", self$k, ") :\n")
                # Print the number of variables in each cluster
                print(sapply(self$clusters, length))
            }
            invisible(self)
        },

        # Summary: display detailed model information
        summary = function() {
            cat("HACVariablesR6 Summary\n")
            cat("----------------------\n")
            cat("Model parameters:\n")
            cat("  Number of clusters (k):", self$k, "\n")
            cat("  Distance method:", self$method, "\n")
            cat("  Linkage method:", self$linkage_method, "\n")
            cat("  Status:", ifelse(self$fitted, "Fitted", "Not fitted"), "\n")

            if (self$fitted && !is.null(self$clusters)) {
                cat("\nCluster composition (k =", self$k, "):\n")

                # List variables in each cluster (detailed info)
                for (cluster_name in names(self$clusters)) {
                    vars <- self$clusters[[cluster_name]]
                    cat("  Cluster", cluster_name, "(N =", length(vars), "):", paste(vars, collapse = ", "), "\n")
                }

                cat("\nInterpretation tools guide:\n")
                cat("  - To view the dendrogram (tree structure): call $plot().\n")
                cat("  - To predict new variables: call $predict(X_new).\n")
            }
            invisible(self)
        }
    )
)
