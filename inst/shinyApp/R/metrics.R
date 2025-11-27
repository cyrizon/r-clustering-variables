# =============================================================================
# Metrics Calculation for Variable Clustering
# =============================================================================

#' Compute simple metrics for clustering of variables
#' @param X data.frame (selected variables)
#' @param clusters list of variable names per cluster
#' @param method character ("correlation" or "euclidean")
#' @return list of metrics
compute_simple_metrics <- function(X, clusters, method = "correlation") {
    # Check if all variables are numeric
    is_numeric <- sapply(X, is.numeric)
    if (!all(is_numeric)) {
        # If ACM or non-numeric variables, return NA and a message
        return(list(
            homogeneity = NA,
            separation = NA,
            silhouette_mean = NA,
            message = "Metrics only available for numeric variables/clustering."
        ))
    }

    # Standardize the data if using the euclidean method
    if (method == "euclidean") {
        X <- as.data.frame(scale(X))
    }

    # Homogeneity: mean absolute correlation (or 1 - mean distance) within clusters
    homogeneity <- sapply(clusters, function(vars) {
        if (length(vars) < 2) {
            return(NA)
        }
        subX <- X[, vars, drop = FALSE]
        if (method == "correlation") {
            cor_mat <- abs(cor(subX))
            mean(cor_mat[lower.tri(cor_mat)])
        } else {
            dist_mat <- as.matrix(dist(t(subX)))
            mean(dist_mat[lower.tri(dist_mat)])
        }
    })
    # Separation: mean absolute correlation (or mean distance) between clusters
    all_vars <- unlist(clusters)
    sep_vals <- c()
    for (i in seq_along(clusters)) {
        for (j in seq_along(clusters)) {
            if (i < j && length(clusters[[i]]) > 0 && length(clusters[[j]]) > 0) {
                subX1 <- X[, clusters[[i]], drop = FALSE]
                subX2 <- X[, clusters[[j]], drop = FALSE]
                if (method == "correlation") {
                    cor_mat <- abs(cor(cbind(subX1, subX2)))
                    idx1 <- 1:ncol(subX1)
                    idx2 <- (ncol(subX1) + 1):(ncol(subX1) + ncol(subX2))
                    sep_vals <- c(sep_vals, as.vector(cor_mat[idx1, idx2]))
                } else {
                    dist_mat <- as.matrix(dist(t(cbind(subX1, subX2))))
                    idx1 <- 1:ncol(subX1)
                    idx2 <- (ncol(subX1) + 1):(ncol(subX1) + ncol(subX2))
                    sep_vals <- c(sep_vals, as.vector(dist_mat[idx1, idx2]))
                }
            }
        }
    }
    separation <- if (length(sep_vals) > 0) mean(sep_vals, na.rm = TRUE) else NA
    # Silhouette: for each variable, silhouette value
    silhouette_vals <- rep(NA, length(all_vars))
    names(silhouette_vals) <- all_vars

    # Compute distance matrix based on method
    if (method == "correlation") {
        dist_mat <- 1 - abs(cor(X[, all_vars, drop = FALSE]))
    } else {
        dist_mat <- as.matrix(dist(t(X[, all_vars, drop = FALSE])))
    }

    # Compute silhouette for each variable
    for (v in all_vars) {
        clust <- which(sapply(clusters, function(vars) v %in% vars))
        in_clust <- clusters[[clust]]
        out_clust <- setdiff(all_vars, in_clust)

        # a = mean distance to variables in same cluster
        a <- if (length(in_clust) > 1) mean(dist_mat[v, in_clust[in_clust != v]]) else NA

        # b = min mean distance to variables in other clusters
        b <- if (length(out_clust) > 0) {
            min(sapply(seq_along(clusters), function(j) {
                if (j != clust && length(clusters[[j]]) > 0) {
                    mean(dist_mat[v, clusters[[j]]])
                } else {
                    Inf
                }
            }))
        } else {
            NA
        }

        silhouette_vals[v] <- if (!is.na(a) && !is.na(b) && is.finite(b)) {
            (b - a) / max(a, b)
        } else {
            NA
        }
    }
    silhouette_mean <- mean(silhouette_vals, na.rm = TRUE)
    list(homogeneity = homogeneity, separation = separation, silhouette_mean = silhouette_mean)
}

#' Compute cophenetic correlation for HAC
#' @param hac_model fitted ClustVarHAC object (not just $model)
#' @return cophenetic correlation
compute_cophenetic <- function(hac_model) {
    if (is.null(hac_model) || is.null(hac_model$model) || is.null(hac_model$dist_matrix)) {
        return(NA)
    }
    # Use the stored original distance matrix
    cophen <- cophenetic(hac_model$model)
    cor(hac_model$dist_matrix, cophen)
}

#' Extract Q for ACM
#' @param acm_model fitted ClustVarACM
#' @return Q value
extract_acm_Q <- function(acm_model) {
    if (is.null(acm_model$Q_final)) {
        return(NA)
    }
    acm_model$Q_final
}
