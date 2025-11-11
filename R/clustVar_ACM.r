library(FactoMineR)

# --- Fonction η² manuelle adaptée aux variables catégorielles ---
eta2_manual <- function(fac, z) {
  fac <- as.factor(fac)
  z <- as.numeric(z)
  ss_total <- sum((z - mean(z))^2)
  if (ss_total == 0) return(0)
  ss_between <- sum(tapply(z, fac, function(v) length(v) * (mean(v) - mean(z))^2))
  return(ss_between / ss_total)
}
#Clustering de variables via ACM
# Entrée :
#   data : data.frame de variables qualitatives
#   K    : nombre de clusters de variables
# Sortie :
#   Liste contenant la partition des variables et les composantes latentes
clustVar_ACM_Q <- function(data, K, max_iter = 30, tol = 1e-4, verbose = TRUE) {
  
  p <- ncol(data)
  clusters <- sample(1:K, p, replace = TRUE)
  Q_old <- 0
  Q_trace <- c()
  
  for (iter in 1:max_iter) {
    
    # Étape 1 : définition des axes latents (ACM)
    axes_list <- vector("list", K)
    for (k in 1:K) {
      vars_k <- names(data)[clusters == k]
      if (length(vars_k) == 0) {
        axes_list[[k]] <- NULL
      } else if (length(vars_k) == 1) {
        axes_list[[k]] <- scale(as.numeric(as.factor(data[[vars_k[1]]])), center = TRUE, scale = FALSE)
      } else {
        acm_k <- FactoMineR::MCA(data[, vars_k, drop = FALSE], ncp = 1, graph = FALSE)
        axes_list[[k]] <- acm_k$ind$coord[, 1]
      }
    }
    
    # Étape 2 : réallocation des variables aux clusters
    eta2_mat <- matrix(0, nrow = p, ncol = K)
    for (j in 1:p) {
      for (k in 1:K) {
        if (!is.null(axes_list[[k]])) {
          eta2_mat[j, k] <- eta2_manual(data[[j]], axes_list[[k]])
        } else {
          eta2_mat[j, k] <- NA
        }
      }
    }
    new_clusters <- apply(eta2_mat, 1, which.max)
    
    #  Étape 3 : calcul du critère Q, qualité de représentation moyenne des variables
    Q_new <- sum(sapply(1:p, function(j) eta2_mat[j, new_clusters[j]]), na.rm = TRUE)
    Q_trace <- c(Q_trace, Q_new)
    
    if (verbose) cat(sprintf("Itération %2d : Q = %.4f\n", iter, Q_new))
    
    # --- Critère d'arrêt
    if (abs(Q_new - Q_old) < tol) {
      if (verbose) cat("→ Convergence atteinte (variation de Q < tolérance)\n")
      break
    }
    Q_old <- Q_new
    clusters <- new_clusters
  }
  
  return(list(
    partition = clusters,
    Q_final = Q_new,
    Q_trace = Q_trace,
    eta2_matrix = eta2_mat
  ))
}


