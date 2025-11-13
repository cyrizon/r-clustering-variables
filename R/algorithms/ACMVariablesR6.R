library(R6)
library(FactoMineR)

# --- Fonction utilitaire : calcul du η² (rapport de corrélation)
eta2_manual <- function(fac, z) {
  fac <- as.factor(fac)
  z <- as.numeric(z)
  ss_total <- sum((z - mean(z))^2)
  if (ss_total == 0) return(0)
  ss_between <- sum(tapply(z, fac, function(v) length(v) * (mean(v) - mean(z))^2))
  return(ss_between / ss_total)
}

# Cette classe permet d'encapsuler l'algorithme de  clustering de variables catégorielles via ACM
ClustVarACM <- R6::R6Class(
  classname = "ClustVarACM",
  
  public = list(
    # === Attributs ===
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

    
    # Méthode fit
    fit = function() {
      p <- ncol(self$data)
      Q_old <- 0
      self$Q_trace <- c()
	 # Etape 0 : Contrôle
          if (!is.data.frame(data)) stop("Votre objet doit être un data.frame")
          if (!all(sapply(data, is.categorical))) stop("Toutes les variables doivent être catégorielles.")
          if (anyNA(X)) stop("Les données ne doivent pas contenir de NA.")
          if (self$K < 2 || self$K > ncol(data)) stop("K doit être compris entre 2 et le nombre de variables.")
      
      for (iter in 1:self$max_iter) {
        # Étape 1 : Calcul des axes (ACM)
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
        self$eta2_matrix <- matrix(0, nrow = p, ncol = self$K,
                                   dimnames = list(names(self$data), paste0("Cluster_", 1:self$K)))
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
    
    # Méthode test / résumé
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
   
 #  Sélection automatique du K optimal 
    select_K = function(K_grid = 2:6, threshold = 0.1) {
      results <- data.frame(K = K_grid, Q = NA)
      for (i in seq_along(K_grid)) {
        if (self$verbose) cat("Test de K =", K_grid[i], "\n")
        tmp <- ClustVarACM$new(data = self$data, K = K_grid[i], verbose = FALSE)
        tmp$fit()
        results$Q[i] <- tmp$Q_final
      }
      
      # Calcul des différences successives de Q
      dQ <- diff(results$Q)
      rel_gain <- dQ / max(dQ)
      K_opt <- results$K[which(rel_gain < threshold)[1] + 1]
      if (is.na(K_opt)) K_opt <- results$K[which.max(results$Q)]
      
      # Visualisation
      plot(results$K, results$Q, type = "b", pch = 19, col = "blue",
           xlab = "Nombre de clusters (K)", ylab = "Critère global Q",
           main = "Méthode du coude pour la sélection du K optimal")
      abline(v = K_opt, col = "red", lty = 2)
      text(K_opt, max(results$Q), labels = paste("K* =", K_opt), pos = 4, col = "red")
      
      cat(sprintf("→ K optimal sélectionné automatiquement : %d (seuil %.0f%%)\n", K_opt, threshold*100))
      return(list(results = results, K_opt = K_opt))
    },
    
    # Méthode pour la visualisation de la convergence de Q 
    plot_Q = function() {
      if (is.null(self$Q_trace)) stop("Le modèle n'a pas encore été ajusté (fit() non exécuté).")
      plot(self$Q_trace, type = "b", pch = 19, col = "blue",
           xlab = "Itération", ylab = "Critère Q",
           main = "Évolution du critère Q")
    }
    # Méthode predict pour de nouvelles variables
    predict = function(newdata) {
      if (is.null(self$axes_list))
        stop("Le modèle doit être entraîné avec fit() avant d'utiliser predict().")
	    if (!is.data.frame(newdata)) stop("Votre objet doit être un data.frame")
        if (!all(sapply(newdata, is.categorical))) stop("Toutes les variables doivent être catégorielles.")

      
      p_new <- ncol(newdata)
      eta2_mat_new <- matrix(0, nrow = p_new, ncol = self$K,
                             dimnames = list(names(newdata), paste0("Cluster_", 1:self$K)))
      
      for (j in 1:p_new) {
        for (k in 1:self$K) {
          if (!is.null(self$axes_list[[k]])) {
            eta2_mat_new[j, k] <- eta2_manual(newdata[[j]], self$axes_list[[k]])
          } else {
            eta2_mat_new[j, k] <- NA
          }
        }
      }
      
      clusters_pred <- apply(eta2_mat_new, 1, which.max)
      cat("→ Nouvelles variables affectées à des clusters existants.\n")
      return(data.frame(Variable = names(newdata), Cluster_Predit = clusters_pred))
    },
    
    
  )
)
