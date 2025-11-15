#' Clustering for categorical variables using MCA
#'
#' @description
#' This class encapsulates the algorithm for variable clustering using MCA .
#'
#' @export
ClustVarACM <- R6::R6Class(
  classname = "ClustVarACM",
  
  public = list(
    #' @field data dataframe containing the variables to cluster; each variable must be categorical(type factor)
    data = NULL,
	#' @field k number of clusters, integer
    K = NULL,
	#' @field clusters List of variable names per cluster  
    clusters = NULL,
	#' @field axes_list list of length K, containing the factorial axe associated with each cluster
    axes_list = NULL,
	#' @field score_matrix matrix containing the association strength between each variable and each cluster ; the score corresponds to 1-p_value from KHI² test
	score_matrix = NULL,
	#' @field Q_trace numeric vector, holding the successive values ofr the criterion Q
    Q_trace = NULL,
	#' @field Q_final numeric, measure for the clustering quality
    Q_final = NULL,
	#' @field tol numeric tolerance for stop criterion
    tol = NULL,
	#' @field max_iter integer maximal number of iterations for the reallocation algorithm
    max_iter = NULL,
    
    #' @description
    #' Create a new ClustVarACMR6 object
    #' @param k Number of clusters 
    #' @return A new `KMeansVariablesR6` object
    initialize = function(data, K, max_iter = 30, tol = 1e-4) {
	# Validate and store core parameters
      self$data <- data
      self$K <- K
      self$max_iter <- max_iter
      self$tol <- tol
      self$clusters <- sample(1:K, ncol(data), replace = TRUE)
    },

    
    #' @description
    #' Fit the ACM model on variables
    #' @param data A data.frame with categorical variables to cluster
    #' @return Self (invisibly) for method chaining
    fit = function() {
      p <- ncol(self$data)
      Q_old <- 0
      self$Q_trace <- c()
	 # Input validation
          if (!is.data.frame(data)) stop("Votre objet doit être un data.frame")
          if (!all(sapply(data, is.categorical))) stop("Toutes les variables doivent être catégorielles.")
          if (anyNA(X)) stop("Les données ne doivent pas contenir de NA.")
          if (self$K < 2 || self$K > ncol(data)) stop("K doit être compris entre 2 et le nombre de variables.")
      
      for (iter in 1:self$max_iter) {
    # Axis calculation (MCA)
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
        
        # Reallocation through KHI² test
        self$score_matrix <- matrix(0, nrow = p, ncol = self$K,
                                    dimnames=list(names(self$data), paste0("Cluster_",1:self$K)))
        
        for (j in 1:p) {
          fac <- as.factor(self$data[[j]])
          
          for (k in 1:self$K) {
            if (!is.null(self$axes_list[[k]])) {
              # axis discretization
              zdisc <- cut(self$axes_list[[k]], breaks = 5, include.lowest = TRUE)
              
              # Contingency table
              tab <- table(fac, zdisc)
              
              if (any(rowSums(tab)==0) || any(colSums(tab)==0)) {
                score <- 0
              } else {
                test <- suppressWarnings(chisq.test(tab, correct = FALSE))
                score <- 1 - test$p.value
              }
              
              self$score_matrix[j,k] <- score
              
            } else {
              self$score_matrix[j,k] <- -Inf
            }
          }
        }
        
        new_clusters <- apply(self$score_matrix, 1, which.max)
        
        # Criterion Q calculation
        Q_new <- sum(sapply(1:p, function(j) self$score_matrix[j, new_clusters[j]]), na.rm = TRUE)
        self$Q_trace <- c(self$Q_trace, Q_new)
       
        
        # Stop criterion
        if (abs(Q_new - Q_old) < self$tol) {
          break
        }
        
        self$clusters <- new_clusters
        Q_old <- Q_new
      }
      
      self$Q_final <- Q_old
      invisible(self)
    },
    
    # Summary method
    summary = function() {
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
   
 	#  Automatic selection for optimal K 
    select_K = function(K_grid = 2:6, threshold = 0.1) {
      results <- data.frame(K = K_grid, Q = NA)
      for (i in seq_along(K_grid)) {
        tmp <- ClustVarACM$new(data = self$data, K = K_grid[i])
        tmp$fit()
        results$Q[i] <- tmp$Q_final
      }
      
      # Calcul des différences successives de Q
      dQ <- diff(results$Q)
      rel_gain <- dQ / max(dQ)
      K_opt <- results$K[which(rel_gain < threshold)[1] + 1]
      if (is.na(K_opt)) K_opt <- results$K[which.max(results$Q)]
      
      # Visualization for the selection of K, based on criterion Q
      plot(results$K, results$Q, type = "b", pch = 19, col = "blue",
           xlab = "Nombre de clusters (K)", ylab = "Critère global Q",
           main = "Méthode du coude pour la sélection du K optimal")
      abline(v = K_opt, col = "red", lty = 2)
      text(K_opt, max(results$Q), labels = paste("K* =", K_opt), pos = 4, col = "red")
      
      cat(sprintf("→ K optimal sélectionné automatiquement : %d (seuil %.0f%%)\n", K_opt, threshold*100))
      return(list(results = results, K_opt = K_opt))
    },
    
    # Visualization for Q convergence
    plot_Q = function() {
      if (is.null(self$Q_trace)) stop("Le modèle n'a pas encore été ajusté (fit() non exécuté).")
      plot(self$Q_trace, type = "b", pch = 19, col = "blue",
           xlab = "Itération", ylab = "Critère Q",
           main = "Évolution du critère Q")
    }
    #' @description
    #' Predict cluster membership for new variables
    #' @param newdata A data.frame with categorical variables to classify
    #' @return A data.frame with variable names, assigned clusters, and distances
    predict = function(newdata) {
      p_new <- ncol(newdata)
      score_new <- matrix(0, nrow=p_new, ncol=self$K)
      
      for (j in 1:p_new) {
        fac <- as.factor(newdata[[j]])
        
        for (k in 1:self$K) {
          if (!is.null(self$axes_list[[k]])) {
            
            zdisc <- cut(self$axes_list[[k]], breaks = 5, include.lowest = TRUE)
            tab <- table(fac, zdisc)
            
            if (any(rowSums(tab)==0) || any(colSums(tab)==0)) {
              score <- 0
            } else {
              test <- suppressWarnings(chisq.test(tab, correct = FALSE))
              score <- 1 - test$p.value
            }
            
            score_new[j,k] <- score
            
          } else {
            score_new[j,k] <- -Inf
          }
        }
      }
      
      clusters_pred <- apply(score_new, 1, which.max)
      return(data.frame(Variable = names(newdata), Cluster_Predit = clusters_pred))
    },
    
    
  )
)
