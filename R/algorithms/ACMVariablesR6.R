# Classe R6 pour le clustering de variables catégorielles via ACM

# --- Fonction utilitaire : calcul du η² (rapport de corrélation)
eta2_manual <- function(fac, z) {
  fac <- as.factor(fac)
  z <- as.numeric(z)
  ss_total <- sum((z - mean(z))^2)
  if (ss_total == 0) {
    return(0)
  }
  ss_between <- sum(tapply(z, fac, function(v) length(v) * (mean(v) - mean(z))^2))
  return(ss_between / ss_total)
}

# Cette classe permet d'encapsuler l'algorithme de  clustering de variables catégorielles via ACM
ACMVariablesR6 <- R6::R6Class(
  classname = "ACMVariablesR6",
  public = list(
    # === Attributs ===
    k = NULL,
    max_iter = NULL,
    tol = NULL,
    verbose = NULL,
    clusters = NULL,
    axes_list = NULL,
    Q_trace = NULL,
    Q_final = NULL,
    eta2_matrix = NULL,
    fitted = FALSE,
    data_fit = NULL,

    # Constructeur
    initialize = function(k = 3, max_iter = 30, tol = 1e-4, verbose = FALSE) {
      self$k <- k
      self$max_iter <- max_iter
      self$tol <- tol
      self$verbose <- verbose
      self$fitted <- FALSE
    },


    # Méthode fit
    fit = function(X) {
      # Etape 0 : Contrôle
      if (!is.data.frame(X)) stop("X doit être un data.frame")
      if (!all(sapply(X, function(x) is.factor(x) || is.character(x)))) {
        stop("Toutes les variables doivent être catégorielles (factor ou character).")
      }
      if (anyNA(X)) stop("Les données ne doivent pas contenir de NA.")
      if (self$k < 2 || self$k > ncol(X)) stop("k doit être compris entre 2 et le nombre de variables.")

      # Stocker les données
      self$data_fit <- X

      # Initialiser les clusters aléatoirement
      self$clusters <- sample(1:self$k, ncol(X), replace = TRUE)

      p <- ncol(X)
      Q_old <- 0
      self$Q_trace <- c()

      for (iter in 1:self$max_iter) {
        # Étape 1 : Calcul des axes (ACM)
        self$axes_list <- vector("list", self$k)
        for (clust_idx in 1:self$k) {
          vars_k <- names(self$data_fit)[self$clusters == clust_idx]
          if (length(vars_k) == 0) {
            self$axes_list[[clust_idx]] <- NULL
          } else if (length(vars_k) == 1) {
            self$axes_list[[clust_idx]] <- scale(as.numeric(as.factor(self$data_fit[[vars_k[1]]])), center = TRUE, scale = FALSE)
          } else {
            acm_k <- FactoMineR::MCA(self$data_fit[, vars_k, drop = FALSE], ncp = 1, graph = FALSE)
            self$axes_list[[clust_idx]] <- acm_k$ind$coord[, 1]
          }
        }

        # Étape 2 : Réallocation
        self$eta2_matrix <- matrix(0,
          nrow = p, ncol = self$k,
          dimnames = list(names(self$data_fit), paste0("Cluster_", 1:self$k))
        )
        for (j in 1:p) {
          for (clust_idx in 1:self$k) {
            if (!is.null(self$axes_list[[clust_idx]])) {
              self$eta2_matrix[j, clust_idx] <- eta2_manual(self$data_fit[[j]], self$axes_list[[clust_idx]])
            } else {
              self$eta2_matrix[j, clust_idx] <- NA
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
      self$fitted <- TRUE
      invisible(self)
    },

    # Méthode summary / résumé
    summary = function() {
      cat("---- Résumé du modèle ----\n")
      cat("Nombre de clusters :", self$k, "\n")
      cat("Critère Q final :", round(self$Q_final, 4), "\n")
      cat("Qualité moyenne (Q/p) :", round(self$Q_final / ncol(self$data_fit), 4), "\n\n")
      cat("Partition finale des variables :\n")
      print(data.frame(Variable = names(self$data_fit), Cluster = self$clusters))

      invisible(list(
        clusters = self$clusters,
        Q = self$Q_final,
        Q_normalized = self$Q_final / ncol(self$data_fit)
      ))
    },

    #  Sélection automatique du k optimal
    select_k = function(k_grid = 2:6, threshold = 0.1) {
      if (!self$fitted) stop("Le modèle doit être ajusté avant de sélectionner k optimal.")
      results <- data.frame(k = k_grid, Q = NA)
      for (i in seq_along(k_grid)) {
        if (self$verbose) cat("Test de k =", k_grid[i], "\n")
        tmp <- ACMVariablesR6$new(k = k_grid[i], verbose = FALSE)
        tmp$fit(self$data_fit)
        results$Q[i] <- tmp$Q_final
      }

      # Calcul des différences successives de Q
      dQ <- diff(results$Q)
      rel_gain <- dQ / max(dQ)
      k_opt <- results$k[which(rel_gain < threshold)[1] + 1]
      if (is.na(k_opt)) k_opt <- results$k[which.max(results$Q)]

      # Visualisation
      plot(results$k, results$Q,
        type = "b", pch = 19, col = "blue",
        xlab = "Nombre de clusters (k)", ylab = "Critère global Q",
        main = "Méthode du coude pour la sélection du k optimal"
      )
      abline(v = k_opt, col = "red", lty = 2)
      text(k_opt, max(results$Q), labels = paste("k* =", k_opt), pos = 4, col = "red")

      cat(sprintf("→ k optimal sélectionné automatiquement : %d (seuil %.0f%%)\n", k_opt, threshold * 100))
      return(list(results = results, k_opt = k_opt))
    },

    # Méthode pour la visualisation de la convergence de Q
    plot_Q = function() {
      if (is.null(self$Q_trace)) stop("Le modèle n'a pas encore été ajusté (fit() non exécuté).")
      plot(self$Q_trace,
        type = "b", pch = 19, col = "blue",
        xlab = "Itération", ylab = "Critère Q",
        main = "Évolution du critère Q"
      )
    },

    # Méthode predict pour de nouvelles variables
    predict = function(newdata) {
      if (is.null(self$axes_list)) {
        stop("Le modèle doit être entraîné avec fit() avant d'utiliser predict().")
      }
      if (!is.data.frame(newdata)) stop("Votre objet doit être un data.frame")
      if (!all(sapply(newdata, function(x) is.factor(x) || is.character(x)))) {
        stop("Toutes les variables doivent être catégorielles (factor ou character).")
      }


      p_new <- ncol(newdata)
      eta2_mat_new <- matrix(0,
        nrow = p_new, ncol = self$k,
        dimnames = list(names(newdata), paste0("Cluster_", 1:self$k))
      )

      for (j in 1:p_new) {
        for (clust_idx in 1:self$k) {
          if (!is.null(self$axes_list[[clust_idx]])) {
            eta2_mat_new[j, clust_idx] <- eta2_manual(newdata[[j]], self$axes_list[[clust_idx]])
          } else {
            eta2_mat_new[j, clust_idx] <- NA
          }
        }
      }

      clusters_pred <- apply(eta2_mat_new, 1, which.max)
      cat("→ Nouvelles variables affectées à des clusters existants.\n")
      return(data.frame(Variable = names(newdata), Cluster_Predit = clusters_pred))
    }
  )
)
