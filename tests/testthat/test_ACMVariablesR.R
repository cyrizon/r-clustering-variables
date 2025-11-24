# ==============================================================================
# Tests unitaires pour la classe ACMVariablesR6
# ==============================================================================
# Ce fichier contient une suite complète de tests pour valider le comportement
# de l'algorithme de clustering de variables catégorielles basé sur l'ACM
#
# Liste des éléments :
# 1. Préparation d'un jeu de données de test
# 2. Bloc de scripts de test pour le package 'testthat'
#   a. Initialisation et Validation des entrées
#   b. Tests de la méthode fit()
#   c. Tests de la validation de la qualité des clusters
#   d. Tests de la méthode predict()
#   e. Tests de la méthode de sélection du K optimal, select_K
#   f. Tests des méthodes d'affichage (print, summary, plot)
# =============================================================================

library(testthat)
library(FactoMineR)
library(R6)

# Charger la classe à tester
source("../../R/ClustVarACMR6.R")

# ==============================================
# 1. Préparation d'un jeu de données de test
#===============================================

# Création d'un jeu de données synthétique avec des corrélations claires
set.seed(42)
n_obs <- 100

# Variables corrélées dans le Cluster 1
VarA <- factor(sample(c("A1", "A2", "A3"), n_obs, replace = TRUE, prob = c(0.5, 0.3, 0.2)))
VarB <- factor(ifelse(VarA %in% c("A1", "A2"), sample(c("B_low", "B_high"), n_obs, replace = TRUE, prob = c(0.7, 0.3)), "B_other"))

# Variables corrélées dans le Cluster 2
VarC <- factor(sample(c("C_yes", "C_no"), n_obs, replace = TRUE, prob = c(0.8, 0.2)))
VarD <- factor(ifelse(VarC == "C_yes", sample(c("D1", "D2"), n_obs, replace = TRUE, prob = c(0.9, 0.1)), "D2"))

# Variable peu corrélée (potentiellement Cluster 3 ou bruit)
VarE <- factor(sample(letters[1:4], n_obs, replace = TRUE))

data_test <- data.frame(VarA, VarB, VarC, VarD, VarE)

# Création de données d'illustration (Predict)
data_illustrative <- data.frame(
  NewVar1 = factor(sample(c("N1", "N2"), n_obs, replace = TRUE, prob = c(0.4, 0.6))),
  NewVar2 = VarA # Une variable très corrélée au Cluster 1
)

#=======================================================
# 2. Bloc de scripts de test pour le package 'testthat'
# ======================================================

#   a. Initialisation des variables
test_that("ClustVarACM: Initialisation et Validation des entrées", {
  # Test de l'initialisation
  model <- ClustVarACM$new(data = data_test, K = 3)
  expect_s3_class(model, "R6")
  expect_equal(model$K, 3)
  expect_equal(ncol(model$data), 5)
  expect_true(length(model$clusters) == 5)
  
  # Test de la validation des erreurs dans fit()
  data_num <- data.frame(a = 1:10, b = 1:10)
  model_err <- ClustVarACM$new(data = data_num, K = 2)
  # Doit échouer car les variables ne sont pas des facteurs
  expect_error(model_err$fit(), "All variables must be categorical \\(type factor\\).") 
  
  data_na <- data_test
  data_na[1, 1] <- NA
  model_na <- ClustVarACM$new(data = data_na, K = 2)
  # Doit échouer car contient des NA
  expect_error(model_na$fit(), "Data must not contain NA values.")
  
  model_k_err <- ClustVarACM$new(data = data_test, K = 1)
  # Doit échouer car K est hors limite
  expect_error(model_k_err$fit(), "K must be between 2 and the number of variables.")
})

---
#   b. Test de la méthode fit()

test_that("ClustVarACM: Fonctionnement principal (fit) et convergence Q", {
  model <- ClustVarACM$new(data = data_test, K = 3, max_iter = 50)
  
  # Exécuter l'ajustement du modèle
  fitted_model <- model$fit()
  
  # Le modèle doit retourner lui-même (invisibly)
  expect_s3_class(fitted_model, "ClustVarACM")
  
  # Le critère Q_final doit être défini et positif
  expect_true(!is.null(model$Q_final))
  expect_gt(model$Q_final, 0)
  
  # Le Q_trace doit montrer une convergence (Q ne doit pas diminuer)
  # L'algorithme est censé être non-décroissant par construction
  expect_true(all(diff(model$Q_trace) >= -model$tol * 10)) 
  
  # La matrice de scores doit avoir les bonnes dimensions (p x K)
  expect_equal(dim(model$score_matrix), c(5, 3))
  
  # Chaque variable doit être assignée à un cluster
  expect_true(all(model$clusters %in% 1:3))
})

---
#   c. Tests de la validation de la qualité des clusters

test_that("ClustVarACM: Validation de la qualité des clusters (Q-score)", {
  model <- ClustVarACM$new(data = data_test, K = 2, max_iter = 100)
  model$fit()
  
  # Le Q final doit être égal à la somme des scores intra-cluster
  p <- ncol(data_test)
  calculated_Q <- sum(sapply(1:p, function(j) {
    cluster_k <- model$clusters[j]
    return(model$score_matrix[j, cluster_k])
  }))
  
  expect_equal(model$Q_final, calculated_Q)
  
  # Vérification heuristique de la partition attendue: 
  # VarA et VarB devraient être ensemble, VarC et VarD aussi.
  cluster_AB <- model$clusters[names(data_test) == "VarA"]
  cluster_CD <- model$clusters[names(data_test) == "VarC"]
  
  expect_equal(model$clusters[names(data_test) == "VarB"], cluster_AB)
  expect_equal(model$clusters[names(data_test) == "VarD"], cluster_CD)
  # Les deux clusters principaux doivent être différents
  expect_true(cluster_AB != cluster_CD)
})

---
#   d. Tests de la méthode predict()

test_that("ClustVarACM: Prédiction sur de nouvelles variables (predict)", {
  model <- ClustVarACM$new(data = data_test, K = 2)
  model$fit()
  
  # Test de la méthode predict
  pred_result <- model$predict(data_illustrative)
  
  expect_s3_class(pred_result, "data.frame")
  expect_equal(nrow(pred_result), 2)
  expect_named(pred_result, c("Variable", "Cluster_Assigned", "Max_Association_Score"))
  
  # La variable 'NewVar2' est une copie de 'VarA' (Cluster 1 dans l'exemple heuristique)
  # Elle devrait être assignée à un des clusters principaux avec un score élevé.
  cluster_AB <- model$clusters[names(data_test) == "VarA"][1]
  
  # Vérification que la variable NewVar2 est assignée au même cluster que VarA et B
  expect_equal(pred_result$Cluster_Assigned[pred_result$Variable == "NewVar2"], cluster_AB)
  # Vérification que le score est élevé (très forte association si c'est une copie)
  expect_gt(pred_result$Max_Association_Score[pred_result$Variable == "NewVar2"], 0.9)
  
  # Test d'erreur: nombre d'observations incompatible
  data_err_obs <- data_illustrative[1:10, ]
  expect_error(model$predict(data_err_obs), "The number of observations in 'X' must be the same as in the training data.")
})

---
#   e. Tests de la méthode de sélection du K optimal, select_K

test_that("ClustVarACM: Sélection du K optimal (select_K)", {
  # Pour ce test, on utilise une petite grille de K
  model_k <- ClustVarACM$new(data = data_test, K = 2) # K initial n'a pas d'importance
  
  # Capture la sortie (print et plot)
  k_selection <- suppressMessages(suppressWarnings(
    capture.output(
      results <- model_k$select_K(K_grid = 2:4, threshold = 0.05)
    )
  ))
  
  # Le résultat doit être une liste avec les résultats et K_opt
  expect_type(results, "list")
  expect_named(results, c("results", "K_opt"))
  
  # La table de résultats doit contenir K et Q pour chaque K
  expect_equal(nrow(results$results), 3)
  expect_true(all(results$results$K == 2:4))
  expect_true(all(!is.na(results$results$Q)))
  
  # K_opt doit être entre 2 et 4
  expect_true(results$K_opt %in% 2:4)
})

---
#   f. Tests des méthodes d'affichage (print, summary, plot)

test_that("ClustVarACM: Méthodes d'affichage (print, summary, plot)", {
  model <- ClustVarACM$new(data = data_test, K = 2)
  model$fit()
  
  # Test print()
  # Doit s'exécuter sans erreur et produire une sortie (vérifié par capture.output)
  expect_output(print(model), "Clustering Model: ClustVarACM")
  
  # Test summary()
  # Doit s'exécuter sans erreur et produire une sortie détaillée
  expect_output(model$summary(), "Final Criterion Q:")
  
  # Test plot_Q()
  # Doit s'exécuter sans erreur (vérification visuelle)
  expect_silent(model$plot_Q())
  
  # Test plot()
  # Doit s'exécuter sans erreur pour les axes par défaut (1 et 2)
  expect_silent(model$plot(axes = c(1, 2)))
  
  # Test plot() avec K=1 (devrait échouer)
  model_k1 <- ClustVarACM$new(data = data_test[, 1:2], K = 1)
  model_k1$clusters <- 1 # Forcer l'assignation à 1
  # La fonction plot exige K >= 2
  expect_error(model_k1$plot(), "Plotting requires at least 2 clusters.")
})
