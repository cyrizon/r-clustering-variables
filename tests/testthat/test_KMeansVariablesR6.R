# ==============================================================================
# Tests unitaires pour la classe KMeansVariablesR6
# ==============================================================================
# Ce fichier contient une suite complète de tests pour valider le comportement
# de l'algorithme K-means adapté au clustering de variables.
#
# Organisation des tests :
# 1. Tests d'initialisation
# 2. Tests de validation des données
# 3. Tests de la méthode fit()
# 4. Tests de la méthode predict()
# 5. Tests de la méthode elbow_method()
# 6. Tests des méthodes utilitaires (print, summary, get_center_variables)
# ==============================================================================

library(testthat)
library(R6)

# Charger la classe à tester
source("../../R/algorithms/KMeansVariablesR6.R")

# ==============================================================================
# 1. TESTS D'INITIALISATION
# ==============================================================================

test_that("KMeansVariablesR6 s'initialise correctement avec les paramètres par défaut", {
    # Test : Création d'un objet avec les paramètres par défaut
    model <- KMeansVariablesR6$new()

    # Vérifications des valeurs par défaut
    expect_equal(model$k, 3)
    expect_equal(model$method, "correlation")
    expect_equal(model$max_iter, 100)
    expect_equal(model$nstart, 10)
    expect_false(model$fitted)
    expect_null(model$seed)
})

test_that("KMeansVariablesR6 s'initialise avec des paramètres personnalisés", {
    # Test : Création avec paramètres spécifiques
    model <- KMeansVariablesR6$new(
        k = 5,
        method = "euclidean",
        max_iter = 50,
        nstart = 20,
        seed = 123
    )

    # Vérifications
    expect_equal(model$k, 5)
    expect_equal(model$method, "euclidean")
    expect_equal(model$max_iter, 50)
    expect_equal(model$nstart, 20)
    expect_equal(model$seed, 123)
})

test_that("La méthode de distance doit être valide", {
    # Test : Méthode invalide doit lever une erreur
    expect_error(
        KMeansVariablesR6$new(method = "invalid_method"),
        "'arg' should be one of"
    )
})

# ==============================================================================
# 2. TESTS DE VALIDATION DES DONNÉES
# ==============================================================================

test_that("fit() rejette les données non numériques", {
    # Test : Données avec colonnes non numériques
    model <- KMeansVariablesR6$new(k = 2)
    data_invalid <- data.frame(
        x1 = c(1, 2, 3),
        x2 = c("a", "b", "c"), # Colonne non numérique
        x3 = c(4, 5, 6)
    )

    expect_error(
        model$fit(data_invalid),
        "All variables must be numeric"
    )
})

test_that("fit() rejette les données avec valeurs manquantes", {
    # Test : Données avec NA
    model <- KMeansVariablesR6$new(k = 2)
    data_with_na <- data.frame(
        x1 = c(1, 2, NA),
        x2 = c(4, 5, 6),
        x3 = c(7, 8, 9)
    )

    expect_error(
        model$fit(data_with_na),
        "Data must not contain missing values"
    )
})

test_that("fit() rejette un k invalide", {
    # Test : k trop petit (< 2)
    model <- KMeansVariablesR6$new(k = 1)
    data <- data.frame(x1 = 1:10, x2 = 11:20, x3 = 21:30)

    expect_error(
        model$fit(data),
        "k must be between 2 and the number of variables"
    )

    # Test : k trop grand (> nombre de variables)
    model <- KMeansVariablesR6$new(k = 10)
    expect_error(
        model$fit(data),
        "k must be between 2 and the number of variables"
    )
})

test_that("fit() accepte une matrice en entrée", {
    # Test : Les matrices doivent être acceptées comme les data.frames
    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    data_matrix <- matrix(rnorm(30), nrow = 10, ncol = 3)

    # fit() produit un message de succès
    expect_message(model$fit(data_matrix), "Best of")
    expect_true(model$fitted)
})

# ==============================================================================
# 3. TESTS DE LA MÉTHODE FIT()
# ==============================================================================

test_that("fit() crée des clusters avec la méthode correlation", {
    # Test : Clustering basé sur la corrélation
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50),
        var4 = rnorm(50)
    )

    model <- KMeansVariablesR6$new(k = 2, method = "correlation", seed = 42)
    model$fit(data)

    # Vérifications
    expect_true(model$fitted)
    expect_equal(length(model$clusters), 2) # 2 clusters
    expect_equal(length(model$centers), 2) # 2 centres
    expect_true(is.numeric(model$inertia))
    expect_true(model$inertia >= 0)

    # Vérifier que toutes les variables sont assignées
    all_vars <- unlist(model$clusters)
    expect_equal(length(all_vars), 4)
    expect_setequal(all_vars, colnames(data))
})

test_that("fit() crée des clusters avec la méthode euclidean", {
    # Test : Clustering basé sur la distance euclidienne
    set.seed(123)
    data <- data.frame(
        var1 = rnorm(50, mean = 0),
        var2 = rnorm(50, mean = 0),
        var3 = rnorm(50, mean = 5),
        var4 = rnorm(50, mean = 5)
    )

    model <- KMeansVariablesR6$new(k = 2, method = "euclidean", seed = 123)
    model$fit(data)

    # Vérifications
    expect_true(model$fitted)
    expect_equal(length(model$clusters), 2)
    expect_true(model$inertia >= 0)
})

test_that("fit() génère des noms de variables par défaut si absents", {
    # Test : Données sans noms de colonnes
    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    data_matrix <- matrix(rnorm(40), nrow = 10, ncol = 4)
    # La matrice n'a pas de colnames initialement

    model$fit(data_matrix)

    # Vérifier que des noms par défaut ont été générés (V1, V2, V3, V4)
    all_vars <- unlist(model$clusters)
    expect_true(all(grepl("^V[0-9]+$", all_vars)))
    expect_equal(length(all_vars), 4)
})

test_that("fit() avec plusieurs départs (nstart) améliore la solution", {
    # Test : nstart > 1 doit potentiellement améliorer l'inertie
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50),
        var4 = rnorm(50),
        var5 = rnorm(50)
    )

    # Modèle avec 1 seul départ
    model1 <- KMeansVariablesR6$new(k = 3, nstart = 1, seed = 42)
    model1$fit(data)

    # Modèle avec 20 départs
    model2 <- KMeansVariablesR6$new(k = 3, nstart = 20, seed = 42)
    model2$fit(data)

    # L'inertie avec plus de départs devrait être <= (meilleure ou égale)
    expect_lte(model2$inertia, model1$inertia)
})

test_that("fit() stocke correctement les paramètres de normalisation", {
    # Test : Les paramètres de scaling doivent être stockés
    data <- data.frame(
        var1 = c(1, 2, 3, 4, 5),
        var2 = c(10, 20, 30, 40, 50),
        var3 = c(100, 200, 300, 400, 500)
    )

    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    model$fit(data)

    # Vérifier que scale_center et scale_scale sont stockés
    expect_equal(length(model$scale_center), 3)
    expect_equal(length(model$scale_scale), 3)
    expect_true(all(!is.na(model$scale_center)))
    expect_true(all(!is.na(model$scale_scale)))
})

# ==============================================================================
# 4. TESTS DE LA MÉTHODE PREDICT()
# ==============================================================================

test_that("predict() nécessite un modèle ajusté", {
    # Test : predict() sans fit() préalable doit échouer
    model <- KMeansVariablesR6$new(k = 2)
    new_data <- data.frame(new_var = rnorm(50))

    expect_error(
        model$predict(new_data),
        "Model must be fitted"
    )
})

test_that("predict() rejette les données avec nombre d'observations différent", {
    # Test : Les nouvelles données doivent avoir le même nombre de lignes
    set.seed(42)
    train_data <- data.frame(
        var1 = rnorm(50), # 50 observations
        var2 = rnorm(50)
    )

    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    model$fit(train_data)

    # Données avec 30 observations (différent de 50)
    new_data <- data.frame(new_var = rnorm(30))

    expect_error(
        model$predict(new_data),
        "must have the same number of observations"
    )
})

test_that("predict() assigne correctement les nouvelles variables (scaling='self')", {
    # Test : Prédiction avec normalisation indépendante
    set.seed(42)
    train_data <- data.frame(
        var1 = rnorm(50, mean = 0, sd = 1),
        var2 = rnorm(50, mean = 0, sd = 1),
        var3 = rnorm(50, mean = 5, sd = 2)
    )

    model <- KMeansVariablesR6$new(k = 2, method = "correlation", seed = 42)
    model$fit(train_data)

    # Nouvelles variables à prédire
    new_data <- data.frame(
        new_var1 = rnorm(50, mean = 0, sd = 1),
        new_var2 = rnorm(50, mean = 5, sd = 2)
    )

    predictions <- model$predict(new_data, scaling = "self")

    # Vérifications
    expect_s3_class(predictions, "data.frame")
    expect_equal(nrow(predictions), 2) # 2 nouvelles variables
    expect_equal(ncol(predictions), 3) # variable, cluster, distance
    expect_true(all(predictions$cluster %in% 1:2))
    expect_true(all(!is.na(predictions$cluster)))
    expect_true(all(!is.na(predictions$distance)))
})

test_that("predict() fonctionne avec scaling='training'", {
    # Test : Prédiction avec paramètres de normalisation de l'entraînement
    # Note : scaling='training' nécessite que les nouvelles données aient
    # le même nombre de variables que les données d'entraînement
    set.seed(42)
    train_data <- data.frame(
        var1 = rnorm(50, mean = 10, sd = 2),
        var2 = rnorm(50, mean = 10, sd = 2),
        var3 = rnorm(50, mean = 5, sd = 1)
    )

    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    model$fit(train_data)

    # Les nouvelles données doivent avoir le même nombre de colonnes (3)
    new_data <- data.frame(
        new_var1 = rnorm(50, mean = 10, sd = 2),
        new_var2 = rnorm(50, mean = 10, sd = 2),
        new_var3 = rnorm(50, mean = 5, sd = 1)
    )

    predictions <- model$predict(new_data, scaling = "training")

    expect_true(all(!is.na(predictions$cluster)))
    expect_equal(nrow(predictions), 3)
})

test_that("predict() fonctionne avec scaling='none'", {
    # Test : Prédiction sans normalisation
    set.seed(42)
    train_data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50)
    )

    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    model$fit(train_data)

    new_data <- data.frame(new_var = rnorm(50))

    predictions <- model$predict(new_data, scaling = "none")

    expect_true(all(!is.na(predictions$cluster)))
})

test_that("predict() génère des noms de variables par défaut si absents", {
    # Test : Prédiction avec données sans nom de colonnes
    set.seed(42)
    train_data <- data.frame(var1 = rnorm(50), var2 = rnorm(50))

    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    model$fit(train_data)

    new_data_matrix <- matrix(rnorm(50), ncol = 1)
    # Pas de colnames

    predictions <- model$predict(new_data_matrix)

    expect_true(grepl("^V[0-9]+$", predictions$variable[1]))
})

# ==============================================================================
# 5. TESTS DE LA MÉTHODE ELBOW_METHOD()
# ==============================================================================

test_that("elbow_method() calcule l'inertie pour différentes valeurs de k", {
    # Test : La méthode du coude doit retourner un k optimal
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50),
        var4 = rnorm(50),
        var5 = rnorm(50),
        var6 = rnorm(50)
    )

    model <- KMeansVariablesR6$new(seed = 42)

    # Exécuter elbow_method sans graphique
    k_opt <- model$elbow_method(data, k_min = 2, k_max = 5, plot = FALSE)

    # Vérifications
    expect_true(is.numeric(k_opt))
    expect_true(k_opt >= 2 && k_opt <= 5)
})

test_that("elbow_method() rejette les données invalides", {
    # Test : Données avec NA doivent être rejetées
    model <- KMeansVariablesR6$new()
    data_with_na <- data.frame(
        var1 = c(1, 2, NA),
        var2 = c(4, 5, 6),
        var3 = c(7, 8, 9)
    )

    expect_error(
        model$elbow_method(data_with_na, plot = FALSE),
        "Data must not contain missing values"
    )
})

test_that("elbow_method() ajuste k_max si trop grand", {
    # Test : k_max > nombre de variables - 1 doit être ajusté automatiquement
    set.seed(42)
    # Utilisons 8 variables pour avoir une plage suffisante (k de 2 à 7)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50),
        var4 = rnorm(50),
        var5 = rnorm(50),
        var6 = rnorm(50),
        var7 = rnorm(50),
        var8 = rnorm(50)
    )

    model <- KMeansVariablesR6$new(seed = 42)

    # k_max = 20 mais seulement 8 variables (max possible = 7)
    # La méthode doit ajuster k_max à 7 automatiquement
    k_opt <- model$elbow_method(data, k_min = 2, k_max = 20, plot = FALSE)

    # Le k optimal doit être valide (entre 2 et 7)
    expect_true(is.numeric(k_opt))
    expect_equal(length(k_opt), 1) # Doit retourner une seule valeur
    expect_true(k_opt >= 2 && k_opt <= 7) # Maximum possible avec 8 variables
})

test_that("elbow_method() fonctionne avec différentes méthodes de distance", {
    # Test : elbow_method avec méthode euclidean
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50, mean = 0),
        var2 = rnorm(50, mean = 0),
        var3 = rnorm(50, mean = 5),
        var4 = rnorm(50, mean = 5),
        var5 = rnorm(50, mean = 10)
    )

    model <- KMeansVariablesR6$new(method = "euclidean", seed = 42)
    k_opt <- model$elbow_method(data, k_min = 2, k_max = 4, plot = FALSE)

    expect_true(k_opt >= 2 && k_opt <= 4)
})

# ==============================================================================
# 6. TESTS DES MÉTHODES UTILITAIRES
# ==============================================================================

test_that("get_center_variables() retourne les noms des centres", {
    # Test : Récupération des variables centres (medoids)
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50)
    )

    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    model$fit(data)

    centers <- model$get_center_variables()

    # Vérifications
    expect_equal(length(centers), 2) # 2 clusters = 2 centres
    expect_true(all(centers %in% colnames(data)))
})

test_that("get_center_variables() échoue si le modèle n'est pas ajusté", {
    # Test : Appel avant fit() doit échouer
    model <- KMeansVariablesR6$new(k = 2)

    expect_error(
        model$get_center_variables(),
        "Model not fitted"
    )
})

test_that("print() affiche les informations du modèle", {
    # Test : La méthode print ne doit pas lever d'erreur
    set.seed(42)
    data <- data.frame(var1 = rnorm(50), var2 = rnorm(50))

    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    model$fit(data)

    expect_output(model$print(), "KMeansVariablesR6")
    expect_output(model$print(), "fitted: TRUE")
})

test_that("summary() affiche un résumé détaillé", {
    # Test : La méthode summary affiche des informations complètes
    set.seed(42)
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50)
    )

    model <- KMeansVariablesR6$new(k = 2, seed = 42)
    model$fit(data)

    expect_output(model$summary(), "KMeansVariablesR6 Summary")
    expect_output(model$summary(), "Cluster sizes")
    expect_output(model$summary(), "Total inertia")
})

# ==============================================================================
# 7. TESTS D'INTÉGRATION ET CAS LIMITES
# ==============================================================================

test_that("Workflow complet : fit() puis predict()", {
    # Test d'intégration : Scénario complet d'utilisation
    set.seed(42)

    # 1. Créer des données d'entraînement
    train_data <- data.frame(
        height = rnorm(100, mean = 170, sd = 10),
        weight = rnorm(100, mean = 70, sd = 15),
        age = rnorm(100, mean = 30, sd = 5),
        salary = rnorm(100, mean = 50000, sd = 10000)
    )

    # 2. Initialiser et ajuster le modèle
    model <- KMeansVariablesR6$new(k = 2, method = "correlation", seed = 42)
    model$fit(train_data)

    # 3. Vérifier l'ajustement
    expect_true(model$fitted)
    expect_equal(length(model$clusters), 2)

    # 4. Prédire sur de nouvelles variables
    new_data <- data.frame(
        shoe_size = rnorm(100, mean = 42, sd = 2),
        income = rnorm(100, mean = 55000, sd = 12000)
    )

    predictions <- model$predict(new_data)

    # 5. Vérifier les prédictions
    expect_equal(nrow(predictions), 2)
    expect_true(all(predictions$cluster %in% 1:2))

    # 6. Obtenir un résumé
    expect_output(model$summary(), "KMeansVariablesR6 Summary")
})

test_that("Le modèle est reproductible avec une graine fixée", {
    # Test : Même graine = mêmes résultats
    data <- data.frame(
        var1 = rnorm(50),
        var2 = rnorm(50),
        var3 = rnorm(50)
    )

    # Premier ajustement
    model1 <- KMeansVariablesR6$new(k = 2, seed = 999)
    model1$fit(data)
    inertia1 <- model1$inertia

    # Second ajustement avec la même graine
    model2 <- KMeansVariablesR6$new(k = 2, seed = 999)
    model2$fit(data)
    inertia2 <- model2$inertia

    # Les inerties devraient être identiques
    expect_equal(inertia1, inertia2)
})

test_that("Le modèle gère correctement un grand nombre de variables", {
    # Test : Performance avec beaucoup de variables
    set.seed(42)
    n_vars <- 20
    data <- as.data.frame(matrix(rnorm(50 * n_vars), nrow = 50, ncol = n_vars))
    colnames(data) <- paste0("var", 1:n_vars)

    model <- KMeansVariablesR6$new(k = 5, seed = 42, nstart = 5)

    # fit() produit un message de succès, donc on utilise expect_message
    expect_message(model$fit(data), "Best of")
    expect_true(model$fitted)
    expect_equal(length(unlist(model$clusters)), n_vars)
})

# ==============================================================================
# MESSAGE DE FIN DES TESTS
# ==============================================================================

message("\n✓ Tests unitaires KMeansVariablesR6 terminés avec succès!")
