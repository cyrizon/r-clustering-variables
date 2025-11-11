# --- 1. Préparation de l'environnement ---

# Charger les packages nécessaires
# R6 est indispensable pour la définition des classes
if (!requireNamespace("R6", quietly = TRUE)) {
  install.packages("R6")
}
# Le package 'mclust' est utilisé ici pour la fonction 'adjustedRandIndex'
# qui permet de comparer la similarité des partitions (optionnel mais utile)
if (!requireNamespace("mclust", quietly = TRUE)) {
  install.packages("mclust")
}

library(R6)
library(mclust) # Pour l'Adjusted Rand Index

# Charger les définitions de classes R6
# Assurez-vous que ces fichiers sont dans votre répertoire de travail
source("CAH/test.R") # Contient KMeansVariablesR6
source("CAH/hac.R")  # Contient HACVariablesR6

cat("Classes R6 chargées avec succès.\n")
cat("----------------------------------\n")

# --- 2. Sélection du Jeu de Données ---

# Utilisation d'un jeu de données intégré à R pour la démonstration.
# 'mtcars' est un data frame de 32 observations sur 11 variables numériques.
# data_to_cluster <- mtcars
# Ou charger vos propres données (remplacez par votre chemin/fichier si besoin) :
data_to_cluster <- read_excel("CAH/jobrate.xlsx")


cat(paste("Dimensions :", nrow(data_to_cluster), "observations et", ncol(data_to_cluster), "variables.\n"))
cat("----------------------------------\n")

# --- 3. Paramètres de Clustering ---

# Définir le nombre de clusters 'k' à tester (doit être > 1 et < nombre de variables)
K_CLUSTERS <- 4
cat(paste("Nombre de clusters (k) défini à :", K_CLUSTERS, "\n"))
cat("----------------------------------\n")

# --- 4. Exécution du K-means de Variables ($KMeansVariablesR6$) ---

# 4.1. Instanciation de la classe K-means
kmeans_model <- KMeansVariablesR6$new(k = K_CLUSTERS, method = "correlation")

# 4.2. Ajustement du modèle
cat("✅ Démarrage de l'ajustement K-means (méthode corrélation)...\n")
tryCatch({
  kmeans_model$fit(data_to_cluster)
  cat("   K-means ajusté avec succès.\n")
}, error = function(e) {
  cat(paste("   Erreur lors de l'ajustement K-means:", e$message, "\n"))
})

# 4.3. Affichage succinct des résultats (méthode $print)
kmeans_model$print()

# Pour la comparaison, on extrait les affectations de cluster sous forme de vecteur nommé
kmeans_assignments <- unlist(lapply(names(kmeans_model$clusters), function(name) {
  setNames(rep(as.numeric(name), length(kmeans_model$clusters[[name]])), kmeans_model$clusters[[name]])
}))
kmeans_assignments <- kmeans_assignments[colnames(data_to_cluster)] # S'assurer du bon ordre
cat("----------------------------------\n")

# --- 5. Exécution de la CAH de Variables ($HACVariablesR6$) ---

# 5.1. Instanciation de la classe HAC
hac_model <- HACVariablesR6$new(k = K_CLUSTERS,
                                distance_method = "correlation",
                                linkage_method = "ward.D2")

# 5.2. Ajustement du modèle
cat("✅ Démarrage de l'ajustement CAH (méthode Ward.D2, corrélation)...\n")
tryCatch({
  hac_model$fit(data_to_cluster)
  cat("   CAH ajustée avec succès.\n")
}, error = function(e) {
  cat(paste("   Erreur lors de l'ajustement CAH:", e$message, "\n"))
})

# 5.3. Affichage détaillé des résultats (méthode $summary)
hac_model$summary()

# 5.4. Visualisation du dendrogramme (un outil d'interprétation graphique)
cat("Affichage du dendrogramme (CAH) :\n")
hac_model$plot(k = K_CLUSTERS) # Affiche le dendrogramme coupé à K_CLUSTERS

# Pour la comparaison, on extrait les affectations de cluster
hac_assignments <- unlist(lapply(names(hac_model$clusters), function(name) {
  setNames(rep(as.numeric(name), length(hac_model$clusters[[name]])), hac_model$clusters[[name]])
}))
hac_assignments <- hac_assignments[colnames(data_to_cluster)] # S'assurer du bon ordre
cat("----------------------------------\n")

# --- 6. Comparaison des Partitions (Indicateur) ---

# Le Rand Index ajusté (ARI) mesure la similarité entre deux partitions,
# avec 1.0 indiquant une partition parfaite et 0.0 une similarité aléatoire.
cat("📊 Comparaison des partitions (K-means vs CAH) :\n")

if (length(kmeans_assignments) == length(hac_assignments)) {
  ari <- adjustedRandIndex(kmeans_assignments, hac_assignments)
  cat(paste("   Adjusted Rand Index (ARI) :", round(ari, 4), "\n"))

  if (ari > 0.8) {
    cat("   => Les deux méthodes ont produit des partitions très similaires.\n")
  } else if (ari > 0.5) {
    cat("   => Les partitions sont modérément similaires.\n")
  } else {
    cat("   => Les méthodes ont produit des partitions largement différentes.\n")
  }
} else {
  cat("   Impossible de comparer : les vecteurs d'affectation n'ont pas la même taille.\n")
}

cat("----------------------------------\n")

# --- 7. Exemple d'utilisation de la fonction $predict (CAH) ---
