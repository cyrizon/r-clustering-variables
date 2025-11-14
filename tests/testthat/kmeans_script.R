# Charger les bibliothèques nécessaires
library(R6)
library(pheatmap)

# Charger les classes et fonctions du package
source("R/algorithms/KMeansVariablesR6.R") # Exemple d'algorithme de clustering
source("R/interpretation/ClusterInterpretationR6.R") # Outils d'interprétation
source("R/visualization/ClusterVisualizationR6.R") # Outils de visualisation

# Charger le dataset College_Data
data <- read.csv("tests/testthat/College_Data", row.names = 1)

# Prétraitement des données
# On sélectionne uniquement les colonnes numériques pour le clustering
X <- data[sapply(data, is.numeric)]

# Étape 1 : Suggérer le nombre optimal de clusters
kmeans_clustering <- KMeansVariablesR6$new(method = "correlation")
optimal_k <- kmeans_clustering$suggest_k_automatic(X, max_k = 10, method = "silhouette")
cat("Nombre optimal de clusters suggéré :", optimal_k, "\n")

# Étape 2 : Clustering des variables avec le k suggéré
kmeans_clustering <- KMeansVariablesR6$new(k = optimal_k)
kmeans_clustering$fit(X) # Appliquer le clustering sur les données

# Étape 3 : Interprétation des résultats
# Créer une instance de la classe d'interprétation
cluster_interpretation <- ClusterInterpretationR6$new()

# Construire un data.frame pour l'interprétation
pred_df <- data.frame(
    variable = names(kmeans_clustering$model$cluster),
    cluster = kmeans_clustering$model$cluster,
    distance = kmeans_clustering$model$tot.withinss # Exemple de distance
)

# Résumer les clusters
cluster_summary <- cluster_interpretation$summary_table(pred_df)

# Afficher un résumé des clusters
print(cluster_summary)

# Étape 4 : Visualisation des résultats
# Créer une instance de la classe de visualisation
cluster_visualization <- ClusterVisualizationR6$new()

# Visualiser la taille des clusters
cluster_visualization$plot_cluster_sizes(kmeans_clustering$clusters)

# Visualiser les centres des clusters (si disponibles)
if (!is.null(kmeans_clustering$centers)) {
    cluster_visualization$plot_centers_heatmap(kmeans_clustering$centers)
}

# Visualiser le dendrogramme des corrélations
cluster_visualization$plot_correlation_dendrogram(X)

