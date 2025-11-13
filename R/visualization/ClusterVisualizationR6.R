#' Classe R6 pour la visualisation des résultats de clustering de variables
#'
#' Permet de générer des graphiques pour interpréter les partitions et groupes.
#'
ClusterVisualizationR6 <- R6::R6Class(
    "ClusterVisualizationR6",
    public = list(
        # Visualisation des clusters (barplot du nombre de variables par cluster)
        plot_cluster_sizes = function(clusters) {
            sizes <- as.numeric(sapply(clusters, length))
            if (length(sizes) == 0) {
                warning("Aucun cluster à afficher.")
                return(invisible(NULL))
            }
            names(sizes) <- paste0("Cluster ", seq_along(sizes))
            barplot(sizes, main = "Nombre de variables par cluster", xlab = "Cluster", ylab = "Nombre de variables", col = "skyblue")
        },
        # Heatmap des centres des clusters
        plot_centers_heatmap = function(centers) {
            if (!requireNamespace("pheatmap", quietly = TRUE)) {
                stop("Le package 'pheatmap' est requis pour la heatmap. Installez-le si besoin.")
            }
            pheatmap::pheatmap(centers, main = "Centres des clusters de variables")
        },
        # Dendrogramme des corrélations
        plot_correlation_dendrogram = function(X) {
            if (!is.data.frame(X) && !is.matrix(X)) stop("X doit être un data.frame ou une matrice.")
            if (!all(sapply(X, is.numeric))) stop("Toutes les variables doivent être numériques.")
            if (anyNA(X)) stop("Les données ne doivent pas contenir de NA.")

            # Calcul de la matrice de corrélation
            cor_mat <- cor(X)
            dist_mat <- as.dist(1 - abs(cor_mat))

            # Clustering hiérarchique
            hc <- hclust(dist_mat, method = "ward.D2")

            # Génération du dendrogramme
            plot(hc, main = "Dendrogramme des corrélations", xlab = "Variables", sub = "", cex = 0.8)
        }
    )
)
