#' R6 class for visualization of variable clustering results
#'
#' Generates plots to interpret partitions and groups.
#'
ClusterVisualizationR6 <- R6::R6Class(
    "ClusterVisualizationR6",
    public = list(
        # Visualization of clusters (barplot of number of variables per cluster)
        plot_cluster_sizes = function(clusters) {
            sizes <- as.numeric(sapply(clusters, length))
            if (length(sizes) == 0) {
                warning("No clusters to display.")
                return(invisible(NULL))
            }
            names(sizes) <- paste0("Cluster ", seq_along(sizes))
            barplot(sizes, main = "Number of variables per cluster", xlab = "Cluster", ylab = "Number of variables", col = "skyblue")
        },
        # Heatmap of cluster centers
        plot_centers_heatmap = function(centers) {
            if (!requireNamespace("pheatmap", quietly = TRUE)) {
                stop("The 'pheatmap' package is required for heatmaps. Please install it if needed.")
            }
            pheatmap::pheatmap(centers, main = "Cluster centers heatmap")
        },
        # Correlation dendrogram
        plot_correlation_dendrogram = function(X) {
            if (!is.data.frame(X) && !is.matrix(X)) stop("X must be a data.frame or matrix.")
            if (!all(sapply(X, is.numeric))) stop("All variables must be numeric.")
            if (anyNA(X)) stop("Data must not contain NA.")

            # Compute the correlation matrix
            cor_mat <- cor(X)
            dist_mat <- as.dist(1 - abs(cor_mat))

            # Hierarchical clustering
            hc <- hclust(dist_mat, method = "ward.D2")

            # Generate the dendrogram
            plot(hc, main = "Correlation dendrogram", xlab = "Variables", sub = "", cex = 0.8)
        }
    )
)
