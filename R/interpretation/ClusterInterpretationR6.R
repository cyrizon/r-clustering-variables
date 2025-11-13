# Classe R6 pour l'interprétation des résultats de clustering de variables

# Permet de générer des tableaux et indicateurs pour interpréter la nature des groupes et le degré d'appartenance.
ClusterInterpretationR6 <- R6::R6Class(
    "ClusterInterpretationR6",
    public = list(
        # Tableau récapitulatif des affectations
        summary_table = function(pred_df) {
            # pred_df : data.frame avec variable, cluster, distance
            if (is.null(pred_df) || nrow(pred_df) == 0) {
                warning("Le tableau de prédiction est vide.")
                return(data.frame())
            }
            aggregate(distance ~ cluster, data = pred_df, FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)))
        },
        # Indicateur de degré d'appartenance (distance normalisée)
        membership_degree = function(pred_df) {
            # Plus la distance est faible, plus l'appartenance est forte
            max_dist <- max(pred_df$distance)
            pred_df$membership <- 1 - (pred_df$distance / max_dist)
            return(pred_df[, c("variable", "cluster", "membership")])
        }
    )
)
