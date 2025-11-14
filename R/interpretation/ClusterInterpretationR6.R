#' Classe R6 pour l'interprétation des résultats de clustering de variables
#'
#' Permet de générer des tableaux et indicateurs pour interpréter la nature des groupes et le degré d'appartenance.
#'
ClusterInterpretationR6 <- R6::R6Class(
    "ClusterInterpretationR6",
    public = list(
        # Tableau récapitulatif des affectations
        summary_table = function(pred_df) {
            # pred_df : data.frame avec variable, cluster, distance (ou correlation)
            if (is.null(pred_df) || nrow(pred_df) == 0) {
                warning("Le tableau de prédiction est vide.")
                return(data.frame())
            }

            # Déterminer le nom de la colonne numérique (distance ou avg_correlation)
            numeric_col <- colnames(pred_df)[3]

            aggregate(as.formula(paste(numeric_col, "~ cluster")),
                data = pred_df,
                FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
            )
        },
        # Indicateur de degré d'appartenance (distance ou corrélation normalisée)
        membership_degree = function(pred_df) {
            # Récupérer le nom de la colonne numérique
            numeric_col <- colnames(pred_df)[3]

            # Plus la distance est faible ou la corrélation est élevée, plus l'appartenance est forte
            max_val <- max(pred_df[[numeric_col]])

            # Pour la distance, on veut 1 - (distance / max_distance)
            # Pour la corrélation, on veut simplement la corrélation (déjà entre 0 et 1)
            if (numeric_col == "distance") {
                pred_df$membership <- 1 - (pred_df[[numeric_col]] / max_val)
            } else if (numeric_col == "avg_correlation") {
                pred_df$membership <- pred_df[[numeric_col]] / max_val
            }

            return(pred_df[, c("variable", "cluster", "membership")])
        }
    )
)
