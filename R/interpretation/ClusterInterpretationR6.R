#' R6 class for interpreting variable clustering results
#'
#' Generates tables and indicators to interpret the nature of groups and membership strength.
#'
ClusterInterpretationR6 <- R6::R6Class(
    "ClusterInterpretationR6",
    public = list(
        # Summary table of assignments
        summary_table = function(pred_df) {
            # pred_df: data.frame with variable, cluster, distance (or correlation)
            if (is.null(pred_df) || nrow(pred_df) == 0) {
                warning("The prediction table is empty.")
                return(data.frame())
            }

            # Determine the name of the numeric column (distance or avg_correlation)
            numeric_col <- colnames(pred_df)[3]

            aggregate(as.formula(paste(numeric_col, "~ cluster")),
                data = pred_df,
                FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
            )
        },
        # Membership degree indicator (normalized distance or correlation)
        membership_degree = function(pred_df) {
            # Get the numeric column name
            numeric_col <- colnames(pred_df)[3]

            # Lower distance or higher correlation indicates stronger membership
            max_val <- max(pred_df[[numeric_col]])

            # For distance, compute 1 - (distance / max_distance)
            # For correlation, use correlation (already between 0 and 1)
            if (numeric_col == "distance") {
                pred_df$membership <- 1 - (pred_df[[numeric_col]] / max_val)
            } else if (numeric_col == "avg_correlation") {
                pred_df$membership <- pred_df[[numeric_col]] / max_val
            }

            return(pred_df[, c("variable", "cluster", "membership")])
        }
    )
)
