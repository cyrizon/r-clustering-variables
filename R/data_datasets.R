#' acp_voitures
#'
#' Example dataset: cars measurements used for demonstration of PCA/clustering
#' @format A data.frame with car-related variables (rows = observations, columns = variables)
#' @source datasets/acp_voitures.csv
#' @examples
#' data(acp_voitures)
#' head(acp_voitures)
"acp_voitures"

#' AUTOS2005subset
#'
#' Subset of AUTOS2005 dataset (example automobile data)
#' @format A data.frame with automobile measurements and categorical features
#' @source datasets/AUTOS2005subset.txt
#' @examples
#' data(AUTOS2005subset)
#' head(AUTOS2005subset)
"AUTOS2005subset"

#' jobrate
#'
#' Employment/job rate dataset for examples
#' @format A data.frame with job rate related variables
#' @source datasets/jobrate.csv
#' @examples
#' data(jobrate)
#' head(jobrate)
"jobrate"

#' protein_dataset
#'
#' Protein measurements example dataset
#' @format A data.frame with protein measurement variables
#' @source datasets/protein_dataset.csv
#' @examples
#' data(protein_dataset)
#' head(protein_dataset)
"protein_dataset"

#' vehicle
#'
#' Vehicle dataset for clustering demonstrations
#' @format A data.frame with vehicle characteristics
#' @source datasets/vehicle.csv
#' @examples
#' data(vehicle)
#' head(vehicle)
"vehicle"

#' vote_catvarclus
#'
#' Synthetic voting / categorical variables dataset for ACM examples
#' @format A data.frame with categorical variables suitable for ACM
#' @source datasets/vote_catvarclus.csv
#' @examples
#' data(vote_catvarclus)
#' head(vote_catvarclus)
"vote_catvarclus"

#' tea_data
#'
#' Tea consumption and preferences dataset with categorical variables
#' @format A data.frame with 300 observations and 18 categorical variables describing tea consumption patterns:
#'   \describe{
#'     \item{breakfast, tea.time, evening, lunch, dinner, always}{Timing of tea consumption (binary: yes/not)}
#'     \item{home, work, tearoom, friends, resto, pub}{Places where tea is consumed (binary: yes/not)}
#'     \item{Tea}{Type of tea preferred (black, Earl Grey, green)}
#'     \item{How}{How tea is taken (alone, milk, lemon, other)}
#'     \item{sugar}{Sugar usage (sugar, No.sugar)}
#'     \item{how}{Packaging format (tea bag, unpackaged, tea bag+unpackaged)}
#'     \item{where}{Purchase location (chain store, tea shop, chain store+tea shop)}
#'     \item{price}{Price category (p_branded, p_variable, p_upscale, p_private label, p_unknown)}
#'   }
#'   All variables are factors, making this dataset ideal for ACM (Multiple Correspondence Analysis) clustering.
#' @source datasets/tea_data.csv
#' @examples
#' data(tea_data)
#' head(tea_data)
#' # Excellent dataset for ACM clustering of categorical variables
#' \dontrun{
#' # Example ACM clustering
#' model_acm <- ClustVarACM$new(K = 3)
#' model_acm$fit(tea_data)
#' model_acm$plot(type = "biplot")
#' }
"tea_data"
