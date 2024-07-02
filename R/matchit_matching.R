#'Interfaces with the MatchIt package to create a matched dataset as specifed by the user
#'
#'Uses MatchIt package as a way of adjusting for covariates. Matches cases to controls from the set "exposure" column
#'Note that users are limited to the greedy "nearest" method, though they may enter in different distance calculation methods
#'
#' @param exposure Character value. The name of a binary column in a data frame that represents the cases and controls that need to be matched
#'
#' @param covariates Character value/vector. The name(s) of the columns that the user wants to match the cases and controls on.
#'
#' @param df The data frame set for matching
#'
#' @param d the method in which distance between a matched case and control is measured as specified by MatchIt. Defaults to "logit"
#'
#' @param ratio Integer. number of controls that should be matched to each case. Defaults to 1
#'
#' @return A data frame where each case from the exposure column is matched to a set number of controls based on the named covariates.
#'
#'@examples McBias/examples/MatchIt_example.R
#'@seealso [varied_runs()]
#'@export
#'


matchit_matching = function(exposure, covariates, df, d = "logit", ratio = 1){
  psm = matchit(as.formula(paste(exposure, paste(covariates, collapse=" + "), sep=" ~ ")), data = df, method = "nearest", distance = d, ratio = ratio)
  treated_index = rownames(psm$match.matrix)
  untreated_index = c(psm$match.matrix[1:length(psm$match.matrix)])
  treated_subset = df[treated_index, ]
  untreated_subset = df[untreated_index, ]
  return(rbind(treated_subset, untreated_subset))
}
