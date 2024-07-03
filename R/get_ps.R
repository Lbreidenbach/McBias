#'Interfaces with the MatchIt package to create a matched dataset as specifed by the user
#'
#'Uses MatchIt package as a way of adjusting for covariates. Matches cases to controls from the set "exposure" column
#'Note that users are limited to the greedy "nearest" method, though they may enter in different distance calculation methods
#'
#' @param exposure Character value. The name of the column in a data frame that represents the exposure
#'
#' @param covariates Character value/vector. The name(s) of the columns that the user wants to adjust on.
#'
#' @param df The data frame set for calculating propensity score values
#'
#' @return The original data frame with an additional propensity score column and an additional propensity score weight column
#'
#'@examples McBias/examples/propensity_score_df.R
#'@seealso [varied_runs()]
#'@export
#'

get_ps = function(exposure, covariates, df){
  if(class(df[,exposure]) == "numeric" ){
    ps_mod = lm(as.formula(paste(exposure, paste(covariates, collapse=" + "), sep=" ~ ")), data = df)
    ps = as.numeric(plogis(fitted.values(ps_mod)))
    num_mod = lm(as.formula(paste(exposure, 1, sep=" ~ ")), data = df)
    num = as.numeric(plogis(fitted.values(num_mod)))
  }else if(class(df[,exposure]) == "integer"){
    ps_mod <- glm(as.formula(paste(exposure, paste(covariates, collapse=" + "), sep=" ~ ")), data = df, family="binomial")
    ps = fitted(ps_mod)
    num_mod = glm(as.formula(paste(exposure, 1, sep=" ~ ")), data = df, family = "binomial")
    num = fitted(num_mod)
  }else{
    print("exposure must be numeric or integer class")
  }

  df_out = data.frame(ps = ps,
                      weights = num/ps)
  df$ps = df_out$ps
  df$weights = df_out$weights
  return(df)
}
