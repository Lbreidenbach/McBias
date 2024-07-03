#'Analyzes a data frame to give an risk ratio
#'
#'returns a relative risk value via the delta method offered in the logisticRR package. The equation is created with the following parameters arranged like so: outcome ~ exposure + covariates.
#'
#'
#' @param exposure Character value. The name of a column in a data frame that represents exposure in an
#'
#' @param outcome Character value. The name of a \strong{binary} column in a data frame that represents the outcome.
#'
#' @param covariates Character value/vector. The name(s) of the columns that the user wants adjust on.
#'
#' @param df The data frame for analysis
#'
#' @return A six column data frame with the following summary statistics:
#' * risk ratio
#' * beta (log odds)
#' * lower 95% confidence interval
#' * upper 95% confidence interval
#' * p value
#' * number of samples in the dataset
#'
#'@examples
#'@seealso [varied_runs()], [apply_methods()]
#'@export
#'


risk_ratio = function(exposure, outcome, covariates=NULL, df){
  require(logisticRR)
  vars = c(exposure, covariates)
  cont_glm = logisticRR::logisticRR(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df)
  exp_coef = cont_glm$fit$coefficients[[2]]
  exp_rr = cont_glm$RR
  or_confint = confint.default(cont_glm$fit, trace = F)
  or_upper = or_confint[2,2]
  or_lower = or_confint[2,1]
  int_diff = or_upper - or_lower

  or_df = data.frame("odds_ratio" = exp_rr,
                     beta = exp_coef,
                     lower_int = or_lower,
                     upper_int = or_upper,
                     confint_diff = abs(int_diff),
                     p_val = coef(summary(cont_glm$fit))[2,4],
                     n = nrow(df))
  row.names(or_df) = "rr_regression"
  return(or_df)
}



