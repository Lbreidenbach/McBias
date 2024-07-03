#'Analyzes a data frame to give an odds ratio
#'
#'runs a logistic regression of with the following parameters order like so: outcome ~ exposure + covariates.
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
#' * odds ratio
#' * beta (log odds)
#' * lower 95% confidence interval
#' * upper 95% confidence interval
#' * p value
#' * number of samples in the dataset
#'
#'@examples
#'@seealso [varied_runs()],[apply_methods()]
#'@export
#'

odds_ratio = function(exposure, outcome, covariates=NULL, df){
  vars = c(exposure, covariates)
  cont_glm = stats::glm(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df, family = "binomial")
  exp_coef = as.numeric(cont_glm$coefficients[2])
  exp_or = exp(exp_coef)
  or_confint = stats::confint.default(cont_glm, trace = F)
  or_upper = or_confint[2,2]
  or_lower = or_confint[2,1]
  int_diff = or_upper - or_lower

  or_df = data.frame("odds_ratio" = exp_or,
                     beta = exp_coef,
                     lower_int = or_lower,
                     upper_int = or_upper,
                     confint_diff = abs(int_diff),
                     p_val = coef(summary(cont_glm))[2,4],
                     n = nrow(df))
  row.names(or_df) = "logistic_or"
  return(or_df)
}

