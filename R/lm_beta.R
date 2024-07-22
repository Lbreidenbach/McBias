#'Analyzes a data frame to give an odds ratio
#'
#'runs a linear regression of with the following parameters order like so: outcome ~ exposure + covariates.
#'
#'
#' @param exposure Character value. The name of a column in a data frame that represents exposure in an
#'
#' @param outcome Character value. The name of a \strong{Gaussian} column in a data frame that represents the outcome.
#'
#' @param covariates Character value/vector. The name(s) of the columns that the user wants adjust on.
#'
#' @param df The data frame for analysis
#'
#' @return A six column data frame with the following summary statistics:
#' * odds ratio (reads NA)
#' * beta coeffecient
#' * lower 95% confidence interval of beta coefficient
#' * upper 95% confidence interval of beta coefficient
#' * p value
#' * number of samples in the dataset
#'
#'@examples examples/effect_estimate_functions.R
#'@seealso [varied_runs()],[apply_methods()]
#'@export
#'

lm_beta = function(exposure, outcome, covariates=NULL, df){
  vars = c(exposure, covariates)
  lm1 = stats::lm(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df)
  confint = confint(lm1, parm = exposure, trace = F)
  upper_int = confint[1,2]
  lower_int = confint[1,1]
  beta = as.numeric(lm1$coefficients[2])
  regression_df = data.frame("odds_ratio" = NA,
                             beta = beta,
                             lower_int =lower_int,
                             upper_int = upper_int,
                             confint_diff = abs(upper_int-lower_int),
                             p_val = coef(summary(lm1))[2,4],
                             n = nrow(df))

  rownames(regression_df) = "linear_regression"
  return(regression_df)
}
