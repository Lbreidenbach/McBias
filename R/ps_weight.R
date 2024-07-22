#'Analyzes a data frame to give an odds ratio
#'
#'runs a linear or logistic regression depending on if the outcome is Gaussian or binary. Adjusts for covariates with given weights
#'The regression is created with the following parameters ordered like so: outcome ~ exposure + covariates.
#'
#'
#' @param exposure Character value. The name of a column in a data frame that represents exposure in an
#'
#' @param outcome Character value. The name of a column in a data frame that represents the outcome.
#'
#' @param covariates Character value/vector. The name(s) of the columns that the user wants adjust on.
#'
#' @param df The data frame for analysis. must have a column that includes weights. Users may generate a weight column with get_ps()
#'
#' @param weights Character value. The name of a column in a data frame that represents the weights the user wishes to use.
#'
#' @return A six column data frame with the following summary statistics:
#' * odds ratio (reads NA if outcome is Gaussian)
#' * beta coeffecient
#' * lower 95% confidence interval of beta coefficient
#' * upper 95% confidence interval of beta coefficient
#' * p value
#' * number of samples in the dataset
#'
#'@example examples/effect_estimate_functions.R
#'@seealso [varied_runs()],[apply_methods()], [get_ps()]
#'@export
#'

ps_weight = function(exposure, outcome, covariates, df, weights){
  vars = c(exposure, covariates)
  if(class(df[,outcome]) == "numeric" ){
    cont_glm = stats::lm(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df, weights = weights)
  }else if(class(df[,outcome]) == "integer"){
    cont_glm = stats::glm(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df, weights = weights, family = "quasibinomial")
  }else{
    warning("exposure must be numeric or integer")
  }

  exp_coef = as.numeric(cont_glm$coefficients[2])
  exp_or = exp(exp_coef)
  or_confint = stats::confint.default(cont_glm, parm = exposure, trace = F)
  or_upper = or_confint[1,2]
  or_lower = or_confint[1,1]
  int_diff = or_upper - or_lower
  if(class(df[,outcome]) == "numeric" ){
    exp_or = NA
  }


  or_df = data.frame("odds_ratio" = exp_or,
                     beta = exp_coef,
                     lower_int = or_lower,
                     upper_int = or_upper,
                     confint_diff = abs(int_diff),
                     p_val = coef(summary(cont_glm))[2,4],
                     n = nrow(df))
  row.names(or_df) = "ps_weighting"
  return(or_df)
}


