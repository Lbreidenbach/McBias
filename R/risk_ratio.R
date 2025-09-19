#'@noRd

risk_ratio <- function(exposure,
                       outcome,
                       covariates = NULL,
                       df) {
  require(logisticRR)
  vars <- c(exposure, covariates)
  cont_glm <- logisticRR::logisticRR(as.formula(paste(outcome,
                                                      paste(vars,
                                                            collapse = " + "),
                                                      sep = " ~ ")), data = df)

  exp_coef <- cont_glm$fit$coefficients[[2]]
  exp_rr <- cont_glm$RR

  or_confint <- confint.default(cont_glm$fit,
                                parm = exposure,
                                trace = FALSE)
  or_upper <- or_confint[1, 2]
  or_lower <- or_confint[1, 1]
  int_diff <- or_upper - or_lower

  or_df <- data.frame("odds_ratio" = exp_rr,
                      beta = exp_coef,
                      lower_int = or_lower,
                      upper_int = or_upper,
                      confint_diff = abs(int_diff),
                      p_val = coef(summary(cont_glm$fit))[2, 4],
                      n = nrow(df))
  row.names(or_df) <- "rr_regression"
  return(or_df)
}
