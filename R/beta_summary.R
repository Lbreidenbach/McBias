#' Returns the total bias, null rejection rate, coverage, and effect size mean and standard deviation
#'
#' Specifically beta_summary returns:
#' * Total bias: the average effect size estimate - the set effect size. Also returns its standard error
#' * Null rejection rate: the percentage of p values that were less than the set alpha value. Also returns its standard error
#' * Coverage: the percent of effect size estimates that contained the set effect size in its 95% confidence interval. Also returns its standard error
#' * The mean effect size estimate
#' * The standard deviation of the effect size estimate
#'
#' @param run a ScenarioMatrix object as returned by varied_runs()
#' @param a The alpha value for calculating the null rejection rate, defaults to 0.05
#' @returns A data frame with the following summary statistics listed above
#' @example McBias/examples/Dag_example.R
#' @seealso [varied_runs()], [ci_ridges()]
#' @export


beta_summary = function(run, a = 0.05){
  the_table = suppressWarnings(beta_sum(run,a))
  the_table[c(7,8,11,12,14,15),] = the_table[c(7,8,11,12,14,15),]*100
  the_table = the_table[c(5:8, 10:15),]
  the_table = t(the_table)
  the_table = as.data.frame(the_table)
  the_table = the_table[,c(5,8,7,10,6,9,1,2)]
  colnames(the_table) = c("bias", "bias_se", "coverage", "coverage_se", "rejection_rate", "rejection_rate_se",
                          "mean_b_estimate", "b_estimate_std_dev")

  return(the_table)
}
