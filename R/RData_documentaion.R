#' Use Case 1 Output data, Demonstrates how different adjustment sets affect bias.
#'
#' The output list data used for constructing the use case figures.
#' bmi_results columns represent the following:
#' b adjusts t2d (a2) is BMI ~ rs7903146T + T2D,
#' a  adjusts all (a3) is BMI ~ rs7903146T + WC + SBP + T2D + PAD + CHD,
#' c naive (a1) is  BMI ~ rs7903146T
#'
#' wc_results columns represent the following:
#' b adjusts t2d (b2) is WC ~ rs7903146T + T2D,
#' a  adjusts all (b3) is WC ~ rs7903146T + BMI + SBP + T2D + PAD + CHD,
#' c naive (a1) is  WC ~ rs7903146T
#'
#' @docType data
#' @keywords data
#' @name "UseCase1Results.RData"
#' @usage load(UseCase1Results)
#' @format Two list outputs from reparse_runs() with 11 elements where each element is a 2 dimensional matrix of 500x3 where each column represents each of the 3 adjustment sets and each row represents the calculation for each of the 500 iterations
#' \describe{
#'   \item{ratio}{Odds ratio. Reads as NA since the ouctome is continuous}
#'   \item{calculated_ate}{The effect size calculated from the simulated datasets with the corresponding adjustment set}
#'   \item{lower_int}{The lower 95% confidence interval of the calculated effect size}
#'   \item{upper_int}{The upper 95% confidence interval of the calculated effect size}
#'   \item{p_values}{The p value of the calculated effect size}
#'   \item{exp_prevalence}{The prevalence of the exposure (rs7903146T)}
#'   \item{out_prevalence}{The prevalence of the outcome, NA since outcome is continuous}
#'   \item{sample_population}{sample size of the simulated dataset}
#'   \item{set_ate}{the effect size the BN. If no edge is between the nodes, the set effect size is assumed to be 0}
#'   \item{over_r}{percentage of 0s changed to 1s in a selected binary vairable. defaults to 0}
#'   \item{under_r}{percentage of 1s changed to 0s in a selected binary vairable. defaults to 0}
#' }
NULL

#' Use Case 2 Output data, Demonstrates how different adjustment sets affect bias.
#'
#' The output matrix data used for constructing the use case figures.
#'
#' tot_bmi_run columns represent the following:
#' naive is BMI ~ Rs303752
#' naive, UKB selection bias is BMI ~ Rs303752 where In_UKB is stratified
#'
#' tot_edu_run columns represent the following:
#' naive is Educational_age ~ Rs303752
#' naive, UKB selection bias is Educational_age ~ Rs303752 where In_UKB is stratified
#'
#' @docType data
#' @keywords data
#' @name "UseCase2Results.RData"
#' @usage load(UseCase2Results)
#' @format Two list outputs from reparse_runs() with 11 elements where each element is a 2 dimensional matrix of 500x3 where each column represents each of the 3 adjustment sets and each row represents the calculation for each of the 500 iterations
#' \describe{
#'   \item{ratio}{Odds ratio. Reads as NA since the ouctome is continuous}
#'   \item{calculated_ate}{The effect size calculated from the simulated datasets with the corresponding adjustment set}
#'   \item{lower_int}{The lower 95% confidence interval of the calculated effect size}
#'   \item{upper_int}{The upper 95% confidence interval of the calculated effect size}
#'   \item{p_values}{The p value of the calculated effect size}
#'   \item{exp_prevalence}{The prevalence of the exposure (rs7903146T)}
#'   \item{out_prevalence}{The prevalence of the outcome, NA since outcome is continuous}
#'   \item{sample_population}{sample size of the simulated dataset.
#'   The "naive, UKB selection bias" sample sizes vary slightly since it reports the number of samples remaining after "In_UKB" is stratified}
#'   \item{set_ate}{the effect size the BN}
#'   \item{over_r}{percentage of 0s changed to 1s in a selected binary vairable. defaults to 0}
#'   \item{under_r}{percentage of 1s changed to 0s in a selected binary vairable. defaults to 0}
#' }
NULL

