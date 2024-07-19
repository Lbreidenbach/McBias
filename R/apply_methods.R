#'Analyzes a data frame with various methods and reports the summary statistics
#'
#'runs the appropriate methods based on the outcome's distribution. Also can run matching methods if set by the user. Returns the summary statistics of each analysis to help with method comparison.
#'
#'
#' @param exposure Character value. The column name in a data frame that represents exposure in an
#'
#' @param outcome Character value. The column name in a data frame that represents the outcome.
#'
#' @param covariates Character value/vector. The name(s) of the columns that the user wants adjust on. Defaults to NULL meaning no covariates are adjusted on.
#'
#' @param sb Character value/vector. The name(s) of \strong{binary} column(s). Stratifies the data frame to include only the rows with 1s in every listed column.
#'
#' @param df The data frame for analysis
#'
#' @param ratio Integer. number of controls that should be matched to each case. Defaults to 1
#'
#' @param match_methods Character value/vector. The method(s) in which distance between a matched case and control is measured as specified by MatchIt. Defaults to NULL which does no matching analysis.
#'
#' @return A six column data frame of n rows where n is the number of different methods run. The columns represent the following summary statistics:
#' * odds ratio
#' * beta (log odds)
#' * lower 95% confidence interval of beta coefficient
#' * upper 95% confidence interval of beta coefficient
#' * p value
#' * number of samples in the dataset
#'
#'@examples McBias/examples/apply_methods_to_matrix.R
#'@seealso [varied_runs()], [lm_beta()], [odds_ratio()], [get_ps()], [matchit_matching()], [ps_weight()], [risk_ratio()]
#'@export
#'



apply_methods = function(exposure, outcome, covariates=NULL, sb=NULL, df, ratio=1, match_methods = NULL){
  re = function(df, name){
    rownames(df) = name
    return(df)
  }
  div=4
  #create empty data frame
  tot_df = data.frame("odds_ratio" = as.numeric(),
                      beta = as.numeric(),
                      lower_int = as.numeric(),
                      upper_int = as.numeric(),
                      confint_diff = as.numeric(),
                      p_val = as.numeric(),
                      n = as.numeric())
  if(is.null(match_methods)==F & is.null(covariates)==T){
    warning("Matching methods cannot be used if no covariates are given and thus were not run.")
  }

  #set selection bias here
  if(length(names(which(unlist(lapply(df[,sb],class))=="numeric")))!=0){
    cont_sb = names(which(unlist(lapply(df[,sb],class))=="numeric"))

    warning(paste0("the following nodes are continuous and cannot be in the sb argument: ",
                   paste0(cont_sb, collapse = ", ")), call. = FALSE)
  }
  if(is.null(sb)==F){
    for(i in sb){
      sb_df = bi_strat(1, df, i)[[1]]
      df = sb_df
    }

  }
  #rewriting for less if/else conditions

  #risk_ratio_quals
  # if(class(df[,outcome])=="integer" & length(find.package("logisticRR", quiet=TRUE))==1){
  #   tot_df = tot_bind(list(tot_df, risk_ratio(exposure, outcome, covariates, df = df)))
  # }

  #odds ratio quals
  if(class(df[,outcome])=="integer"){
    tot_df = tot_bind(list(tot_df, odds_ratio(exposure, outcome, covariates, df = df)))
  }

  #lm beta quals
  if(class(df[,outcome])=="numeric"){
    tot_df = tot_bind(list(tot_df, lm_beta(exposure, outcome, covariates, df)))
  }

  #ps weighting quals
  if(is.null(covariates)==F){
    ps_df = get_ps(exposure, covariates, df)
    tot_df =  tot_bind(list(tot_df,
                            ps_weight(exposure, outcome, covariates, ps_df, "weights")))

  }

  #matching quals
  if(length(find.package("MatchIt", quiet=TRUE))!=1 & is.null(match_methods)==F){
    warning("You've entered matching methods without the MatchIt package installed. Matching Analyses have been skipped")
  }

  if(is.null(match_methods)==F & length(find.package("MatchIt", quiet=TRUE))==1){
    di_df = df
    if(class(df[,exposure]) == "numeric"){
      warning("Matching methods require a binary exposure. The exposure has been dichotomized so the top 25% quartile are 1s and the bottom 75% are 0s")
      di_df = dichotomize(exposure, df, div)
    }

    match_df_list = lapply(match_methods, function(x) matchit_matching(exposure, covariates, di_df, d = x, ratio))
    if(class(df[,outcome]) == "numeric"){
      match_dfs = tot_bind(lapply(c(1:length(match_df_list)), function(x) re(lm_beta(exposure, outcome, df = match_df_list[[x]]), match_methods[[x]])
      ))
    }
    if(class(df[,outcome]) == "integer"){
      match_dfs = tot_bind(lapply(c(1:length(match_df_list)), function(x) re(odds_ratio(exposure, outcome, df = match_df_list[[x]]), match_methods[[x]])
      ))
    }

    tot_df = tot_bind(list(tot_df,match_dfs))
  }

  tot_df = as.data.frame(apply(tot_df, 2, unlist))
  return(tot_df)
}

