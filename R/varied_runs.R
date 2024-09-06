#'Creates a list of summary statistics for methods testing and bias quantification
#'
#'Creates a set number of simulated data frames and analyzes them with the methods offered in apply_methods(). Returns a list of summary statistics from each analysis
#'
#' @param runs The number of data frames to be simulated
#'
#' @param dag Hydenet directed acyclic graph object (DAG object) with set distributions for each node.
#'
#' @param exposure Character value. The column name in a data frame that represents exposure in an
#'
#' @param outcome Character value. The column name in a data frame that represents the outcome.
#'
#' @param covariates Character value/vector. The name(s) of the columns that the user wants adjust on. Defaults to NULL meaning no covariates are adjusted on.
#'
#' @param sb Character value/vector. The name(s) of \strong{binary} column(s). Stratifies the data frame to include only the rows with 1s in every listed column.
#'
#' @param n Integer. The number of samples in each data frame
#'
#' @param positivity logical. If set to TRUE, checks for positivity violations among binary columns. If violated, it changes the value in the first row of the column to comply with positivity. Defaults to FALSE.
#'
#' @param misdiagnosis_v Character value. A \strong{binary} column name in a data frame that the user wishes to set misdiagnosis rates on. Defaults to the column set as the outcome.
#'
#' @param under_r double between 0 and 1. The percentage of 1s to be changed to 0s in the column set by misdiagnosis_v
#'
#' @param over_r double between 0 and 1.  The percentage of 0s to be changed to 1s in the column set by misdiagnosis_v
#'
#' @param ratio Integer. number of controls that should be matched to each case. Defaults to 1
#'
#' @param match_methods Character value/vector. The method(s) in which distance between a matched case and control is measured as specified by MatchIt. Defaults to NULL which does no matching analysis.
#'
#' @param ... If the DAG has any unset variables, define them here
#'
#' @return A list of 11 elements with each element bulleted below in order
#' * odds_ratio, NA if not applicable to the method used
#' * calculated_ate, the calculated effect size estimate between the set exposure and outcome
#' * lower_int, the lower bound of the 95% confidence interval for the calculated_ate
#' * upper_int, the upper bound of the 95% confidence interval for the calculated_ate
#' * p_values, the p value for the calculated_ate
#' * exp_prevalence, the exposure prevalence if the exposure variable is binary, returns NAs if not
#' * out_prevalence, the outcome prevalence if the outcome variable is binary, returns NAs if not
#' * sample_population, the number of samples in the dataset. If the analysis requires stratifying or throwing data samples, sample_population will change to reflect that.
#' * set_ate, the effect size between the exposure and outcome that is directly set in the DAG object. Acts as the true effect size.
#' * over_r, the overdiagnosis rate that was set. defaults to 0
#' * under_r, the overdiagnosis rate that was set. defaults to 0
#'
#' each list element will contain x number of columns and y number of rows, where x is the number of different methods tested, and y is the number of simulated data sets created.
#'
#' @example examples/varied_methods_example.R
#'
#'@seealso [apply_methods()],[ci_ridges()],[beta_summary()], [reparse_runs()]
#'@export
#'


varied_runs = function(runs, dag, exposure, outcome, covariates=NULL, sb=NULL, n=10000, positivity = F, misdiagnosis_v = outcome, under_r = 0, over_r = 0, ratio=1, match_methods = NULL, ...){
  randomize = function(variable, rmodel){
    if(is.null(variable) == TRUE){
      variable = rmodel
    } else {
      variable = rep(variable, runs)
    }
  }

  n = randomize(n, as.integer(runif(runs, 1000, 100000)))
  under_r = randomize(under_r, runif(runs, 0, 1))
  over_r = randomize(over_r, runif(runs, 0, 1))

  value_df = data.frame(n = n,
                        under_r = under_r,
                        over_r = over_r)
  #capture the set ate
  temp_dag = make_model(dag, ...)
  ate_set = temp_dag$dag[exposure, outcome]
  b_value = capture.output(temp_dag$jags)
  outcome_eq = grep(paste0("   ", outcome), b_value, value = T)
  outcome_eq = gsub(" ", "", outcome_eq, fixed = T)
  outcome_eq = unlist(strsplit(outcome_eq, ")", fixed = T))
  outcome_eq = unlist(strsplit(outcome_eq, "(", fixed = T))
  outcome_eq = unlist(strsplit(outcome_eq, "+", fixed = T))
  outcome_val = grep(exposure, outcome_eq, value = T)
  outcome_val = gsub(exposure, 1, outcome_val)
  outcome_val = gsub("[[:alpha:]]", "0", outcome_val)
  ate = sum(unlist(lapply(outcome_val, function(x) eval(parse(text = x)))))


  #temp_dag = lapply(c(1:runs), function(x) make_model(dag, value_df[x,1], value_df[x,3], value_df[x,4]))

  #FIX DIMENSION PROBLEM
  temp_df = lapply(c(1:runs), function(x) create_data(dag, value_df[x,1], positivity = positivity, ...))
  temp_df = lapply(c(1:runs), function(x) misdiagnosis(temp_df[[x]], misdiagnosis_v, under_r[x], over_r[x]))
  temp_output = lapply(temp_df, apply_methods, exposure = exposure, outcome = outcome, covariates = covariates, sb = sb, ratio=ratio, match_methods=match_methods)

  one_dim = FALSE
  if(names(temp_output[[1]])[1]=="apply(tot_df, 2, unlist)"){
    temp_output = lapply(1:runs,function(x) t(temp_output[[x]]))
    one_dim = TRUE
  }

  if(class(temp_df[[1]][,outcome])=="integer"){
    out_p = unlist(lapply(c(1:runs), function(x) sum(temp_df[[x]][,outcome])/nrow(temp_df[[x]])))

  }else{
    out_p=rep(NA, runs)

  }
  if(class(temp_df[[1]][,exposure])=="integer"){
    exp_p = unlist(lapply(c(1:runs), function(x) sum(temp_df[[x]][,exposure])/nrow(temp_df[[x]])))
    #mde = unlist(lapply(c(1:runs), function(x) sum(temp_df[[x]][,outcome])/nrow(temp_df[[x]])))
    #unlist(lapply(colnames(x_val), function(x) 0.02*sqrt(1 / (run[[3]][,x]*(1 - run[[3]][,x])*run[[5]][,x] ) )))
  }else{
    exp_p=rep(NA, runs)
    #mde=rep(NA, runs)
  }
  temp_output =lapply(c(1:runs), function(x) cbind(temp_output[[x]], exp_prev = rep(exp_p[x], nrow(temp_output[[x]]))))
  temp_output =lapply(c(1:runs), function(x) cbind(temp_output[[x]], out_prev = rep(out_p[x], nrow(temp_output[[x]]))))
  ##FIX SET ATE HERE
  temp_output =lapply(c(1:runs), function(x) cbind(temp_output[[x]], set_ate = rep(ate, nrow(temp_output[[x]]))))
  temp_output =lapply(c(1:runs), function(x) cbind(temp_output[[x]], over_r = rep(over_r[x], nrow(temp_output[[x]]))))
  temp_output =lapply(c(1:runs), function(x) cbind(temp_output[[x]], under_r = rep(under_r[x], nrow(temp_output[[x]]))))
  partition = laply(temp_output, as.matrix)
  if(one_dim == TRUE){
    partition = list(odds_ratio = partition[, 1],
                     calculated_ate = partition[, 2],
                     lower_int = partition[, 3],
                     upper_int = partition[, 4],
                     p_values = partition[, 6],
                     exp_prevalence = partition[, 8],
                     out_prevalence = partition[, 9],
                     sample_population = partition[, 7],
                     set_ate = partition[, 10],
                     over_r = partition[,11],
                     under_r = partition[,12])
    # partition = as.matrix(partition)
    # class(partition) = c("ScenarioMatrix", "matrix")
    return(partition)
  }
  run_list = list(ratio = partition[,, 1],
                  calculated_ate = partition[,, 2],
                  lower_int = partition[,, 3],
                  upper_int = partition[,, 4],
                  p_values = partition[,, 6],
                  exp_prevalence = partition[,, 8],
                  out_prevalence = partition[,, 9],
                  sample_population = partition[,, 7],
                  set_ate = partition[,, 10],
                  over_r = partition[,,11],
                  under_r = partition[,,12])
  # class(run_list) = "ScenarioMatrix"


  return(run_list)
}


