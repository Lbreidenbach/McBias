#'creates a matrix of summary statistics for methods testing and bias quantification
#'
#'creates a number of data frames (set by runs) and analyzes them with the methods offered in apply_methods().
#'
#' @param runs The number of data frames to be simulated
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
#' @return A six column data frame of n rows where n is the number of different methods run. The columns represent the following summary statistics:
#' * odds ratio
#' * beta (log odds)
#' * lower 95% confidence interval of beta coefficient
#' * upper 95% confidence interval of beta coefficient
#' * p value
#' * number of samples in the dataset
#'
#'@examples McBias/examples/apply_methods_to_matrix.R
#'@seealso [apply_methods()],[ci_ridges()],[beta_summary()]
#'@export
#'


varied_runs = function(runs, dag, exposure, outcome, covariates=NULL, sb=NULL, n=10000, misdiagnosis_v = outcome, under_r = 0, over_r = 0, ratio=1, match_methods = NULL, ...){
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
  if(ate_set == 0){
    ate = 0
  }else{
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
    #outcome_val = eval(parse(text = ))


    #FIX FINDING ATE
  }


  #temp_dag = lapply(c(1:runs), function(x) make_model(dag, value_df[x,1], value_df[x,3], value_df[x,4]))
  temp_df = lapply(c(1:runs), function(x) create_data(dag, as.numeric(value_df[x,1]), ...))
  temp_df = lapply(c(1:runs), function(x) misdiagnosis(temp_df[[x]], misdiagnosis_v, under_r[x], over_r[x]))
  temp_output = lapply(temp_df, apply_methods, exposure = exposure, outcome = outcome, covariates = covariates, sb = sb, ratio=ratio, match_methods=match_methods)

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
  if(is.null(covariates)==T & class(temp_df[[1]][,outcome])=="numeric"){
    # partition = list(data.frame(odds_ratio = NA),
    #                  data.frame(calculated_ate = partition[, 2]),
    #                  data.frame(lower_int = partition[, 3]),
    #                  data.frame(upper_int = partition[, 4]),
    #                  data.frame(p_values = partition[, 6]),
    #                  data.frame(exp_prevalence = partition[, 8]),
    #                  data.frame(out_prevalence = partition[, 9]),
    #                  data.frame(sample_population = partition[, 7]),
    #                  data.frame(set_ate = partition[, 10]),
    #                  data.frame(over_r = partition[,11]),
    #                  data.frame(under_r = partition[,12]))
    # partition = as.matrix(partition)
    return(partition[,-5])
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
  class(run_list) = "ScenarioMatrix"


  return(run_list)
}


