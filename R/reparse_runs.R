#'Combines outputs from varied_runs() together to create a new output that contains different analyses to compare.
#'
#'The final output will be a list of 11 elements with each element corresponding to a summary statistic, but the columns in each element will now reflect the different circumstances of each scenario set from varied_runs().
#'
#' @param run_list Several different outputs from varied_runs() in a list format that the user wishes to recombine. The outputs from varied_runs() in the list must all have the same number of runs (the same number of datasets must be simulated).
#'
#' @param method Character value. Defaults to Null, meaning every method in each varied_runs() output will be included.
#'
#' @param list_names Character vector. Must be equal to the number of elements in run_list. Defaults to numeric naming with the first element of run_list being named "1", the second "2" and so on.
#'
#' @return A list of 11 elements with each element bulleted below in order
#'
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
#'@example examples/varied_methods_example.R
#'@seealso [apply_methods()],[ci_ridges()],[beta_summary()], [varied_runs()]
#'@export
#'


reparse_runs = function(run_list, method = NULL, list_names=as.character(1:length(run_list))){
  extract_method = function(run,method){
    m_list = lapply(c(1:length(run)),function(x) run[[x]][,colnames(run[[1]]) == method])
    names(m_list) = names(run)
    m_list = as.data.frame(m_list)
    return(m_list)
  }
  if(length(run_list)==1){
    stop("must compare at least 2 different SenarioMatrices in order to reparse")
    # rep_size = nrow(as.data.frame(run_list))
    # dim_holder = data.frame("ratio" = rep(NA, rep_size),
    #                         "calculated_ate" = rep(NA, rep_size),
    #                         "lower_int" = rep(NA, rep_size),
    #                         "upper_int" = rep(NA, rep_size),
    #                         "p_values" = rep(NA, rep_size),
    #                         "exp_prevalence" = rep(NA,rep_size),
    #                         "out_prevalence" = rep(NA,rep_size),
    #                         "sample_population" = rep(NA,rep_size),
    #                         "set_ate" = rep(NA,rep_size),
    #                         "over_r"= rep(NA,rep_size),
    #                         "under_r" = rep(NA,rep_size))
    # run_list = list(dim_holder, run_list)
  }
  names(run_list) = list_names
  index = unlist(lapply(1:length(run_list), function(x) class(run_list[[x]][[1]])[1]))
  names(index)=list_names

  list_list = run_list[which(index != "numeric")]
  mat_list = run_list[which(index == "numeric")]
  mat_names = names(mat_list)

  if(length(mat_list) !=0){
    mat_list = lapply(c(1:length(mat_list)), function(x) as.data.frame(mat_list[[x]]))
    names(mat_list) = mat_names
  }

  if(is.null(method) == F && length(list_list)>0){
    extraction = lapply(list_list, function(x) extract_method(x,method))
    list_names = names(extraction)
  }else if(is.null(method) == T && length(list_list)>0){
    method_names = lapply(1:length(list_list), function(x) dimnames(list_list[[x]][[1]])[[2]])
    names(method_names)=names(list_list)
    extraction = lapply(names(list_list), function(x)
      lapply( method_names[[x]], function(y) extract_method(list_list[[x]], y) )
      )
    names(extraction) = names(list_list)
    header_names = unlist(lapply(1:length(method_names), function(x) rep(names(method_names)[[x]], length(method_names[[x]]))))
    method_names_2 = unname(unlist(method_names))
    expand_names = unlist(lapply(1:length(method_names_2), function(x) paste0(header_names[[x]], "_", method_names_2[[x]])))
    extraction = unlist(extraction, recursive = F)
    names(extraction) = expand_names

  }else{
    extraction = list_list
  }



  tot_list = append(extraction, mat_list)

  tot_mat = laply(tot_list, as.matrix)

  partition = aperm(tot_mat, c(2,1,3))
  colnames(partition) = c(names(extraction),mat_names)
  out_df = list(ratio = partition[,, 1],
                calculated_ate = partition[,, 2],
                lower_int = partition[,, 3],
                upper_int = partition[,, 4],
                p_values = partition[,, 5],
                exp_prevalence = partition[,, 6],
                out_prevalence = partition[,, 7],
                sample_population = partition[,, 8],
                set_ate = partition[,, 9],
                over_r = partition[,,10],
                under_r = partition[,,11])
  return(out_df)
}

