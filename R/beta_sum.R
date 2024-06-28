#'@noRD

beta_sum = function(run, a=0.05){
  require(dplyr)
  #call needed data
  ate = run[[9]][2]
  calc_ate = as.data.frame(run[[2]])
  lower_int = as.data.frame(run[[3]])
  upper_int = as.data.frame(run[[4]])
  p_val = as.data.frame(run[[5]])
  n=length(calc_ate[[1]])

  #parse data
  names= unlist(lapply(colnames(lower_int), function(y) rep(y, nrow(lower_int))))
  upper_int= as.numeric(unlist(upper_int))
  lower_int = as.numeric(unlist(lower_int))
  calc_ate = as.numeric(unlist(calc_ate))
  p_val = as.numeric(unlist(p_val))
  data = dplyr::tibble(names,lower_int, upper_int, calc_ate, p_val)
  data_ci = data %>% dplyr::filter(lower_int <= ate & upper_int >= ate)
  data_under_ci = data %>% dplyr::filter(upper_int < ate)
  data_over_ci = data %>% dplyr::filter(lower_int > ate)
  data_ci$in_ci = 1
  data_under_ci$in_ci = 0
  data_over_ci$in_ci = 0
  data_ci$over_ci = 0
  data_ci$under_ci = 0
  data_under_ci$over_ci = 0
  data_over_ci$over_ci = 1
  data_under_ci$under_ci = 1
  data_over_ci$under_ci = 0

  data_under_a = data %>% dplyr::filter(p_val <= a)

  data = rbind(data_ci, data_over_ci, data_under_ci)
  #max((data_ci %>% dplyr::filter(names == "logistic_or"))[4])

  #create summary functions
  max_ci_beta = sapply(unique(data$names), function(x) max((data_ci %>% dplyr::filter(names == x))[4]))
  min_ci_beta = sapply(unique(data$names), function(x) min((data_ci %>% dplyr::filter(names == x))[4]))
  max_beta = sapply(unique(data$names), function(x) max((data %>% dplyr::filter(names == x))[4]))
  min_beta = sapply(unique(data$names), function(x) min((data %>% dplyr::filter(names == x))[4]))
  mean_beta = sapply(as.list(unique(data$names)), function(x) mean((data %>% dplyr::filter(names == x))[[4]]))
  names(mean_beta) = colnames(run[[2]])
  sd_beta = sapply(as.list(unique(data$names)), function(x) sd((data %>% dplyr::filter(names == x))[[4]]))
  prop_over_ci = sapply(unique(data$names), function(x) length((data_over_ci %>% dplyr::filter(names == x))[[4]])/length(run[[1]][,1]))
  prop_under_ci = sapply(unique(data$names), function(x) length((data_under_ci %>% dplyr::filter(names == x))[[4]])/length(run[[1]][,1]))

  beta_se =  sapply(as.list(unique(data$names)), function(x) sd((data %>% dplyr::filter(names == x))[[4]])/sqrt(n))
  beta_bias = mean_beta-ate

  reject_per = sapply(unique(data$names), function(x) length((data_under_a %>% dplyr::filter(names == x))[[4]])/n)
  coverage = 1-(prop_over_ci+prop_under_ci)

  #MCMC std error of functions
  beta_bias_mcse =sapply(as.list(unique(data$names)), function(x) sqrt(sum(((data %>% dplyr::filter(names == x))[[4]]-mean_beta[[x]])^2) * 1/(n*(n-1))) )
  coverage_mcse = sqrt((coverage*(1-coverage))/n)
  reject_per_mcse = sqrt((reject_per*(1-reject_per))/n)

  #amalgamte into model
  beta_in_ci = as.data.frame(rbind(max_ci_beta, min_ci_beta, max_beta, min_beta, mean_beta, sd_beta, prop_over_ci, prop_under_ci,
                                   beta_se, beta_bias, reject_per, coverage,
                                   beta_bias_mcse, reject_per_mcse, coverage_mcse)
  )
  beta_in_ci[sapply(beta_in_ci, is.infinite)]=NA
  beta_in_ci = beta_in_ci[,c(colnames(run[[1]]))] #reorder columns to original run order for indexing
  return(beta_in_ci)
}
