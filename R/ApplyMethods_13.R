library(HydeNet)
library(rjags)
library(MatchIt)
library(ggplot2)
library(plyr)
#library(EmpiricalBrownsMethod)
library(dplyr)
library(gtable)
library(grid)
library(GGally)
library(ggridges)
library("reshape2")
library(gridExtra)
library(logisticRR)

#Mechanical functions######
#x is numeric
check.integer = function(x){
  check = all.equal(x, as.integer(x))
  return(isTRUE(check))
}

#n is integer, columns is string vector, dag is jag dag, reclassification makes integers integers
#example create_data(make_model(iges_demo, 0, .1, .1), 10000)
create_data = function(jag_dag, n, reclassify = as.integer){
  sim_df = bindSim(HydeSim(jag_dag, variable.names = colnames(jag_dag$dag), n.iter = n, bind = FALSE))
  relabel = lapply(sim_df, check.integer) # JAGS labels integers as numeric, have to reclassify them
  relabel = relabel[relabel != FALSE]
  relabel = names(relabel)
  sim_df[relabel] = lapply(sim_df[relabel], reclassify)
  sim_df = sim_df[c(-length(sim_df), -(length(sim_df)-1))]
  return(sim_df)
}

set_p = function(p,model){
  p2 = log(p/(1-p))
  b0 = p2-model
  return(b0)
}

misdiagnosis = function(df, variable, under_rate=0, over_rate=0){
  index_1 = which(df[variable] == 1)
  index_0 = which(df[variable] == 0)

  over = round(length(index_0)*over_rate)
  under = round(length(index_1)*under_rate)

  if(under != 0){
    df[index_1, variable][1:under] = 0
  }
  if(over != 0){
    df[index_0, variable][1:over] = 1
  }

  return(df)
}

tot_bind <- function(datalist) {
  require(plyr)
  temp = rbind.fill(datalist)
  rownames(temp) <- unlist(lapply(datalist, row.names))
  return(temp)
}

#####
#data parsing########
#column is character
bi_strat = function(x, df, column){
  index_1 = which(df[column] == x)
  index_0 = which(df[column] != x)
  df[index_0, column] = 0
  df[index_1, column] = 1
  return(list(df[index_1,], df[index_0,]))
}

find_nulls = function(run){
  pval_df = as.data.frame(run[[2]])
  beta_df= as.data.frame(run[[1]])
  null_index = sapply(pval_df, function(x) which(x >0.05))
  null_betas = lapply(c(1:ncol(run[[1]])), function(x) beta_df[null_index[[x]], x])
  n = unlist(lapply(null_betas, length))
  max_b = unlist(lapply(null_betas, max))
  min_b = unlist(lapply(null_betas, min))
  df = data.frame(colnames(run[[1]]), n/nrow(run[[2]]), max_b, min_b)
  colnames(df) = c("method", "null_percent", "max_null beta", "min_null_beta")
  return(df)
}

#column is character
dichotomize = function(column, df, div){
  x = as.numeric(quantile(df[,column])[div])
  index_0 = which(df[column] < x)
  index_1 = which(df[column] >= x)
  df[index_0, column] = 0
  df[index_1, column] = 1
  df[,column] = as.integer(df[,column])
  df = rbind(df[index_1,], df[index_0,])
  rownames(df) = NULL
  return(df)
}

#column is vector
col_dichotomize = function(column, div =4){
  x = as.numeric(quantile(column)[div])
  index_0 = which(column < x)
  index_1 = which(column >= x)
  column[index_0] = 0
  column[index_1] = 1
  column = as.integer(column)
  return(column)
}

#base strat necessary???
base_strat = function(exposure, outcome, covariate, df, div = 3){
  if(class(df[,exposure]) == "numeric"){
    strats = dichotomize(exposure, df, div)
    new_df = rbind(strats[[1]], strats[[2]])
    out_df = odds_ratio(exposure, outcome, covariate, df = new_df)
  } else{
    out_df = odds_ratio(exposure, outcome, covariate, df = df)
  }
  # if(class(df$outcome) == "integer"){
  #   out_df = odds_ratio(exposure, outcome, covariate, df = df)
  # } else if(class(df$outcome) == "numeric"){
  #   strats_2 = bi_strat(div, df, exposure)
  #   treat = as.data.frame(strats_2[1])
  #   untreat = as.data.frame(strats_2[2])
  #   temp_test = t.test(treat[[paste(exposure)]], untreat[[paste(exposure)]])
  #   out_df = data.frame(odds_ratio = NA,
  #                       beta = abs(temp_test$estimate[1] - temp_test$estimate[2]),
  #                       lower_int = temp_test$conf.int[1],
  #                       upper_int = temp_test$conf.int[2],
  #                       confint_diff = abs(temp_test$conf.int[2]-temp_test$conf.int[1]),
  #                       p_val = temp_test$p.value)
  # } else {
  #   print("outcome must be integer or numeric")
  # }
  return(out_df)
}
#bi exposure
matchit_matching = function(exposure, covariates, df, d = "logit", ratio = 1){
  psm = matchit(as.formula(paste(exposure, paste(covariates, collapse=" + "), sep=" ~ ")), data = df, method = "nearest", distance = d, ratio = ratio)
  treated_index = rownames(psm$match.matrix)
  untreated_index = c(psm$match.matrix[1:length(psm$match.matrix)])
  treated_subset = df[treated_index, ]
  untreated_subset = df[untreated_index, ]
  return(rbind(treated_subset, untreated_subset))
}

get_ps = function(exposure, covariates, df){
  if(class(df[,exposure]) == "numeric" ){
    ps_mod = lm(as.formula(paste(exposure, paste(covariates, collapse=" + "), sep=" ~ ")), data = df)
    ps = as.numeric(plogis(fitted.values(ps_mod)))
    num_mod = lm(as.formula(paste(exposure, 1, sep=" ~ ")), data = df)
    num = as.numeric(plogis(fitted.values(num_mod)))
  }else if(class(df[,exposure]) == "integer"){
    ps_mod <- glm(as.formula(paste(exposure, paste(covariates, collapse=" + "), sep=" ~ ")), data = df, family="binomial")
    ps = fitted(ps_mod)
    num_mod = glm(as.formula(paste(exposure, 1, sep=" ~ ")), data = df, family = "binomial")
    num = fitted(num_mod)
  }else{
    print("exposure must be numeric or integer class")
  }

  df_out = data.frame(ps = ps,
                      weights = num/ps)
  df$ps = df_out$ps
  df$weights = df_out$weights
  return(df)
}
#####

#Read outs#####
#bi outcome
odds_ratio = function(exposure, outcome, covariates=NULL, df){
  vars = c(exposure, covariates)
  cont_glm = glm(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df, family = "binomial")
  exp_coef = as.numeric(cont_glm$coefficients[2])
  exp_or = exp(exp_coef)
  or_confint = confint.default(cont_glm, trace = F)
  or_upper = or_confint[2,2]
  or_lower = or_confint[2,1]
  int_diff = or_upper - or_lower

  or_df = data.frame("odds_ratio" = exp_or,
                     beta = exp_coef,
                     lower_int = or_lower,
                     upper_int = or_upper,
                     confint_diff = abs(int_diff),
                     p_val = coef(summary(cont_glm))[2,4],
                     n = nrow(df))
  row.names(or_df) = "logistic_or"
  return(or_df)
}
risk_ratio = function(exposure, outcome, covariates=NULL, df){
  vars = c(exposure, covariates)
  cont_glm = logisticRR(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df)
  exp_coef = cont_glm$fit$coefficients[[2]]
  exp_rr = cont_glm$RR
  or_confint = confint.default(cont_glm$fit, trace = F)
  or_upper = or_confint[2,2]
  or_lower = or_confint[2,1]
  int_diff = or_upper - or_lower

  or_df = data.frame("odds_ratio" = exp_rr,
                     beta = exp_coef,
                     lower_int = or_lower,
                     upper_int = or_upper,
                     confint_diff = abs(int_diff),
                     p_val = coef(summary(cont_glm$fit))[2,4],
                     n = nrow(df))
  row.names(or_df) = "rr_regression"
  return(or_df)
}

#bi outcome, bi exposure
beta_chi_sq = function(exposure, outcome, df){
  if ((class(df[,exposure]) == "integer") & (class(df[,outcome]) == "integer")){
    r_tab = table(df[,exposure], df[,outcome])
    a = r_tab[2,2]
    b = r_tab[2,1]
    c = r_tab[1,2]
    d = r_tab[1,1]
    or_2 = (a/c)/(b/d)
    beta = log(or_2)
    or_upper_2 = log(or_2) + 1.96 * sqrt(1/a + 1/b + 1/c + 1/d)
    or_lower_2 = log(or_2) - 1.96 * sqrt(1/a + 1/b + 1/c + 1/d) #confint for coef
    int_diff = or_upper_2-or_lower_2
    # se = int_diff/(2*1.96)
    # z = abs(beta/se)
    # p = exp(-0.717*z - 0.416*z^2)
    chi_test = chisq.test(r_tab)
    or_df_2 = data.frame("odds_ratio" = or_2,
                         beta = beta,
                         lower_int = or_lower_2,
                         upper_int = or_upper_2,
                         confint_diff = abs(int_diff),
                         p_val = chi_test$p.value,
                         n = nrow(df))
    rownames(or_df_2) = "chi_sq"
    return(or_df_2)
  }else{
    warning("chi squared only accepts integer values")
  }
}

#cont outcome, bi exposure
beta_t_test = function(exposure, outcome, df){
  if((class(df[,outcome]) == "numeric")&(class(df[,exposure]) == "integer")){
    strat = bi_strat(1, df, exposure)
    temp_test = t.test(strat[[1]][,outcome], strat[[2]][,outcome])
    t_test_df = data.frame("odds_ratio" = NA,
                           beta = abs(temp_test$estimate[1] - temp_test$estimate[2]),
                           lower_int = temp_test$conf.int[1],
                           upper_int = temp_test$conf.int[2],
                           confint_diff = abs(temp_test$conf.int[2]-temp_test$conf.int[1]),
                           p_val = temp_test$p.value,
                           n = nrow(df))
    rownames(t_test_df)="t_test"
    return(t_test_df)
  }else{
    warning("outcome must be continuous")
  }

}

#cont outcome
lm_beta = function(exposure, outcome, covariates=NULL, df){
  vars = c(exposure, covariates)
  lm1 = lm(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df)
  confint = confint(lm1, trace = F)
  upper_int = confint[2,2]
  lower_int = confint[2,1]
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



#fix so it accepts only catagorical quantiles
ps_strat = function(exposure, outcome, covariates, df, x=5, return_strata=F){
  temp_df = get_ps(exposure, covariates, df)
  if(apply(temp_df[covariates], 2, is.integer)==TRUE){
    quintiles = as.double(names(table(temp_df$ps)))-0.000000001
    temp_df$strat_cat = as.integer(cut(temp_df$ps, breaks = c(quintiles,1), labels = 1:length(quintiles-1), include.lowest = TRUE))
  }else{
    quintiles = unique(quantile(temp_df$ps, prob=seq(0,1,1/(x))))-0.0000001
    #temp_df$strat_cat = as.integer(cut(temp_df$ps, breaks = c(0, quintiles), labels = 1:(length(quintiles)), include.lowest = FALSE))
    temp_df$strat_cat = as.integer(cut(temp_df$ps, breaks = c(-Inf, quintiles[-1]), labels = 1:(length(quintiles)-1), include.lowest = FALSE))
  }

  method = function(m){
    strat_df = aaply(1:length(quintiles), 1, function(y) m(exposure, outcome, df = temp_df[temp_df$strat_cat == y, ]))
    strat_df = as.data.frame(strat_df)
    strat_df = as.data.frame(apply(strat_df, 2, unlist))
    rownames(strat_df) = paste0("strata_", as.character(1:length(quintiles)))
    pop = as.numeric(aaply(1:length(quintiles), 1, function(y) nrow(temp_df[temp_df$strat_cat == y, ])))
    strat_df$pop = pop
    strat_df = as.data.frame(strat_df)
    strat_df$weights = strat_df$pop/ sum(strat_df$pop)
    beta_df = strat_df
    beta_df = lapply(beta_df, as.numeric)
    beta_df = lapply(beta_df[1:6], function(y) y*(beta_df$pop/sum(beta_df$pop)))
    beta_df = as.data.frame(beta_df)
    beta_df = lapply(beta_df, sum)
    beta_df = as.data.frame(beta_df)
    rownames(beta_df) = "averaged_strata"
    # if(return_strata==T){
    #   tot_df = rbind(strat_df, beta_df)
    # }else{
    #   tot_df = beta_df
    # }

    return(list(strat_df, beta_df))
  }
  strat_means = aaply(1:length(quintiles), 1, function(y) colMeans(temp_df[temp_df$strat_cat == y, ]))
  strat_means = as.data.frame(strat_means)
  #strat_means = strat_means[,c(exposure, outcome, covariates)]
  output = method(odds_ratio)
  output[[1]]$strata = 1:length(quintiles)
  applied_df = output[[2]]

  #temp_sub = temp_df[temp_df$strat_cat == x, ]
  #wrong, shoud be for outcome


  return(applied_df)
}
ps_weight = function(exposure, outcome, covariates, df, weights){
  vars = c(exposure, covariates)
  if(class(df[,outcome]) == "numeric" ){
    cont_glm = lm(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df, weights = weights)
  }else if(class(df[,outcome]) == "integer"){
    cont_glm = glm(as.formula(paste(outcome, paste(vars, collapse=" + "), sep=" ~ ")), data = df, weights = weights, family = "quasibinomial")
  }else{
    warning("exposure must be numeric or integer")
  }

  exp_coef = as.numeric(cont_glm$coefficients[2])
  exp_or = exp(exp_coef)
  or_confint = confint.default(cont_glm, trace = F)
  or_upper = or_confint[2,2]
  or_lower = or_confint[2,1]
  int_diff = or_upper - or_lower

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

#ipw for stratified varaibles, inv_covariates accept lists of different formulas
#for example sb = in_biobank, inv_covariates = list(c("depression", "sex", "under_40", "diabetes"), c("sex", "under_40"))
#that would give in_biobank ~ depression + sex + under_40 + diabetes and in_biobank ~ sex + under_40
ipw = function(exposure, outcome, covariates=NULL, sb, inv_covariates, df){
  inv_df = lapply(c(1:length(inv_covariates)), function(x) get_ps(sb, inv_covariates[[x]], df))
  ##make a logestic regression instead
  inv_cov_df = lapply(c(1:length(inv_covariates)), function(x) ps_weight(exposure, outcome, covariates, inv_df[[x]], weights = weights))
  # tot_df = rbind.fill(inv_cov_df)

  #inv_sum = lapply(c(1:length(inv_covariates)), function(x) ps_weight(exposure, outcome, covariates, inv_cov_df[[x]], "weights"))
  tot_df = rbind.fill(inv_cov_df)
  inv_row = lapply(c(1:length(inv_covariates)), function(x) paste("ipw:", sb, "~", paste(inv_covariates[[x]],collapse = " + ")))
  rownames(tot_df) = unlist(inv_row)
  return(tot_df)
}

#####
#bi exp & bi outcome: Read out:logisitc regression/chi squared. Cov control: all
#bi exp & cont outcome: read out linear regression/t.test, Cov control:all
#cont. exp & bi outcome: read out: logistic regression, Cov control: no matching
#cont. exp & cont. outcome: Readout: linear regression, Cov control: no matching

##Applying methods#######
#x is no. of ps strata, div is quartile to dichotomize exposure, ratio is no. matches to controls, sb is string of stratified int column
#fix so sb can take multiple versions, use apply
apply_methods = function(exposure, outcome, covariates=NULL, sb=NULL, inv_covariates=NULL, df, x=5, div=4, ratio=1, matching=F){

  #create empty data frame
  tot_df = data.frame("odds_ratio" = as.numeric(),
                      beta = as.numeric(),
                      lower_int = as.numeric(),
                      upper_int = as.numeric(),
                      confint_diff = as.numeric(),
                      p_val = as.numeric(),
                      n = as.numeric())

  #do any IPW here
  #
  #
  #

  ##Data parsing, maybe update to apply for mter than one Selection Bias source
  if(is.null(sb)==F){
    # if(is.null(inv_covariates)==T){
    #   sb_df = bi_strat(1, df, sb)[[1]]
    # } else {
    #   tot_df = tot_bind(list(tot_df, ipw(exposure, outcome, covariates=NULL, sb, inv_covariates, df)))
    #   #lapply(c(1:length(sb)), function(x) ps_weight(exposure, outcome, covariates, inv_df[[x]], "weights"))
    #   sb_df = bi_strat(1, df, sb)[[1]]
    #   df = sb_df
    # }
    sb_df = bi_strat(1, df, sb)[[1]]
    df = sb_df
  }


  if(is.null(covariates)==T  & class(df[,outcome])=="integer"){
    tot_df = tot_bind(list(tot_df, odds_ratio(exposure, outcome, df = df), risk_ratio(exposure, outcome, df = df)))
    return(tot_df)

  }
  if(is.null(covariates)==T  & class(df[,outcome])=="numeric"){
    tot_df = tot_bind(list(tot_df, lm_beta(exposure, outcome, covariates, df)))
    return(tot_df)

  }
  re = function(df, name){
    rownames(df) = name
    return(df)
  }
  ps_df = get_ps(exposure, covariates, df)
  #dichotomizes exposure if needed
  if(class(df[,exposure]) == "numeric" ){
    di_df = dichotomize(exposure, df, div)
  } else {
    di_df = df
  }

  if(matching == F & class(df[,outcome])=="integer"){
    tot_df = tot_bind(list(tot_df, odds_ratio(exposure, outcome, covariates, df = df),
                           risk_ratio(exposure, outcome, covariates, df = df),
                           #ps_strat(exposure, outcome, covariates, df, x=5),
                           ps_weight(exposure, outcome, covariates, ps_df, "weights")
                           ))
    return(tot_df)

  }
  if(matching == F & class(df[,outcome])=="numeric"){
    tot_df = tot_bind(list(tot_df,
                           lm_beta(exposure, outcome, covariates, df),
                           ps_weight(exposure, outcome, covariates, ps_df, "weights")
    ))
    return(tot_df)

  }

  #matching methods
  psm_match_df = matchit_matching(exposure, covariates, di_df, d = "logit", ratio)
  mdm_match_df = matchit_matching(exposure, covariates, di_df, d = "mahalanobis", ratio)


  #all-accepting methods
  tot_df =  tot_bind(list(tot_df,
                          #ps_strat(exposure, outcome, covariates, df, x=5),
                          ps_weight(exposure, outcome, covariates, ps_df, "weights")))
  #tot_df =  tot_bind(list(tot_df, ps_weight(exposure, outcome, covariates, ps_df, "weights")))
  #
  #combining data parses w/ read outs

  if(class(df[,outcome])=="numeric"){
    tot_df = tot_bind(list(tot_df,
                           lm_beta(exposure, outcome, covariates, df),
                           re(lm_beta(exposure, outcome, df= psm_match_df), "ps_matching_regression"),
                           re(lm_beta(exposure, outcome, df =mdm_match_df), "mdm_matching_regression")
                           ))
  }else if(class(df[,outcome])=="integer"){
    tot_df = tot_bind(list(tot_df,
                           odds_ratio(exposure, outcome, covariates, df),
                           risk_ratio(exposure, outcome, covariates, df),
                           re(odds_ratio(exposure, outcome, df = psm_match_df), "ps_matching_regression"),
                           re(odds_ratio(exposure, outcome, df = mdm_match_df), "mdm_matching_regression")
                           ))
  }
  tot_df = as.data.frame(apply(tot_df, 2, unlist))
  return(tot_df)
}

#this function is out-of-date and redundant
multiple_runs = function(n, jag_dag, runs, exposure, outcome, covariates=NULL, x=5, div = 4, ratio=1){
  temp_df = replicate(runs, create_data(jag_dag, n), simplify = FALSE)
  temp_output = lapply(1:runs, function(y) apply_methods(exposure, outcome, covariates, temp_df[[y]], x, div, ratio))
  if(class(temp_df[[1]][,outcome])=="integer"){
    out_p = unlist(lapply(temp_df, function(x) sum(x%>% select(outcome))/n))
  }else{
    out_p=rep(NA, runs)
  }
  if(class(temp_df[[1]][,exposure])=="integer"){
    exp_p = unlist(lapply(temp_df, function(x) sum(x%>% select(exposure))/n))
  }else{
    exp_p=rep(NA, runs)
  }
  temp_output =lapply(c(1:10), function(x) cbind(temp_output[[x]], exp_prev = rep(exp_p[x], nrow(temp_output[[x]]))))
  temp_output =lapply(c(1:10), function(x) cbind(temp_output[[x]], out_prev = rep(out_p[x], nrow(temp_output[[x]]))))
  partition = laply(temp_output, as.matrix)
  if(is.null(covariates)==T){
    return(partition)
  }
  temp_average = aaply(partition, c(2, 3), function(x) mean(na.omit(x)))
  temp_average = aaply(partition, c(2, 3), function(x) sd(na.omit(x)))

  all_ate_values = partition[,, 2]
  all_p_val = partition[,, 6]
  exp_prev = partition[,, 7]
  out_prev = partition[,, 8]
  return(list(temp_average, all_ate_values, all_p_val, exp_prev, out_prev))
}

make_model = function(dag, ate, exp_p, out_p){
  dag_1 = dag(ate, exp_p, out_p)
  writeNetworkModel(dag_1, pretty = TRUE)
  comp_dag = compileJagsModel(dag_1)
  return(comp_dag)
}

#Need a way to incorporate a DAG into this
#fix so it passes if 1 run violates positivity
varied_runs = function(runs, dag, exposure, outcome, covariates=NULL, sb=NULL, inv_covariates = NULL, ate=NULL, n=NULL, exp_p=NULL, out_p=NULL, under_r = 0, over_r = 0, x=5, div = 4, ratio=1, matching=F){
  randomize = function(variable, rmodel){
    if(is.null(variable) == TRUE){
      variable = rmodel
    } else {
      variable = rep(variable, runs)
    }
  }

  ate = randomize(ate, runif(runs, -1, 1))
  n = randomize(n, as.integer(runif(runs, 10000, 100000)))
  exp_p = randomize(exp_p, runif(runs, 0.02, 0.3))
  out_p = randomize(out_p, runif(runs, 0.01, 0.4))
  under_r = randomize(under_r, runif(runs, 0, 1))
  over_r = randomize(over_r, runif(runs, 0, 1))

  value_df = data.frame(ate = ate,
                        n = n,
                        exp_p = exp_p,
                        out_p = out_p,
                        under_r = under_r,
                        over_r = over_r)

  temp_dag = lapply(c(1:runs), function(x) make_model(dag, value_df[x,1], value_df[x,3], value_df[x,4]))
  temp_df = lapply(c(1:runs), function(x) create_data(temp_dag[[x]], as.numeric(value_df[x,2])))
  temp_df = lapply(c(1:runs), function(x) misdiagnosis(temp_df[[x]], outcome, under_r[x], over_r[x]))
  temp_output = lapply(temp_df, apply_methods, exposure = exposure, outcome = outcome, covariates = covariates, sb = sb, inv_covariates = inv_covariates, x=x, div=div, ratio=ratio, matching=matching)

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
  #temp_output =lapply(c(1:runs), function(x) cbind(temp_output[[x]], population = rep(n[x], nrow(temp_output[[x]]))))
  temp_output =lapply(c(1:runs), function(x) cbind(temp_output[[x]], set_ate = rep(ate[x], nrow(temp_output[[x]]))))
  temp_output =lapply(c(1:runs), function(x) cbind(temp_output[[x]], over_r = rep(over_r[x], nrow(temp_output[[x]]))))
  temp_output =lapply(c(1:runs), function(x) cbind(temp_output[[x]], under_r = rep(under_r[x], nrow(temp_output[[x]]))))
  partition = laply(temp_output, as.matrix)
  if(is.null(covariates)==T & is.null(inv_covariates)==T & class(temp_df[[1]][,outcome])=="numeric"){
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

  return(run_list)
}

#####

reparse_runs = function(run_list, method, list_names=as.character(1:length(run_list))){
  extract_method = function(run){
    m_list = lapply(c(1:length(run)),function(x) run[[x]][,colnames(run[[1]]) == method])
    names(m_list) = names(run)
    m_list = as.data.frame(m_list)
    return(m_list)
  }
  if(length(run_list)==1){
    rep_size = nrow(as.data.frame(run_list))
    dim_holder = data.frame("ratio" = rep(NA, rep_size),
                            "calculated_ate" = rep(NA, rep_size),
                            "lower_int" = rep(NA, rep_size),
                            "upper_int" = rep(NA, rep_size),
                            "p_values" = rep(NA, rep_size),
                            "exp_prevalence" = rep(NA,rep_size),
                            "out_prevalence" = rep(NA,rep_size),
                            "sample_population" = rep(NA,rep_size),
                            "set_ate" = rep(NA,rep_size),
                            "over_r"= rep(NA,rep_size),
                            "under_r" = rep(NA,rep_size))
    run_list = list(dim_holder, run_list)
  }
  names(run_list) = list_names
  index = sapply(run_list, is.list)


  list_list = run_list[which(index == TRUE)]
  mat_list = run_list[which(index == FALSE)]
  mat_names = names(mat_list)

  if(length(mat_list) !=0){
    mat_names = names(mat_list)
    mat_list = lapply(c(1:length(mat_list)), function(x) as.data.frame(mat_list[[x]]))
  }


  extraction = lapply(list_list, extract_method)
  list_names = names(extraction)

  tot_list = append(extraction, mat_list)

  tot_mat = laply(tot_list, as.matrix)

  partition = aperm(tot_mat, c(2,1,3))
  colnames(partition) = c(list_names,mat_names)
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



##making pictures#######
#constant runs
make_boxplot = function (run, title=NULL, subtitle=NULL){
  comp_df = as.data.frame(run[[1]])
  ate_val = as.data.frame(run[[1]])

  names= unlist(lapply(colnames(ate_val), function(y) rep(y, nrow(ate_val))))
  value= as.numeric(unlist(ate_val))
  data=tibble(names,value)

  p <- ggplot(data, aes(x=names, y=value, fill=names)) +
    geom_boxplot(alpha=0.7, outlier.shape = NA) +
    geom_segment( aes(x=0, xend=length(colnames(ate_val))+1, y=run[[8]][1,1], yend=run[[8]][1,1]), color="navy", linetype = "dashed", lwd = 1) +
    #scale_y_continuous(limits = quantile(data$value, c(0.1, 0.9))) +
    coord_flip()+
    stat_summary(fun=mean, geom="point", shape=20, size=6, color="navy", fill="navy") +
    theme(legend.position="none") +
    scale_fill_brewer(palette="Set3") +
    xlab("Method type") +
    ylab("Beta") +
    ggtitle(label = paste(title),
            subtitle = paste(subtitle)) +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.text = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14, face = "bold", hjust = 0.5))
  return(p)
}
make_p_v_ate = function(run, title){
  ate_val = as.data.frame(run[[1]])
  p_val = as.data.frame(run[[2]])
  ate_long <- melt(ate_val)
  p_val_long = melt(p_val)
  tot_long = data_frame(method = ate_long$variable, ate_val = ate_long$value, pval = p_val_long$value)
  q = ggplot(tot_long) +
    aes(x=ate_val, y=pval, colour=method) +
    geom_line(lwd = 1.5) +
    geom_point(cex = 1) +
    geom_segment( aes(x=0, y=0.05, xend = min(ate_val), yend=0.05), color="firebrick", linetype = "dashed", lwd = 1) +
    geom_segment( aes(x=0, y=0.05, xend = max(ate_val), yend=0.05), color="firebrick", linetype = "dashed", lwd = 1) +
    ggtitle(label = paste(title)) +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          legend.text = element_text(size = 12, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold", hjust = 0.5),
          legend.key.size = unit(1.5, 'cm'),
          legend.position = "left",
          legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.text.align = 0.7,
          axis.text = element_text(size = 12, face = "bold", hjust = 0.5)) +
    xlab("ATE") +
    ylab("P value")
  return(q)
}
make_ridgeline = function(run, title=NULL, subtitle=NULL, limits = c(-1,1)){
  comp_df = as.data.frame(run[[1]])
  ate_val = as.data.frame(run[[1]])

  names= unlist(lapply(colnames(ate_val), function(y) rep(y, nrow(ate_val))))
  value= as.numeric(unlist(ate_val))
  data=tibble(names,value)

  p <- ggplot(data, aes(x=value, y=names, fill=names)) +
    geom_density_ridges() +
    geom_segment( aes(y=1, yend=length(colnames(comp_df))+2, x=run[[8]][1], xend=run[[8]][1]), color="navy", linetype = "dashed", lwd = 1) +
    theme(legend.position="none") +
    scale_fill_brewer(palette="Set3") +
    ylab("Method type") +
    xlab("Beta") +
    ggtitle(label = paste(title),
            subtitle = paste(subtitle)) +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.text = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
    if(max(data$value) > 10 | min(data$value < -10)){
      xlim(limits[1],limits[2])
    }

  return(p)
}
ci_ridges = function(run, title =NULL, subtitle=NULL){
  ate_val = as.data.frame(run[[2]])
  drawn_ci = beta_sum(run)
  drawn_ci[drawn_ci==0] =NA
  drawn_ci[drawn_ci==1] =NA

  # drawn_ci$tag = c(1,1,0,0)
  # drawn_ci = drawn_ci[!(duplicated(drawn_ci[1:3]) | duplicated(drawn_ci[1:3], fromLast = TRUE)), ]
  # drawn_ci = drawn_ci[drawn_ci$tag==1,]
  # drawn_ci = drawn_ci[-length(drawn_ci)]
  upper = sapply(drawn_ci[7,], function(x) rep(x, length(ate_val[[1]])))
  lower = sapply(drawn_ci[8,], function(x) rep(x, length(ate_val[[1]])))
  #rep(drawn_ci[,1],length(comp_df[[1]]))


  names= unlist(lapply(colnames(ate_val), function(y) rep(y, nrow(ate_val))))
  value= as.numeric(unlist(ate_val))
  upper = 1- as.numeric(unlist(upper))
  lower = as.numeric(unlist(lower))

  data=tibble(names,value, upper, lower)
  data$tag =0
  data[data$value>0,]$tag = 1
  data$names = as.factor(data$names)
  data$tag = as.character(data$tag)

  lines = drawn_ci[1:2,]
  nas_df = drawn_ci[7:8,]
  #na_dex
  inf_dex = which(is.na(nas_df[1,]))
  neg_inf_dex = which(is.na(nas_df[2,]))

  lines[1, inf_dex]= Inf
  lines[2, neg_inf_dex]= -Inf

  factor_df = as.data.frame(unique(as.integer(data$names)))
  factor_df = t(factor_df)
  colnames(factor_df) = unique(as.character(data$names))
  rownames(factor_df) = "i"
  lines = rbind(lines, factor_df)
  #lines = lines[,order(lines[nrow(lines),])]

  drawn_ci = rbind(drawn_ci, factor_df)
  #drawn_ci = drawn_ci[,order(drawn_ci[nrow(drawn_ci),])]

  ###WORKING LINES
  p <- ggplot(data, aes(x=value, y=names)) +
    stat_density_ridges(scale = 0.95,
                        quantile_lines = TRUE,
                        quantile_fun = function(x, ...) quantile(x, probs =
                                                                   c(sort(c(mean(data[data$value == x,]$lower))), sort(c(mean(data[data$value == x,]$upper)))), na.rm = TRUE)
    ) +
    theme_ridges(center = TRUE) +
    ylab("Anaylsis Performed") +
    xlab("Estimated Beta") +
    ggtitle(label = paste(title),
            subtitle = paste(subtitle)) +
    theme(plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14, face = "bold")) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, .1))) +
    if(max(data$value) > 10 | min(data$value < -10)){
      xlim(limits[1],limits[2])
    }
  p

  d <- ggplot_build(p)$data[[1]]
  ribbon = function(upper, lower, i) {
    if(upper == Inf & lower == -Inf){
      return()
    }
    q = geom_ribbon(
      data = transform(subset(d, x <= upper & x >= lower & ymin == i), names = group),
      aes(x, ymin = ymin, ymax = ymax, group = group),
      fill = "lightblue2")
    return(q)
  }
  # i = 3
  # p + ribbon(lines[1,i], lines[2,i], lines[3,i])
  # p + ribbon(-0.1, -0.2, 2)
  q = p + lapply(c(1:ncol(lines)), function(x) ribbon(lines[1,x],lines[2,x],lines[3,x]))

  # q =p + geom_ribbon(
  #   data = transform(subset(d, x <= 0.0373545924 & x >= -0.0412792948 & ymin == 3), names = group),
  #   aes(x, ymin = ymin, ymax = ymax, group = group),
  #   fill = "lightblue2") +
  #   geom_ribbon(
  #     data = transform(subset(d, x <= 0.0348230349 & x >= -0.0357772047  & ymin == 2), names = group),
  #     aes(x, ymin = ymin, ymax = ymax, group = group),
  #     fill = "lightblue2") +
  #   geom_ribbon(
  #     data = transform(subset(d, ymin == 1), names = group),
  #     aes(x, ymin = ymin, ymax = ymax, group = group),
  #     fill = "lightblue2") +
  #   geom_segment( aes(y=1, yend=length(colnames(ate_val))+1, x=run[[8]][1], xend=run[[8]][1]), color="navy", linetype = "dashed", lwd = 1)

  r = q+ stat_density_ridges(scale = 0.95,
                             quantile_lines = TRUE,
                             quantile_fun = function(x, ...) quantile(x, probs =
                                                                        c(sort(c(mean(data[data$value == x,]$lower))), sort(c(mean(data[data$value == x,]$upper)))), na.rm = TRUE),
                             fill = "lightblue2",
                             alpha= 0.01) +
    geom_segment( aes(y=1, yend=length(colnames(ate_val))+1, x=run[[9]][1], xend=run[[9]][1]), color="navy", linetype = "dashed", lwd = 1)




  ###



  # Construct the six grobs - three symbols and three labels
  L1 = rectGrob(height = .5, width = .5, gp = gpar(fill = "lightblue2", col = NA))
  L2 = rectGrob(height = .5, width = .5, gp = gpar(fill = "grey50", col = NA))
  T1 = textGrob("Yes", x = .2, just = "left")
  T2 = textGrob("No", x = .2, just = "left")


  # Construct a gtable - 2 columns X 4 rows
  leg = gtable(width = unit(c(1,1), "cm"), height = unit(c(1.8,1,1), "cm"))

  # Place the six grob into the table
  leg = gtable_add_grob(leg, L1, t=2, l=1)
  leg = gtable_add_grob(leg, L2, t=3, l=1)
  leg = gtable_add_grob(leg, T1, t=2, l=2)
  leg = gtable_add_grob(leg, T2, t=3, l=2)

  # Give it a title (if needed)
  leg = gtable_add_grob(leg, textGrob(expression(bold("True B in\n95% CI?")), vjust = 2), t=1, l=1, r=2)
  #leg = gtable_add_grob(leg, textGrob(expression(bold("95% CI?"))), t=2, l=1, r=2)
  # Get the ggplot grob for plot1
  g = ggplotGrob(r)

  # Get the position of the panel,
  # add a column to the right of the panel,
  # put the legend into that column,
  # and then add another spacing column
  pos = g$layout[grepl("panel", g$layout$name), c('t', 'l')]
  g = gtable_add_cols(g, sum(leg$widths), pos$l)
  g = gtable_add_grob(g, leg, t = pos$t, l = pos$l + 1)
  g = gtable_add_cols(g, unit(6, "pt"), pos$l)

  # Draw it
  grid.newpage()
  return(grid.draw(g))

}
#gives average beta, sd of betas and
beta_sum = function(run){
  ate = run[[9]][2]
  calc_ate = as.data.frame(run[[2]])
  lower_int = as.data.frame(run[[3]])
  upper_int = as.data.frame(run[[4]])


  names= unlist(lapply(colnames(lower_int), function(y) rep(y, nrow(lower_int))))
  upper_int= as.numeric(unlist(upper_int))
  lower_int = as.numeric(unlist(lower_int))
  calc_ate = as.numeric(unlist(calc_ate))
  data=tibble(names,lower_int, upper_int, calc_ate)
  data_ci = data %>% filter(lower_int <= ate & upper_int >= ate)
  data_under_ci = data %>% filter(upper_int < ate)
  data_over_ci = data %>% filter(lower_int > ate)
  data_ci$in_ci = 1
  data_under_ci$in_ci = 0
  data_over_ci$in_ci = 0
  data_ci$over_ci = 0
  data_ci$under_ci = 0
  data_under_ci$over_ci = 0
  data_over_ci$over_ci = 1
  data_under_ci$under_ci = 1
  data_over_ci$under_ci = 0

  data = rbind(data_ci, data_over_ci, data_under_ci)
  #max((data_ci %>% filter(names == "logistic_or"))[4])
  max_ci_beta = sapply(unique(data$names), function(x) max((data_ci %>% filter(names == x))[4]))
  min_ci_beta = sapply(unique(data$names), function(x) min((data_ci %>% filter(names == x))[4]))
  max_beta = sapply(unique(data$names), function(x) max((data %>% filter(names == x))[4]))
  min_beta = sapply(unique(data$names), function(x) min((data %>% filter(names == x))[4]))
  mean_beta = sapply(as.list(unique(data$names)), function(x) mean((data %>% filter(names == x))[[4]]))
  sd_beta = sapply(as.list(unique(data$names)), function(x) sd((data %>% filter(names == x))[[4]]))
  percent_over_ci = sapply(unique(data$names), function(x) length((data_over_ci %>% filter(names == x))[[4]])/length(run[[1]][,1]))
  percent_under_ci = sapply(unique(data$names), function(x) length((data_under_ci %>% filter(names == x))[[4]])/length(run[[1]][,1]))
  beta_in_ci = as.data.frame(rbind(max_ci_beta, min_ci_beta, max_beta, min_beta, mean_beta, sd_beta, percent_over_ci, percent_under_ci))
  beta_in_ci[sapply(beta_in_ci, is.infinite)]=NA
  beta_in_ci = beta_in_ci[,c(colnames(run[[1]]))] #reorder columns to original run order for indexing
  return(beta_in_ci)
}


#varied runs lm
constant_ate_graph = function(run, x_data, title = NULL){
  x_val = as.data.frame(run[[x_data]])
  y_val = as.data.frame(run[["calculated_ate"]])
  x_long = melt(x_val)
  y_long = melt(y_val)
  tot_long = data_frame(method = x_long$variable, x_values = x_long$value, y_values = y_long$value)
  q = ggplot(tot_long) +
    aes(x=x_values, y=y_values, colour=method) +
    geom_line(lwd = 1.5) +
    geom_point(cex = 1) +
    geom_segment( aes(x=min(x_val), y=run[["set_ate"]][1], xend = max(x_val), yend=run[["set_ate"]][1]), color="firebrick", linetype = "dashed", lwd = 1) +
    ggtitle(label = paste(title)) +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          legend.text = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold", hjust = 0.5),
          legend.key.size = unit(1, 'cm'),
          legend.position = "right",
          legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.text.align = 0.7,
          axis.text = element_text(size = 12, face = "bold", hjust = 0.5)) +
    xlab(x_data) +
    ylab("calculated_ate")
  return(q)
}

ate_accuracy_graph = function(run, title = NULL, subtitle=NULL, limits = c(-5,5)){
  run[["calculated_ate"]][,!is.infinite(colSums(run[["calculated_ate"]]))==FALSE] = rep(0, length(run[["calculated_ate"]][,1]))
  x_val = as.data.frame(run[["set_ate"]])
  r_2 = unlist(lapply(colnames(x_val), function(x) cor(run[["calculated_ate"]][,x],run[["set_ate"]][,x])))
  m = unlist(lapply(colnames(x_val), function(x) as.numeric(lm(run[["calculated_ate"]][,x]~run[["set_ate"]][,x])$coefficients[2])))
  int = unlist(lapply(colnames(x_val), function(x) as.numeric(lm(run[["calculated_ate"]][,x]~run[["set_ate"]][,x])$coefficients[[1]])))
  colnames(x_val) = paste0(colnames(x_val), ", R2 = ", as.character(round(r_2,3)))
  colnames(x_val) = paste0(colnames(x_val), ", Slope = ", as.character(round(m,3)))
  colnames(x_val) = paste0(colnames(x_val), ", Intercept = ", as.character(round(int,3)))
  y_val = as.data.frame(run[["calculated_ate"]])
  colnames(y_val) = paste0(colnames(y_val), ", R2 = ", as.character(round(r_2,3)))
  x_long = melt(x_val)
  y_long = melt(y_val)
  tot_long = data_frame(method = x_long$variable, x_values = x_long$value, y_values = y_long$value)
  q = ggplot(tot_long) +
    aes(x=x_values, y=y_values, colour=method) +
    geom_line(lwd = 1.5) +
    geom_point(cex = 1) +
    ggtitle(label = paste(title),
            subtitle = paste(subtitle)) +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.text = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold", hjust = 0.5),
          legend.key.size = unit(1, 'cm'),
          legend.position = "bottom",
          legend.direction = "vertical",
          legend.box.margin = margin(10, 10, 10, 10),
          legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.text.align = 0.7,
          axis.text = element_text(size = 12, face = "bold", hjust = 0.5)) +
    scale_color_brewer(palette="Set3") +
    xlab("Set ATE") +
    ylab("Calculated ATE") +
    if(max(y_long$value) > 10 | min(y_long$value < -10)){
      ylim(limits[1],limits[2])
    }
  return(q)
}

sum_table_graphic = function(run){
  myt <- ttheme_default(
    rowhead = list(fg_params=list(cex = 1.0, fontface = "bold"), bg_params=list(fill="gray80", col = "black")),
    colhead = list(bg_params = list(col = "black")),
    core = list(bg_params = list(fill = "white", col = "black"))
  )



  the_table = beta_sum(run)
  the_table = the_table[5:8,]
  the_table[3,] = the_table[3,]+ the_table[4,]
  the_table = the_table[1:3,]
  the_table[3,] = 100 - the_table[3,]*100
  the_table = round(the_table,3)
  rows= c("B estimate\n mean", "B estimate\n std dev", "True B in 95% CI of B estimate (%)")
  rownames(the_table) = sapply(rows, function(x) paste(strwrap(x, width = 15),  collapse="\n"))
  cols = colnames(the_table)
  colnames(the_table) = sapply(cols, function(x) paste(strwrap(x, width = 20),  collapse="\n"))
  the_table = as.data.frame(t(the_table))
  g5 <- tableGrob(the_table, theme = myt)

  grid.newpage()
  return(grid.draw(g5))
}
#only does MDE if SD is 1, df is n-1
sum_runs = function(run, a= 0.05, b = .2){
  x_val = as.data.frame(run[["set_ate"]])
  run[["calculated_ate"]][,!is.infinite(colSums(run[["calculated_ate"]]))==FALSE] = rep(-1000, length(run[["calculated_ate"]][,1]))
  mde = unlist(lapply(colnames(x_val), function(x) (1-(pt(.05, run[[5]][,x]-1))+(1-pt(.05, run[[5]][,x]-1)))*sqrt(1 / (run[[3]][,x] * (1 - run[[3]][,x]) * run[[5]][,x] ) )))
  r_2 = unlist(lapply(colnames(x_val), function(x) cor(run[["calculated_ate"]][,x],run[["set_ate"]][,x])))
  methods = unlist(lapply(1:ncol(x_val), function(x) rep(colnames(x_val)[x],nrow(x_val))))
  #r_means = unlist(lapply(colnames(x_val), function(x) mean(run[["calculated_ate"]][,x])))
  r_sds = unlist(lapply(colnames(x_val), function(x) sd(run[["calculated_ate"]][,x])))
  #p_vals = unlist(lapply(colnames(x_val), function(x) mean(run[["p_values"]][,x])))
  m = unlist(lapply(colnames(x_val), function(x) as.numeric(lm(run[["calculated_ate"]][,x]~run[["set_ate"]][,x])$coefficients[2])))
  int = unlist(lapply(colnames(x_val), function(x) as.numeric(lm(run[["calculated_ate"]][,x]~run[["set_ate"]][,x])$coefficients[[1]])))
  df = data.frame(r2 = r_2,
                  slope = m,
                  intercept = int,
                  mde = mde,
                  calculated_ate = as.vector(run[[1]]),
                  ate_sd = r_sds,
                  p_values = as.vector(run[[2]]),
                  exp_p = as.vector(run[[3]]),
                  out_p = as.vector(run[[4]]),
                  sample_population = as.vector(run[[5]]),
                  set_ate = as.vector(run[[6]]),
                  over_r = as.vector(run[[7]]),
                  under_r = as.vector(run[[8]]),
                  method = methods)
  return(df)
}

run_sum_df = function(run_list){
  slope_sums = lapply(run_list, sum_runs)
  slope_df = bind_rows(slope_sums)
  return(slope_df)
}
run_sum_graph = function(run_list, x_axis, y_axis, title =  NULL, subtitle = NULL, limits = c(-5,5), x_lab = paste0(x_axis), y_lab = paste0(y_axis)){
  slope_sums = lapply(run_list, sum_runs)
  slope_df = bind_rows(slope_sums)
  slope_df$method = as.factor(slope_df$method)
  slope_df = data_frame(slope_df)
  spec_df = slope_df[,c("method", x_axis, y_axis)]
  colnames(spec_df) = c("method", "x_vals", "y_vals")
  q = ggplot(spec_df) +
    aes(x=x_vals, y=y_vals, colour = method) +
    geom_line(lwd = 1.5) +
    ggtitle(label = paste(title),
            subtitle = paste(subtitle)) +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          legend.text = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold", hjust = 0.5),
          legend.key.size = unit(1, 'cm'),
          legend.position = "right",
          legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.text.align = 0.7,
          axis.text = element_text(size = 12, face = "bold", hjust = 0.5)) +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_color_brewer(palette="Set3") +
    if(max(spec_df$y_vals) > 10 | min(spec_df$y_vals < -10)){
      ylim(limits[1],limits[2])
    }
  return(q)
}
method_breakdown = function(run, simplifed = F, limits = c(-5,5)){
  values = lapply(c(1:length(run)), function(x) melt(run[x])[,3])
  outputs = lapply(c(1:length(run)), function(x) names(run[x]))
  methods = lapply(c(1:length(run)), function(x) melt(run[x])[,2])[[1]]
  names(values) = unlist(outputs)
  df = as.data.frame(values)
  df$methods = methods
  use_col = unlist(lapply(c(1:length(df)), function(x) if(length(unique(df[,x]))==1) {x=NULL} else {x=x} ))
  df = df[,use_col]
  #good simplified distribution graph: q = ggpairs(df[,c("calculated_ate","methods")], ggplot2::aes(colour=methods))
  if(simplifed == T){
    # graphs = ggpairs(df[,c("calculated_ate","methods")], ggplot2::aes(colour=methods))
    # auxplot =  constant_ate_graph(test, "calculated_ate", "wge")
    # method_legend = grab_legend(auxplot)
    # graphs2 = putPlot(graphs, method_legend, 2, 2)
    return(ggpairs(df[,c("calculated_ate","methods")], ggplot2::aes(colour=methods))+
             scale_fill_brewer(palette="Set3")+
             scale_color_brewer(palette="Set3")
             # if(max(df[,"calculated_ate"]) > 10 | min(df[,"calculated_ate"]) < -10){
             #   xlim(limits[1],limits[2])
             # }
             )
  }else{
    return(ggpairs(df, ggplot2::aes(colour=methods))+
             scale_fill_brewer(palette="Set3")+
             scale_color_brewer(palette="Set3"))
  }

}

######

#Original DAG######
# dag = HydeNetwork(~ cont_conf
#                   + cat_conf
#                   + exposure | cat_conf*cont_conf
#                   + outcome | cat_conf*cont_conf*exposure
#                   + cont_coll | exposure*outcome
#                   + cat_coll | exposure*outcome)
#
# dag = setNode(dag, cont_conf, nodeType = "dnorm", mu = 1, tau = 1)
# dag = setNode(dag, cat_conf, nodeType = "dbern", prob = 0.5)
# if(cont_exp==T){
#   dag = setNode(dag, exposure, nodeType = "dnorm", mu = "0.5*cat_conf[1] + 0.5*cont_conf", tau = 1)
# }else{
#   dag = setNode(dag, exposure, nodeType = "dbern", prob = paste0("ilogit(0.5*cat_conf[1] + 0.5*cont_conf+",set_p(exp_p,0.5*0.5 + 0.5), ")"))
# }
# dag = setNode(dag, outcome, nodeType = "dbern", prob = paste0("ilogit(0.5*cat_conf[1] +", ate,"*exposure + 0.5*cont_conf+", set_p(out_p, 0.5*0.5 + 0.5 + 0.75*ate),")"))
# dag = setNode(dag, cont_coll, nodeType = "dnorm", mu = "0.5*exposure + 0.5*outcome[1]", tau = 1)
# dag = setNode(dag, cat_coll,
#               nodeType = "dbern",
#               prob = "ilogit(.3*exposure + .2*outcome[1]-2)")

#####

#PRS_dag #########



# prs_dag = HydeNetwork(~prs
#                       + phenotype | prs
#                       + outcome | phenotype
#                       + in_data_base | outcome*phenotype)
#
# plot(prs_dag)
# prs_dag = setNode(prs_dag, prs, nodeType = "dnorm", mu = 0, tau = 1)
# prs_dag = setNode(prs_dag, phenotype, nodeType = "dbern", prob = paste0("ilogit(0.5*prs+",set_p(exp_p, 0), ")"))
# prs_dag = setNode(prs_dag, outcome, nodeType = "dbern", prob = paste0("ilogit(",ate,"*phenotype+",set_p(out_p, exp_p*ate), ")"))
# prs_dag = setNode(prs_dag, in_data_base, nodeType = "dbern", prob = "ilogit(0.5*outcome + 0.5*phenotype)")

# prs_run = multiple_runs(10000, make_model(1, .1, .1), 20, "prs", "outcome", "phenotype")
# make_boxplot(prs_run, "phenotype as mediator", "estsgd", 0.3)
#
# prs_run_2 = multiple_runs(10000, make_model(1, .1, .1), 20, "prs", "outcome")
# boxplot(prs_run_2[,"beta"])
#
# test = varied_runs(10, "prs", "outcome", "phenotype", ate = 2, n = 5000)
# method_breakdown(test, simplifed = T)
######

#many covariantes dag mirrored betas##########
# dag = HydeNetwork(~a
#                   + b
#                   + c
#                   + d
#                   + e
#                   + f
#                   + g
#                   + h
#                   + exposure | a*b*c*d*e*f*g*h
#                   + outcome | a*b*c*d*e*f*g*h*exposure)
# plot(dag)
# dag = setNode(dag, a, nodeType = "dnorm", mu = 0.2, tau = 1/sqrt(.5))
# dag = setNode(dag, b, nodeType = "dbern", prob = .05,)
# dag = setNode(dag, c, nodeType = "dnorm", mu = 5, tau = 1)
# dag = setNode(dag, d, nodeType = "dnorm", mu = -2, tau = 1/sqrt(5))
# dag = setNode(dag, e, nodeType = "dnorm", mu = 0.0001, tau = 1)
# dag = setNode(dag, f, nodeType = "dbern", prob = 0.5)
# dag = setNode(dag, g, nodeType = "dbern", prob = 0.01)
# dag = setNode(dag, h, nodeType = "dbern", prob = 0.1)
# dag = setNode(dag, exposure, nodeType = "dbern", prob = paste0("ilogit(0.05*a + 1*b + .04*c + 1*d + -4*e + 0.02*f + 5*g + 0.5*h+",
#                                                                set_p(exp_p, 0.2*0.05 + 0.05*1 + 5*0.04 + -2*1 + 0.0001*-4 + 0.5*0.02 + 0.01*5 + 0.1+0.5),")"))
# dag = setNode(dag, outcome, nodeType = "dbern", prob = paste0("ilogit(0.05*a + 1*b + .04*c + 1*d + -4*e + 0.02*f + 5*g + 0.5*h + ", ate,"*exposure + ",
#                                                               set_p(out_p, 0.2*0.05 + 0.05*1 + 5*0.04 + -2*1 + 0.0001*-4 + 0.5*0.02 + 0.01*5 + 0.1+0.5 + ate*exp_p),")"))

# multi_cov = varied_runs(20, "exposure", "outcome", c("a", "b", "c", "d", "e", "f", "g", "h"), n = 10000, exp_p = .05, out_p = .1, ate = 1, under_r = 0, over_r = 0)
# method_breakdown(multi_cov)
# method_breakdown(multi_cov, simplifed = T)
#
# multi_cov_betas = varied_runs(20, "exposure", "outcome", c("a", "b", "c", "d", "e", "f", "g", "h"), n = 10000, exp_p = .05, out_p = .1, under_r = 0, over_r = 0)
# ate_accuracy_graph(multi_cov_betas, "Mirrored Exposure Variables")


#####

#many covariates dag weak exposure betas#########
# dag = HydeNetwork(~a
#                   + b
#                   + c
#                   + d
#                   + e
#                   + f
#                   + g
#                   + h
#                   + exposure | a*b*c*d*e*f*g*h
#                   + outcome | a*b*c*d*e*f*g*h*exposure)
# plot(dag)
# dag = setNode(dag, a, nodeType = "dnorm", mu = 0.2, tau = 1/sqrt(.5))
# dag = setNode(dag, b, nodeType = "dbern", prob = .05,)
# dag = setNode(dag, c, nodeType = "dnorm", mu = 5, tau = 1)
# dag = setNode(dag, d, nodeType = "dnorm", mu = -2, tau = 1/sqrt(5))
# dag = setNode(dag, e, nodeType = "dnorm", mu = 0.0001, tau = 1)
# dag = setNode(dag, f, nodeType = "dbern", prob = 0.5)
# dag = setNode(dag, g, nodeType = "dbern", prob = 0.01)
# dag = setNode(dag, h, nodeType = "dbern", prob = 0.1)
# dag = setNode(dag, exposure, nodeType = "dbern", prob = paste0("ilogit(0.01*a + 0.01*b + .01*c + 0.01*d + 0.01*e + 0.01*f + 0.01*g + 0.01*h+",
#                                                                set_p(exp_p, 0.2*0.05 + 0.05*1 + 5*0.04 + -2*1 + 0.0001*-4 + 0.5*0.02 + 0.01*5 + 0.1+0.5),")"))
# dag = setNode(dag, outcome, nodeType = "dbern", prob = paste0("ilogit(0.05*a + 1*b + .04*c + 1*d + -4*e + 0.02*f + 5*g + 0.5*h + ", ate,"*exposure + ",
#                                                               set_p(out_p, 0.2*0.05 + 0.05*1 + 5*0.04 + -2*1 + 0.0001*-4 + 0.5*0.02 + 0.01*5 + 0.1+0.5 + ate*exp_p),")"))
#

# weak_exp_beta_multi_cov = varied_runs(20, "exposure", "outcome", c("a", "b", "c", "d", "e", "f", "g", "h"), n = 10000, exp_p = .05, out_p = .1, ate = 1, under_r = 0, over_r = 0)
#
# method_breakdown(weak_exp_beta_multi_cov)
# method_breakdown(weak_exp_beta_multi_cov, simplifed = T)
#
# weak_exp_beta_multi_cov_betas = varied_runs(20, "exposure", "outcome", c("a", "b", "c", "d", "e", "f", "g", "h"), n = 10000, exp_p = .05, out_p = .1, under_r = 0, over_r = 0)
# ate_accuracy_graph(weak_exp_beta_multi_cov_betas, "weak exposure variables")
#####

#convergant variables########
# dag = HydeNetwork(~exposure
#                   + mediator|exposure
#                   + outcome|mediator*exposure)
#
# dag = setNode(dag, exposure, nodeType = "dbern", prob = exp_p)
# dag = setNode(dag, mediator, nodeType = "dbern", prob = "ilogit(0.1*exposure)")
# dag = setNode(dag, outcome, nodeType = "dbern", prob = paste0("ilogit(",ate,"*exposure + 0.1*mediator + 3*exposure*mediator + ",set_p(out_p, 0.1*exp_p + 0.1*plogis(0.1*exp_p) + 3*plogis(0.1*exp_p)*exp_p) ,")"))


# converge_med = varied_runs(20, "exposure", "outcome", "mediator", n = 10000, exp_p = .05, out_p = .1, ate = .1, under_r = 0, over_r = 0)
# method_breakdown(converge_med)
# method_breakdown(converge_med, simplifed = T)
#
#
# converge_med_beta = varied_runs(20, "exposure", "outcome", "mediator", n = 10000, exp_p = .05, out_p = .1, under_r = 0, over_r = 0)
# ate_accuracy_graph(converge_med_beta)
#
#
iges_demo = function(ate, exp_p, out_p){
  dag = HydeNetwork(~sex
                    + under_40
                    + depression|under_40*sex
                    + diabetes|under_40*sex*depression
                    + in_biobank|sex*under_40*depression*diabetes)
  plot(dag)
  df = data.frame(sex = c(.46, -0.094, 0.7),
                  under_40 = c(0.91, -0.62, -1.14),
                  depression = c(0,0,.23),
                  diabetes = c(0,0,.92))
  dag = setNode(dag, sex, nodeType = "dbern", prob = 0.52)
  dag = setNode(dag, under_40, nodeType = "dbern", prob = 0.39)

  dag = setNode(dag, depression, nodeType = "dbern",
                prob = paste0("ilogit(", df[1,1], "*sex +",
                              df[1,2], "*under_40 +",
                              set_p(exp_p,
                                    0.52*df[1,1] +
                                    0.39*df[1,2]),")"))

  dag = setNode(dag, diabetes, nodeType = "dbern",
                prob = paste0("ilogit(", df[2,1], "*sex +",
                              df[2,2], "*under_40 +",
                              ate,"*depression +",
                              set_p(out_p, 0.52*df[2,1] +
                                      0.39*df[2,2] +
                                      ate*exp_p),")"))

  dag = setNode(dag, in_biobank, nodeType = "dbern",
                prob = paste0("ilogit(", df[3,1], "*sex +", df[3,2], "*under_40 +", df[3,3],"*depression +", df[3,4],"*diabetes +", set_p(.76, 0.52*df[3,1] + 0.39*df[3,2] + df[3,3]*exp_p + df[3,4]*out_p),")"))
}

uk_bio_dag = function(ate, exp_p, out_p){
  dag = HydeNetwork(~non_smoke
                    + qualifications
                    + qualifications|non_smoke
                    + in_biobank|non_smoke*qualifications)
  plot(dag)
  dag = setNode(dag, non_smoke, nodeType = "dbern", prob = exp_p)
  dag = setNode(dag, qualifications, nodeType = "dbern", prob = paste0("ilogit(", ate, "*non_smoke +" , set_p(out_p, ate * exp_p),")"))
  dag = setNode(dag, in_biobank, nodeType = "dbern", prob = paste0("ilogit(", .64, "*non_smoke +", .48, "*qualifications)"))
}


#plogis(set_p(0.000023*1,0)+ set_p(0.00077, set_p(0.000023*39,0))) example linker for continuous
biovu_dag = function(ate, exp_p, out_p){
  oop = out_p
  dag = HydeNetwork(~age
                    + snp
                    + sex
                    + race
                    + ethnicity
                    + depression| sex * race * ethnicity * snp
                    + diabetes| sex * race * ethnicity * age * snp
                    + in_biovu| sex * race * ethnicity * age * depression * diabetes)
  plot(dag)
  dag = setNode(dag, age, nodeType = "dunif", a = 0.1, b=85 )
  dag = setNode(dag, snp, nodeType = "dbern", prob = exp_p)
  dag = setNode(dag, sex, nodeType = "dbern", prob = 0.505)
  dag = setNode(dag, race, nodeType = "dbern", prob = 0.242)
  dag = setNode(dag, ethnicity, nodeType = "dbern", prob =0.189 )
  dag = setNode(dag, depression, nodeType = "dbern",
                prob = paste0("ilogit(", .2, "* snp +",
                              0.9, "* sex +",
                              0.15, "* race +",
                              0.36, "* ethnicity +",
                              set_p(0.16, exp_p*.2
                                    + 0.505 * 0.9
                                    + 0.189 * 0.36
                                    + 0.242 * 0.15)
                                    ,")")
  )
  dag = setNode(dag, diabetes, nodeType = "dbern",
                prob = paste0("ilogit(log((", 0.0029, "* age)/(1 -", 0.0029, "* age )) + ",
                              ate, "* snp +",
                              -0.094, "* sex +",
                              0.69, "* race +",
                              0.88, "* ethnicity +",
                              set_p(0.074, set_p(0.0029*39,0)
                                    + ate*exp_p
                                    + 0.505 * -0.094
                                    + 0.242 * 0.69
                                    + 0.189*0.88)
                              ,")")
  )
  dag = setNode(dag, in_biovu, nodeType = "dbern",
                prob = paste0("ilogit(log((", 0.000023, "* age)/(1 -", 0.000023, "* age )) + ",
                              0.25, "* sex +",
                              -0.6, "* race +",
                              -2, "* ethnicity +",
                              0.23, "* depression +",
                              0.92, "* diabetes +",
                              set_p(0.1, set_p(0.000023*39,0)
                                    + 0.25 * 0.505
                                    + -0.6 * 0.242
                                    + -2 * 0.189
                                    + 0.92 * 0.074
                                    + 0.23 * 0.16)
                              ,")")
  )
}

test_dag = function(ate, exp_p, out_p){
  oop = out_p
  oppy = exp_p
  dag = HydeNetwork(~age
                    + sex
                    + depression| sex * age
                    + diabetes|sex *  age
                    + in_biovu| sex * age * depression * diabetes)

  plot(dag)

  dag = setNode(dag, age, nodeType = "dnorm", mu =1, tau = 1 )
  dag = setNode(dag, sex, nodeType = "dbern", prob = 0.505)
  dag = setNode(dag, depression, nodeType = "dbern",
                prob = paste0("ilogit(", 0.1, "* age +",
                              0.9, "* sex +",
                              set_p(0.16, 0.1 * 1
                                    + 0.505 * 0.9)
                              ,")")
  )
  dag = setNode(dag, diabetes, nodeType = "dbern",
                prob = paste0("ilogit(", 0.05, "* age + ",
                              -0.094, "* sex +",
                              ate, "* depression +",
                              set_p(0.074, 0.05 * 1
                                    + 0.505 * -0.094
                                    + ate * 0.16)
                              ,")")
  )
  dag = setNode(dag, in_biovu, nodeType = "dbern",
                prob = paste0("ilogit(", 0.1, "* age + ",
                              0.25, "* sex +",
                              0.23, "* depression +",
                              0.92, "* diabetes +",
                              set_p(0.1, 0.1 * 1
                                    + 0.25 * 0.505
                                    + 0.92 * 0.074
                                    + 0.23 * 0.16)
                              ,")")
  )
}

prs_dag = function(ate, exp_p, out_p){
  oop = out_p
  oppy = exp_p
  dag = HydeNetwork(~prs
                    + depression| prs
                    + diabetes
                    + in_biovu| depression * diabetes)

  plot(dag)

  dag = setNode(dag, prs, nodeType = "dnorm", mu =0, tau = 1 )
  dag = setNode(dag, diabetes, nodeType = "dbern", prob = 0.11)
  dag = setNode(dag, depression, nodeType = "dbern",
                prob = paste0("ilogit(", 0.07, "* prs +",
                              set_p(0.16, 0.07 * 0)
                              ,")")
  )
  dag = setNode(dag, in_biovu, nodeType = "dbern",
                prob = paste0("ilogit(", 2, "* depression +",
                              2, "* diabetes +",
                              set_p(0.1, 2 * 0.16
                                    + 2 * 0.11)
                              ,")")
  )
}

med_dag = function(ate, exp_p, out_p){
  oop = out_p
  oppy = exp_p
  dag = HydeNetwork(~snp
                    + depression| snp
                    + in_biovu| depression)

  plot(dag)

  dag = setNode(dag, snp, nodeType = "dbern", prob = 0.05)
  dag = setNode(dag, depression, nodeType = "dbern",
                prob = paste0("ilogit(", 1, "* snp +",
                              set_p(0.16, 1 * 0.05)
                              ,")")
  )
  dag = setNode(dag, in_biovu, nodeType = "dbern",
                prob = paste0("ilogit(", 1, "* depression +",

                              set_p(0.1, 1 * 0.16 )
                              ,")")
  )
}

snp_dag = function(ate, exp_p, out_p){
  oop = out_p
  oppy = exp_p
  dag = HydeNetwork(~snp1
                    + snp2
                    + zip_code
                    + diabetes | snp2 * zip_code
                    + depression| snp1
                    + in_biovu| depression * diabetes * zip_code)

  plot(dag)

  # dag = setNode(dag, age, nodeType = "dnorm", mu =1, tau = 1 )
  dag = setNode(dag, snp1, nodeType = "dbern", prob = 0.01)
  dag = setNode(dag, snp2, nodeType = "dbern", prob = 0.05)
  dag = setNode(dag, zip_code, nodeType = "dbern", prob = 0.2)
  dag = setNode(dag, diabetes, nodeType = "dbern",
                prob = paste0("ilogit(", 0.3, "* snp2 +",
                              4, "* zip_code +",
                              set_p(0.11, 0.3 * 0.05 +
                                    4 * 0.2)
                              ,")")
  )
  dag = setNode(dag, depression, nodeType = "dbern",
                prob = paste0("ilogit(", 0.3, "* snp1 +",
                              set_p(0.16, 0.3 * 0.01)
                              ,")")
  )
  dag = setNode(dag, in_biovu, nodeType = "dbern",
                prob = paste0("ilogit(", 2, "* depression +",
                              2, "* diabetes +",
                              4, "* zip_code + ",
                              set_p(0.05, 2 * 0.16 +
                                      4 * 0.2 +
                                      2 * 0.11)
                              ,")")
  )
}

#alt biovu has SNP JUST to depression, ate between SNP and depression
biovu_dag_alt = function(ate, exp_p, out_p){
  oop = out_p
  dag = HydeNetwork(~age
                    + snp
                    + sex
                    + race
                    + ethnicity
                    + depression| sex * race * ethnicity * snp
                    + diabetes| sex * race * ethnicity * age
                    + in_biovu| sex * race * ethnicity * age * depression * diabetes)
  plot(dag)
  dag = setNode(dag, age, nodeType = "dunif", a = 0.1, b=85 )
  dag = setNode(dag, snp, nodeType = "dbern", prob = exp_p)
  dag = setNode(dag, sex, nodeType = "dbern", prob = 0.505)
  dag = setNode(dag, race, nodeType = "dbern", prob = 0.758)
  dag = setNode(dag, ethnicity, nodeType = "dbern", prob =0.189 )
  dag = setNode(dag, depression, nodeType = "dbern",
                prob = paste0("ilogit(", ate, "* snp +",
                              0.9, "* sex +",
                              0.15, "* race +",
                              0.36, "* ethnicity +",
                              set_p(0.16, exp_p*ate
                                    + 0.505 * 0.9
                                    + 0.189 * 0.36
                                    + 0.242 * 0.15)
                              ,")")
  )
  dag = setNode(dag, diabetes, nodeType = "dbern",
                prob = paste0("ilogit(log((", 0.0029, "* age)/(1 -", 0.0029, "* age )) + ",
                              -0.094, "* sex +",
                              0.69, "* race +",
                              0.88, "* ethnicity +",
                              set_p(0.074, set_p(0.0029*39,0)
                                    + 0.505 * -0.094
                                    + 0.242 * 0.69
                                    + 0.189*0.88)
                              ,")")
  )
  dag = setNode(dag, in_biovu, nodeType = "dbern",
                prob = paste0("ilogit(log((", 0.000023, "* age)/(1 -", 0.000023, "* age )) + ",
                              0.25, "* sex +",
                              -0.6, "* race +",
                              -2, "* ethnicity +",
                              0.23, "* depression +",
                              0.92, "* diabetes +",
                              set_p(0.0077, set_p(0.000023*39,0)
                                    + 0.25 * 0.505
                                    + -0.6 * 0.242
                                    + -2 * 0.189
                                    + 0.92 * 0.074
                                    + 0.23 * 0.16)
                              ,")")
  )
}


orig_dag = function(ate, exp_p, out_p){
  dag = HydeNetwork(~cont_conf
                    + bi_conf
                    + exposure|cont_conf*bi_conf
                    + outcome|cont_conf*bi_conf*exposure
                    +cont_col|exposure*outcome
                    +bi_col|exposure*outcome)
  plot(dag)
  df = data.frame(cont_conf = c(0.5, 0.5),
                  bi_conf = c(0.5, 0.5),
                  cont_col = c(0.5, 0.5),
                  bi_col = c(5, 0.5))
  rownames(df) = c("exposure", "outcome")

  dag = setNode(dag, cont_conf, nodeType = "dnorm", mu = 8, tau = 1)
  dag = setNode(dag, bi_conf, nodeType = "dbern", prob = 0.1)

  dag = setNode(dag, exposure, nodeType = "dbern",
                prob = paste0("ilogit(log((", 0.02, "* cont_conf)/(1 -", 0.02, "* cont_conf)) + ",
                              df[1,2], "*bi_conf +",
                              set_p(exp_p,
                                    set_p(0.02 * 2,0) +
                                    0.1*df[1,2]),")"))

  dag = setNode(dag, outcome, nodeType = "dbern",
                prob = paste0("ilogit(log((", 0.01, "* cont_conf)/(1 -", 0.01, "* cont_conf)) + ",
                              df[2,2], "*bi_conf +",
                              ate,"*exposure +",
                              set_p(exp_p,
                                    set_p(0.01 * 2,0) +
                                    0.1*df[2,2] +
                                    ate*exp_p),")"))

  dag = setNode(dag, cont_col, nodeType = "dnorm", mu =paste0("ilogit(", df[1,3], "*exposure +", df[2,3], "*outcome)"), tau = 1)

  dag = setNode(dag, bi_col, nodeType = "dbern",
                prob = paste0("ilogit(", df[1,4], "*exposure +",
                              df[2,4], "*outcome + ",
                              set_p(.5,
                                    df[1,4]*exp_p +
                                    df[2,4]*out_p),")"))
}

orig_dag_2 = function(ate, exp_p, out_p){
  dag = HydeNetwork(~bi_conf
                    + exposure|bi_conf
                    + outcome|bi_conf*exposure
                    +cont_col|exposure*outcome
                    +bi_col|exposure*outcome)
  plot(dag)

  dag = setNode(dag, bi_conf, nodeType = "dbern", prob = 0.1)

  dag = setNode(dag, exposure, nodeType = "dbern",
                prob = paste0("ilogit(",
                              0.5, "*bi_conf +",
                              set_p(exp_p,
                                      0.1*0.5),")"
                              ))

  dag = setNode(dag, outcome, nodeType = "dbern",
                prob = paste0("ilogit(",
                              0.5, "*bi_conf +",
                              ate,"*exposure +",
                              set_p(exp_p,
                                      0.1*0.5 +
                                      ate*exp_p),")"
                              ))

  dag = setNode(dag, cont_col, nodeType = "dnorm", mu =paste0("ilogit(", 0.5, "*exposure +", 0.5, "*outcome)"), tau = 1)

  dag = setNode(dag, bi_col, nodeType = "dbern",
                prob = paste0("ilogit(", 0.5, "*exposure +",
                              0.5, "*outcome + ",
                              set_p(.5,
                                    0.5*exp_p +
                                    0.5*out_p),")"))
}

cov_8_dag = function(ate, exp_p, out_p){
  dag = HydeNetwork(~a
                    + b
                    + c
                    + d
                    + e
                    + f
                    + g
                    + h
                    + exposure | a*b*c*d*e*f*g*h
                    + outcome | a*b*c*d*e*f*g*h*exposure)
  plot(dag)
  df = data.frame(a = c(0, 0),
                  b = c(5, 0.001),
                  c = c(5, 0.001),
                  d = c(5, 0.001),
                  e = c(0.001, 5),
                  f = c(0.001, 5),
                  g = c(0.001, 5),
                  h = c(0.001, 5))
  rownames(df) = c("exposure", "outcome")

  dag = setNode(dag, a, nodeType = "dnorm", mu = 0.00001, tau = 1)
  dag = setNode(dag, b, nodeType = "dnorm", mu = 0.00001, tau = 1)
  dag = setNode(dag, c, nodeType = "dnorm", mu = 0.00001, tau = 1)
  dag = setNode(dag, d, nodeType = "dnorm", mu = 0.00001, tau = 1)
  dag = setNode(dag, e, nodeType = "dbern", prob = 0.1,)
  dag = setNode(dag, f, nodeType = "dbern", prob = 0.1)
  dag = setNode(dag, g, nodeType = "dbern", prob = 0.1)
  dag = setNode(dag, h, nodeType = "dbern", prob = 0.1)
  dag = setNode(dag, exposure, nodeType = "dbern", prob = paste0("ilogit(", df[1,1], "*a +", df[1,2], "*b +", df[1,3], "*c +", df[1,4], "*d +", df[1,5], "*e +", df[1,6], "*f +", df[1,7], "*g +", df[1,8], "*h +", set_p(exp_p, 0.1*df[1,1] + 0.1*df[1,2] + 0.1*df[1,3] + 0.1*df[1,4] + 0.00001*df[1,5] + 0.00001*df[1,6] + 0.00001*df[1,7] + 0.0001*df[1,8]),")"))
  dag = setNode(dag, outcome, nodeType = "dbern", prob = paste0("ilogit(", df[2,1], "*a +", df[2,2], "*b +", df[2,3], "*c +", df[2,4], "*d +", df[2,5], "*e +", df[2,6], "*f +", df[2,7], "*g +", df[2,8], "*h +", ate,"*exposure + ",set_p(out_p, 0.1*df[1,1] + 0.1*df[1,2] + 0.1*df[1,3] + 0.1*df[1,4] + 0.00001*df[1,5] + 0.00001*df[1,6] + 0.00001*df[1,7] + 0.0001*df[1,8]),")"))

}

# run_1 = varied_runs(50, cov_8_dag, "exposure", "outcome", c("a", "b", "c", "d", "e", "f", "g", "h"), n = 10000, exp_p = .1, out_p = .1, under_r = 0, over_r = 0)
# ate_accuracy_graph(run_1, "Testing ps weighting", "cont. and exp. = 5, cont. and out. = 0.001, bi and exp = 0.001, bi and out = 5")

cov_4_dag = function(ate, exp_p, out_p){
  dag = HydeNetwork(~a
                    + b
                    + c
                    + d
                    + exposure | a*b*c*d
                    + outcome | a*b*c*d*exposure)
  plot(dag)
  dag = setNode(dag, a, nodeType = "dbern", prob = .1)
  dag = setNode(dag, b, nodeType = "dbern", prob = .1,)
  dag = setNode(dag, c, nodeType = "dnorm", mu = 0.0001, tau = 1)
  dag = setNode(dag, d, nodeType = "dnorm", mu = 0.0001, tau = 1)
  df = data.frame(a = c(.5, 0.5),
                  b = c(.5, 0.5),
                  c = c(.5, 0.5),
                  d = c(.5, 0.5))
  rownames(df) = c("exposure", "outcome")

  dag = setNode(dag, exposure, nodeType = "dbern", prob = paste0("ilogit(", df[1,1], "*a +", df[1,2], "*b +", df[1,3], "*c +", df[1,4], "*d +", set_p(exp_p, 0.1*df[1,1] + 0.1*df[1,2] + 0.0001*df[1,3] + 0.0001*df[1,4]),")"))
  dag = setNode(dag, outcome, nodeType = "dbern", prob = paste0("ilogit(", df[2,1], "*a + ", df[2,2], "*b + ", df[2,3], "*c + ", df[2,4], "*d +", ate,"*exposure + ",set_p(out_p, .1*df[2,1] + 0.1*df[2,2] + 0.0001*df[2,3] + 0.0001*df[2,4] + ate*exp_p),")"))
}

cov_2_dag = function(ate, exp_p, out_p){
  dag = HydeNetwork(~cont
                    + bi
                    + exposure | cont*bi
                    + outcome | cont*bi*exposure)
  plot(dag)

  dag = setNode(dag, bi, nodeType = "dbern", prob = .1,)
  dag = setNode(dag, cont, nodeType = "dnorm", mu = 0.00001, tau = 1)

  df = data.frame(continuous = c(.1, .1),
                  bi = c(.1, 0.1))
  rownames(df) = c("exposure", "outcome")

  dag = setNode(dag, exposure, nodeType = "dbern", prob = paste0("ilogit(", df[1,1], "*cont +", df[1,2], "*bi +", set_p(exp_p, 0.0001*df[1,1] + 0.1*df[1,2]),")"))
  dag = setNode(dag, outcome, nodeType = "dbern", prob = paste0("ilogit(", df[2,1], "*cont + ", df[2,2], "*bi + ", ate, "*exposure +",set_p(out_p, 0.0001*df[2,1] + 0.1*df[2,2] + ate*exp_p),")"))
}

cov_na_nodes = function(ate, exp_p, out_p){
  dag = HydeNetwork(~a
                    + b
                    + c
                    + d
                    + e
                    + f
                    + g
                    + h
                    + exposure | a*e
                    + outcome | a*e*exposure)
  plot(dag)
  df = data.frame(a = c(0.1, 0.1),
                  b = c(0, 0),
                  c = c(0, 0),
                  d = c(0, 0),
                  e = c(.1, .1),
                  f = c(0, 0),
                  g = c(0, 0),
                  h = c(0, 0))
  rownames(df) = c("exposure", "outcome")

  dag = setNode(dag, a, nodeType = "dnorm", mu = 0.00001, tau = 1)
  dag = setNode(dag, b, nodeType = "dnorm", mu = 0.00001, tau = 1)
  dag = setNode(dag, c, nodeType = "dnorm", mu = 0.00001, tau = 1)
  dag = setNode(dag, d, nodeType = "dnorm", mu = 0.00001, tau = 1)
  dag = setNode(dag, e, nodeType = "dbern", prob = 0.1,)
  dag = setNode(dag, f, nodeType = "dbern", prob = 0.1)
  dag = setNode(dag, g, nodeType = "dbern", prob = 0.1)
  dag = setNode(dag, h, nodeType = "dbern", prob = 0.1)
  dag = setNode(dag, exposure, nodeType = "dbern", prob = paste0("ilogit(", df[1,1], "*a +", df[1,5], "*e +", set_p(exp_p, 0.1*df[1,1] + 0.00001*df[1,5]),")"))
  dag = setNode(dag, outcome, nodeType = "dbern", prob = paste0("ilogit(", df[2,1], "*a +", df[2,5], "*e +", ate,"*exposure + ",set_p(out_p, 0.1*df[1,1] + 0.00001*df[1,5]),")"))

}

mean_dag = function(ate, exp_p, out_p){

}

phe_dag = function(ate, exp_p, out_p){
  holder1 = ate
  holder2 = exp_p
  holder3 = out_p
  dag = HydeNetwork(~depression|sex*ethnicity*race + diabetes|sex*ethnicity*race + in_ehr|sex*ethnicity*race*diabetes*depression)
  dag = setNode(dag, depression, nodeType = "dbern", prob = paste0("ilogit(",0.9," * sex + ", 0.36," * ethnicity + ", .15," * race + ", -2.21706807660353,")"))
  dag = setNode(dag, sex, nodeType = "dbern", prob = paste0("ilogit(", 0.0200006667066694,")"))
  dag = setNode(dag, ethnicity, nodeType = "dbern", prob = paste0("ilogit(", -1.45652103905577,")"))
  dag = setNode(dag, race, nodeType = "dbern", prob = paste0("ilogit(", -1.14174565948569,")"))
  dag = setNode(dag, diabetes, nodeType = "dbern", prob = paste0("ilogit(",0.92," * sex + ", -0.94," * ethnicity + ", -0.43," * race + ", -2.27362109693377,")"))
  dag = setNode(dag, in_ehr, nodeType = "dbern", prob = paste0("ilogit(",0.23," * depression + ", 0.095," * sex + ", -0.94," * ethnicity + ", -0.43," * race + ", 0.92," * diabetes + ", -4.49937485013459,")"))
}
# phe = varied_runs(200, phe_dag, exposure = "depression" , outcome = "diabetes" , covariates = NULL , sb = "in_ehr" , ate = 0, n = 1000000)
# phe_adj = varied_runs(200, phe_dag, exposure = "depression" , outcome = "diabetes" , covariates = c("sex", "race") , sb = "in_ehr" , ate = 0, n = 1000000)
# phe_all_adj = varied_runs(200, phe_dag, exposure = "depression" , outcome = "diabetes" , covariates = c("sex", "race", "ethnicity") , sb = "in_ehr" , ate = 0, n = 1000000)
# phe_all_adj_no_sb = varied_runs(200, phe_dag, exposure = "depression" , outcome = "diabetes" , covariates = c("sex", "race", "ethnicity") , sb = NULL , ate = 0, n = 1000000)

# phe_test = reparse_runs(list(phe, phe_adj, phe_all_adj, phe_all_adj_no_sb), "logistic_or", c("no adjustment", "adjust for sex and race", "adjust for all confounders", "adjust for all confounders,\n no selection bias"))
# ci_ridges(phe_test, "Effect Size Estimates Between Depression and Diabetes", "Beta Values from 200 Analysis Iterations")
# sum_table_graphic(phe_test)
#
# ci_ridges(snp_test, "Effect Size Estimates Between rs2254298 and Anxiety", "Beta Values from 500 Analysis Iterations")
# sum_table_graphic(snp_test)

t2d_dag  = function(ate, exp_p, out_p){
  holder1 = ate
  holder2 = exp_p
  holder3 = out_p
  dag = HydeNetwork(~diabetes|rs7903146*bmi  + sbp|bmi*diabetes + pad|sbp*chd*diabetes + chd|diabetes*sbp)
  dag = setNode(dag, diabetes, nodeType = "dbern", prob = paste0("ilogit(",0.358," * rs7903146T + ", 0.231," * bmi + ", -8.96609805209938,")"))
  dag = setNode(dag, rs7903146, nodeType = "dbern", prob = paste0("ilogit(", -1.20831120592453,")"))
  dag = setNode(dag, pad, nodeType = "dbern", prob = paste0("ilogit(",0.637," * diabetes + ", 0.0139," * sbp + ", 0.54," * chd + ", 0,")"))
  dag = setNode(dag, chd, nodeType = "dbern", prob = paste0("ilogit(",0.48," * diabetes + ", 0.0229," * sbp + ", -3.1599,")"))
  dag = setNode(dag, bmi, nodeType = "dnorm", mu = paste0(28.7), tau = 0.447)
  dag = setNode(dag, sbp, nodeType = "dnorm", mu = paste0(10," * diabetes + ", 0.077," * bmi + ",122-28.7*0.077-0.095*10), tau = 0.267)
}
#BUG with rshiny app?? Didn't correctly calculate formula for T2D,
# t2d_dag2 = function(ate, exp_p, out_p){
#   holder1 = ate
#   holder2 = exp_p
#   holder3 = out_p
#   dag = HydeNetwork(~T2D|rs7903146*WC*SBP + PAD|T2D + CHD|T2D*WC*SBP + SBP|BMI + WC|BMI)
#   dag = setNode(dag, T2D, nodeType = "dbern", prob = paste0("ilogit(", -1.75832696071294,")"))
#   dag = setNode(dag, rs7903146, nodeType = "dbern", prob = paste0("ilogit(", -1.56451310052591,")"))
#   dag = setNode(dag, PAD, nodeType = "dbern", prob = paste0("ilogit(",0.7," * T2D + ", -4.2476,")"))
#   dag = setNode(dag, CHD, nodeType = "dbern", prob = paste0("ilogit(",1.09," * T2D + ", 0.029," * WC + ", 0.027," * SBP + ", -23.5627,")"))
#   dag = setNode(dag, WC, nodeType = "dnorm", mu = paste0(0.148," * BMI + ",39.6), tau = 0.253)
#   dag = setNode(dag, SBP, nodeType = "dnorm", mu = paste0(0.821," * BMI + ",122), tau = 0.267)
#   dag = setNode(dag, BMI, nodeType = "dnorm", mu = paste0(28.7), tau = 0.447)
# }

#more weirdness, proportion of SNPs is off
# t2d_dag2 = function(ate, exp_p, out_p){
#   holder1 = ate
#   holder2 = exp_p
#   holder3 = out_p
#   dag = HydeNetwork(~PAD|DM + CHD|DM*WC*SBP + DM|rs7903146*SBP*WC + WC|BMI + SBP|BMI)
#   dag = setNode(dag, PAD, nodeType = "dbern", prob = paste0("ilogit(",0.7," * DM + ", -1.33966262714893,")")) #RIGHT
#   dag = setNode(dag, DM, nodeType = "dbern", prob = paste0("ilogit(",0.21," * WC + ", 0.055," * SBP + ", 0.36," * rs7903146 + ", -16.8671269607129,")")) #RIGHT
#   dag = setNode(dag, CHD, nodeType = "dbern", prob = paste0("ilogit(",1," * DM + ", 0.029," * WC + ", 0.027," * SBP + ", -6.13999741241117,")")) #RIGHT
#   dag = setNode(dag, rs7903146, nodeType = "dbern", prob = paste0("ilogit(", -23.5627,")")) #WRONG
#   dag = setNode(dag, WC, nodeType = "dnorm", mu = paste0(0.821," * BMI + ",39.6), tau = 0.253)
#   dag = setNode(dag, SBP, nodeType = "dnorm", mu = paste0(0.148," * BMI + ",122), tau = 0.267)
#   dag = setNode(dag, BMI, nodeType = "dnorm", mu = paste0(28.7), tau = 0.447)
# }

t2d_dag2 = function(ate, exp_p, out_p){
  holder1 = ate
  holder2 = exp_p
  holder3 = out_p
  dag = HydeNetwork(~PAD|DM + CHD|DM*WC*SBP + DM|rs7903146*SBP*WC + WC|BMI + SBP|BMI)
  dag = setNode(dag, PAD, nodeType = "dbern", prob = paste0("ilogit(",0.7," * DM + ", -1.339663,")"))
  dag = setNode(dag, DM, nodeType = "dbern", prob = paste0("ilogit(",0.21," * WC + ", 0.055," * SBP + ", 0.36," * rs7903146 + ", -16.8671269607129,")"))
  dag = setNode(dag, CHD, nodeType = "dbern", prob = paste0("ilogit(",1," * DM + ", 0.029," * WC + ", 0.027," * SBP + ", -6.13999741241117,")"))
  dag = setNode(dag, rs7903146, nodeType = "dbern", prob = paste0("ilogit(", -1.208311,")"))
  dag = setNode(dag, WC, nodeType = "dnorm", mu = paste0(0.821," * BMI + ",39.6 - 0.821*28.7), tau = 0.253)
  dag = setNode(dag, SBP, nodeType = "dnorm", mu = paste0(0.148," * BMI + ",122 - .148*28.7), tau = 0.267)
  dag = setNode(dag, BMI, nodeType = "dnorm", mu = paste0(28.7), tau = 0.447)
}

no_n = 1000000
no_r = 500
#outcome CHD
naive_chd = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "CHD" , covariates = NULL , sb = NULL , n = no_n)
t2d_chd = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "CHD" , covariates = "DM" , sb = NULL ,  n = no_n)
all_chd = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "CHD" , covariates = c("DM", "PAD", "BMI", "WC", "SBP") , sb = NULL ,  n = no_n)
allbt2d_chd = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "CHD" , covariates = c("PAD", "BMI", "WC", "SBP") , sb = NULL ,  n = no_n)
bmiwcsbp_chd = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "CHD" , covariates = c("BMI", "WC", "SBP") , sb = NULL ,  n = no_n)
padt2d_chd = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "CHD" , covariates = c("PAD", "DM") , sb = NULL ,  n = no_n)
pad_chd = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "CHD" , covariates = c("PAD") , sb = NULL , n = no_n)

chd_results = reparse_runs(list(naive_chd,
                                t2d_chd,
                                all_chd,
                                allbt2d_chd,
                                bmiwcsbp_chd,
                                padt2d_chd,
                                pad_chd), "logistic_or",
                           c("e naive",
                             "a adjusts t2d",
                             "b adjusts all",
                             "f adjusts all but T2D",
                             "g adjusts nodes affecting T2D",
                             "c adjusts t2d and its children",
                             "d adjusts for t2d's children"))
#outcome pad
naive_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = NULL , sb = NULL ,  n = no_n)
t2d_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = "DM" , sb = NULL , n = no_n)
all_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("DM", "SBP", "CHD", "WC", "BMI") , sb = NULL ,  n = no_n)
allbt2d_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("SBP", "CHD", "WC", "BMI") , sb = NULL ,  n = no_n)
bmiwcsbp_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("BMI", "WC", "SBP") , sb = NULL ,  n = no_n)
chdt2d_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("CHD", "DM") , sb = NULL , n = no_n)
chd_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("CHD") , sb = NULL , n = no_n)

pad_results = reparse_runs(list(naive_pad, t2d_pad, all_pad, allbt2d_pad, bmiwcsbp_pad, chdt2d_pad, chd_pad), "logistic_or",
                           c("e  naive", "a adjusts t2d", "b  adjusts all", "f  adjusts all but T2D", "g adjusts nodes affecting T2D", "c adjusts t2d and its children", "d adjusts for t2d's children"))


#outcome bmi
naive_bmi = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "BMI" , covariates = NULL , sb = NULL , n = no_n)
t2d_bmi = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "BMI" , covariates = "DM" , sb = NULL , n = no_n)
all_bmi = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "BMI" , covariates = c("DM", "PAD", "CHD", "WC", "SBP") , sb = NULL ,  n = no_n)
allbt2d_bmi = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "BMI" , covariates = c("PAD", "CHD", "WC", "SBP") , sb = NULL , n = no_n)
wcsbp_bmi = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "BMI" , covariates = c("WC", "SBP") , sb = NULL , n = no_n)
chdpadt2d_bmi = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "BMI" , covariates = c("PAD", "CHD", "DM") , sb = NULL , n = no_n)
chdpad_bmi = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "BMI" , covariates = c("PAD", "CHD") , sb = NULL , n = no_n)

bmi_results = reparse_runs(list(naive_bmi, t2d_bmi, all_bmi, allbt2d_bmi, wcsbp_bmi, chdpadt2d_bmi, chdpad_bmi), "linear_regression",
                           c("f naive", "b adjusts t2d", "c  adjusts all", "d  adjusts all but T2D", "e adjusts nodes affecting T2D", "a adjusts t2d and its children", "g adjusts for t2d's children"))


#outcome wc
naive_wc = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "WC" , covariates = NULL , sb = NULL , n = no_n)
t2d_wc = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "WC" , covariates = "DM" , sb = NULL ,  n = no_n)
all_wc = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "WC" , covariates = c("DM", "PAD", "CHD", "BMI", "SBP") , sb = NULL ,  n = no_n)
allbt2d_wc = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "WC" , covariates = c("PAD", "CHD", "BMI", "SBP") , sb = NULL ,  n = no_n)
bmisbp_wc = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "WC" , covariates = c("BMI", "SBP") , sb = NULL , n = no_n)
chdpadt2d_wc = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "WC" , covariates = c("PAD", "CHD", "DM") , sb = NULL , n = no_n)
chdpad_wc = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "WC" , covariates = c("PAD", "CHD") , sb = NULL , n = no_n)

wc_results = reparse_runs(list(naive_wc, t2d_wc, all_wc, allbt2d_wc, bmisbp_wc, chdpadt2d_wc, chdpad_wc), "linear_regression",
                           c("f  naive", "b adjusts t2d", "c  adjusts all", "d  adjusts all but T2D", "e adjusts nodes affecting T2D", "a adjusts t2d and its children", "g adjusts for t2d's children"))

#outcome sbp
naive_sbp = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "SBP" , covariates = NULL , sb = NULL , ate = 0, n = no_n)
t2d_sbp = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "SBP" , covariates = "DM" , sb = NULL , ate = 0, n = no_n)
all_sbp = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "SBP" , covariates = c("DM", "PAD", "CHD", "WC", "BMI") , sb = NULL , ate = 0, n = no_n)
allbt2d_sbp = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "SBP" , covariates = c("PAD", "CHD", "WC", "BMI") , sb = NULL , ate = 0, n = no_n)
bmiwc_sbp = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "SBP" , covariates = c("BMI", "WC") , sb = NULL , ate = 0, n = no_n)
chdpadt2d_sbp = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "SBP" , covariates = c("PAD", "CHD", "DM") , sb = NULL , ate = 0, n = no_n)
chdpad_sbp = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "SBP" , covariates = c("PAD", "CHD") , sb = NULL , ate = 0, n = no_n)

sbp_results = reparse_runs(list(naive_sbp, t2d_sbp, all_sbp, allbt2d_sbp, bmiwc_sbp, chdpadt2d_sbp, chdpad_sbp), "linear_regression",
                           c("f  naive", "b adjusts t2d", "c  adjusts all", "d  adjusts all but T2D", "e adjusts nodes affecting T2D", "a adjusts t2d and its children", "g adjusts for t2d's children"))


#outcome pad
naive_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = NULL , sb = NULL , ate = 0, n = no_n)
t2d_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = "DM" , sb = NULL , ate = 0, n = no_n)
all_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("DM", "SBP", "CHD", "WC", "BMI") , sb = NULL , ate = 0, n = no_n)
allbt2d_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("SBP", "CHD", "WC", "BMI") , sb = NULL , ate = 0, n = no_n)
bmiwcsbp_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("BMI", "WC", "SBP") , sb = NULL , ate = 0, n = no_n)
chdt2d_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("CHD", "DM") , sb = NULL , ate = 0, n = no_n)
chd_pad = varied_runs(no_r, t2d_dag2, exposure = "rs7903146" , outcome = "PAD" , covariates = c("CHD") , sb = NULL , ate = 0, n = no_n)

pad_results = reparse_runs(list(naive_pad, t2d_pad, all_pad, allbt2d_pad, bmiwcsbp_pad, chdt2d_pad, chd_pad), "logistic_or",
                           c("a  naive", "b adjusts t2d", "c  adjusts all", "d  adjusts all but T2D", "e adjusts nodes affecting T2D", "f adjusts t2d and its children", "g adjusts for t2d's children"))

# chd_results = reparse_runs(list(chd_run, chd_col_run, chd_t2d_run), "logistic_or", c("a  naive", "b  adjusts pad", "c  adjusts T2D"))
# chd_results_a = reparse_runs(list(chd_run, chd_col_run), "logistic_or", c("a  naive", "b  adjusts pad"))
#
#
# chd_n_results = reparse_runs(list(chd_run, chd_run_e5, chd_run_e4), "logistic_or", c("a  n is 1,000,000", "b  n is 100,000", "c  n is 10,000"))



# rep_test = reparse_runs(list(test, test_sb, test_cov, test_cov_sb), "logistic_or", c("naive", "naive, sb", "confounder adjusted, no sb", "confounder adjusted, sb"))
# rep_test_2 = reparse_runs(list(test_m, test_m_sb), "logistic_or", c("naive", "sb"))
#
# ci_ridges(rep_test_2)
# sum_table_graphic(rep_test_2)
#
# rm(test_m)
# rm(test_m_sb)

# biovu_log = reparse_runs(list(sb_cov, cov), "logistic_or", c("covariates + sb", "covariates"))
# biovu_ps = reparse_runs(list(sb_cov, cov), "ps_weighting", c("covariates + sb", "covariates"))

#run_5 = varied_runs(50, uk_bio_dag, "non_smoke", "qualifications", sb = "in_biobank", inv_covariates = "non_smoke",n=100000, exp_p = .16, out_p = 0.20, ate = 0, under_r = 0, over_r = 0)

#run_6 = varied_runs(50, orig_dag, "exposure", "outcome", c("cont_conf", "bi_conf"), sb = "bi_col", inv_covariates = list(c("exposure"), c("exposure", "bi_conf", "cont_conf")), n=100000, exp_p = .16, out_p = 0.20, ate = 0, under_r = 0, over_r = 0)
