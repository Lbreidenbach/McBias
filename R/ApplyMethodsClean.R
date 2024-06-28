
#roxygen format for documentation
#' Add together two numbers
#'
#' @param x A number.
#' @param y A number.
#' @return A number.
#' @examples
#' add(1, 1)
#' add(10, 1)

#use_package() to import file dependencies under devtools library
#maybe specify package for each dependent function -_-
#use_r("function_name") creates empty r script for just that function
#devtools::load_all(".")
#####

library(HydeNet)
library(rjags)
library(MatchIt)
library(ggplot2)
library(plyr)
library(dplyr)
library(gtable)
library(grid)
library(GGally)
library(ggridges)
library("reshape2")
library(gridExtra)
library(logisticRR)

###CHECK P VAL ACCEPTANCE IN beta_sum()

#Mechanical functions (hidden)######
#x is numeric

set_p = function(p,model){
  p2 = log(p/(1-p))
  b0 = p2-model
  return(b0)
}



#hidden?
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

#prep for read out######
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

#Read outs perhaps should all be hidden#####
#bi outcome maybe cut risk_ratio
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
#any
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
#####


#Data Parsing hidden#####
bi_strat = function(x, df, column){
  index_1 = which(df[column] == x)
  index_0 = which(df[column] != x)
  df[index_0, column] = 0
  df[index_1, column] = 1
  return(list(df[index_1,], df[index_0,]))
}
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

#####

#example create_data(iges_demo, 10000, further arguements)

apply_methods = function(exposure, outcome, covariates=NULL, sb=NULL, df, x=5, div=4, ratio=1, match_methods = NULL){

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

  if(is.null(match_methods)==T & class(df[,outcome])=="integer"){
    tot_df = tot_bind(list(tot_df, odds_ratio(exposure, outcome, covariates, df = df),
                           risk_ratio(exposure, outcome, covariates, df = df),
                           ps_weight(exposure, outcome, covariates, ps_df, "weights")
    ))
    return(tot_df)

  }
  if(is.null(match_methods)==T & class(df[,outcome])=="numeric"){
    tot_df = tot_bind(list(tot_df,
                           lm_beta(exposure, outcome, covariates, df),
                           ps_weight(exposure, outcome, covariates, ps_df, "weights")
    ))
    return(tot_df)

  }

  #matching methods
  match_df_list = lapply(match_methods, function(x) matchit_matching(exposure, covariates, di_df, d = x, ratio))

  #all-accepting methods
  tot_df =  tot_bind(list(tot_df,
                          ps_weight(exposure, outcome, covariates, ps_df, "weights")))
  #tot_df =  tot_bind(list(tot_df, ps_weight(exposure, outcome, covariates, ps_df, "weights")))
  #
  #combining data parses w/ read outs

  if(class(df[,outcome])=="numeric"){
    match_dfs = tot_bind(lapply(c(1:length(match_df_list)), function(x) re(lm_beta(exposure, outcome, df = match_df_list[[x]]), match_methods[[x]])
                                ))
    tot_df = tot_bind(list(tot_df,
                           lm_beta(exposure, outcome, covariates, df),
                           match_dfs
    ))
  }else if(class(df[,outcome])=="integer"){
    match_dfs = tot_bind(lapply(c(1:length(match_df_list)), function(x) re(odds_ratio(exposure, outcome, df = match_df_list[[x]]), match_methods[[x]])
                                ))
    tot_df = tot_bind(list(tot_df,
                           odds_ratio(exposure, outcome, covariates, df),
                           risk_ratio(exposure, outcome, covariates, df),
                           match_dfs
    ))
  }
  tot_df = as.data.frame(apply(tot_df, 2, unlist))
  return(tot_df)
}
varied_runs = function(runs, dag, exposure, outcome, covariates=NULL, sb=NULL, n=10000, misdiagnosis_v = outcome, under_r = 0, over_r = 0, x=5, div = 4, ratio=1, match_methods = NULL, ...){
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
  temp_output = lapply(temp_df, apply_methods, exposure = exposure, outcome = outcome, covariates = covariates, sb = sb, x=x, div=div, ratio=ratio, match_methods=match_methods)

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

reparse_runs = function(run_list, method=NULL, list_names=as.character(1:length(run_list))){
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
#visualization#####
ci_ridges = function(run, title =NULL, subtitle=NULL){
  ate_val = as.data.frame(run[[2]])
  drawn_ci = beta_sum(run)
  all_in_ci = unlist(lapply(c(1:ncol(run[[2]])), function(x){
    if(drawn_ci[7,x]==0&drawn_ci[8,x]==0){
      return(TRUE)
    }else{
      return(FALSE)
    }
  } ))

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
  all_in_dex = which(all_in_ci==TRUE)

  lines[1, inf_dex]= Inf
  lines[2, neg_inf_dex]= -Inf


  factor_df = as.data.frame(unique(as.integer(data$names)))
  factor_df = t(factor_df)
  colnames(factor_df) = unique(as.character(data$names))
  rownames(factor_df) = "i"
  lines = rbind(lines, factor_df, all_in_ci)
  rownames(lines[4,]) = "all"
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
  ribbon = function(upper, lower, i, all) {
    if(upper == Inf & lower == -Inf & all == 0){
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
  q = p + lapply(c(1:ncol(lines)), function(x) ribbon(lines[1,x],lines[2,x],lines[3,x], lines[4,x]))

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

#####


###test DAG
t2d_dag2 = function(){
  dag = HydeNetwork(~PAD|DM + CHD|DM*WC*SBP + DM|rs7903146*SBP*WC + WC|BMI + SBP|BMI)
  dag = setNode(dag, PAD, nodeType = "dbern", prob = paste0("ilogit(",.7," * DM + ", -1.339663,")"))
  dag = setNode(dag, DM, nodeType = "dbern", prob = paste0("ilogit(",0.21," * WC + ", 0.055," * SBP + ", 0.36," * rs7903146 + ", -16.8671269607129,")"))
  dag = setNode(dag, CHD, nodeType = "dbern", prob = paste0("ilogit(",1," * DM + ", 0.029," * WC + ", 0.027," * SBP + ", -6.13999741241117,")"))
  dag = setNode(dag, rs7903146, nodeType = "dbern", prob = paste0("ilogit(", -1.208311,")"))
  dag = setNode(dag, WC, nodeType = "dnorm", mu = paste0(0.821," * BMI + ",39.6 - 0.821*28.7), tau = 0.253)
  dag = setNode(dag, SBP, nodeType = "dnorm", mu = paste0(0.148," * BMI + ",122 - .148*28.7), tau = 0.267)
  dag = setNode(dag, BMI, nodeType = "dnorm", mu = paste0(28.7), tau = 0.447)
}

test_run_500 = varied_runs(500, t2d_dag, "rs7903146", "WC",  sb = "DM", n = 100000)
better_run_500 = varied_runs(500, t2d_dag, "rs7903146", "WC", n = 100000)
comb_run_500 = reparse_runs(list(test_run_500,better_run_500), list_names=c("selection bias", "no selection bias"))
