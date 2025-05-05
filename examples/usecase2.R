library(McBias)
library(ggplot2)
library(ggridges)
library(grid)

#QC weights to align with mean diff
strat_mean = function(out_mean){
  test_df = create_data(dag,200000)
  test_df = test_df[test_df$In_UKB==1,]
  return(mean(test_df[,out_mean]))
}


#Set dag
edu_beta = 0.7
bmi_beta = -0.21
dag = HydeNetwork(~BMI|Rs303752*Educational_Age+ Educational_Age|Rs303752 + In_UKB|Educational_Age*BMI)
dag = setNode(dag, Rs303752, nodeType = "dbern", prob = 0.41)
dag = setNode(dag, In_UKB, nodeType = "dbern", prob = paste0("ilogit( (",bmi_beta ," * BMI)/(",5.38,") + (", edu_beta," * Educational_Age )/(",1.56,") + ", set_p(0.36, (bmi_beta*28.11)/5.38 + (16.59*edu_beta)/1.56),")"))
dag = setNode(dag, BMI, nodeType = "dnorm", mu = paste0(28.11 ,"+", 0.0859," * Rs303752 + ", -0.188," * Educational_Age + ", 3.11892+ -0.035219), tau = 1/(5.38)^2)
dag = setNode(dag, Educational_Age, nodeType = "dnorm", mu = paste0(16.59, "+", -0.021," * Rs303752 + ", 0.00861), tau = 1/(1.56)^2)


# Simulations for setting mean shifts in selection bias
edu_vec = replicate(100,strat_mean(out_mean = "Educational_Age"))
plot(density(edu_vec), xlab(paste0(paste0("edu beta = ", edu_beta, ", mean = ", round(mean(edu_vec),3)))))
#reported mean edu 17.23

bmi_vec = replicate(100,strat_mean(out_mean = "BMI"))
plot(density(bmi_vec), xlab(paste0(paste0("bmi beta = ", bmi_beta, ", mean = ", round(mean(bmi_vec),3)))))
#reported mean BMI 27.36

#qc check, make sure generated data reflects means/sds/prevalences
test_df = create_data(dag,100000)
strat_df = test_df[test_df$In_UKB==1,]

sum(test_df$In_UKB)/nrow(test_df)

sd(test_df$Educational_Age)
mean(test_df$Educational_Age)
sd(strat_df$Educational_Age)
mean(strat_df$Educational_Age)

mean(test_df$BMI)
sd(test_df$BMI)
sd(strat_df$BMI)
mean(strat_df$BMI)


sum(strat_df$In_UKB)/nrow(strat_df)

sum(test_df$Rs303752)/nrow(test_df)

sum(strat_df$Rs303752)/nrow(strat_df)


#sb non-weighted n = 283749, set n to 283749/UKB prevalence(0.36) to reflect , which is ~788192
#no sb weighted n = 102215

bmi_run = varied_runs(500, dag, exposure = "Rs303752" , outcome = "BMI" , covariates = NULL, sb = NULL , n = 102215)
bmi_sb_run = varied_runs(500, dag, exposure = "Rs303752" , outcome = "BMI" , covariates = NULL , sb = "In_UKB", n = 788192)

edu_run = varied_runs(500, dag, exposure = "Rs303752" , outcome = "Educational_Age" , covariates = NULL , sb = NULL, n = 102215)
edu_sb_run = varied_runs(500, dag, exposure = "Rs303752" , outcome = "Educational_Age" , covariates = NULL , sb = "In_UKB", n = 788192)

tot_bmi_run = reparse_runs(list(bmi_run,bmi_sb_run), "regression",list_names = c("naive","naive UKB selection bias"))
tot_edu_run = reparse_runs(list(edu_run,edu_sb_run), "regression",list_names = c("naive","naive, UKB selection bias") )

bmi_pval = as_tibble(tot_bmi_run[[5]])
bmi_pval$no_sel_b = tot_bmi_run[[2]][,1]
bmi_pval$sel_b =tot_bmi_run[[2]][,2]

bmi_est = as_tibble(tot_bmi_run[[2]])

edu_pval = as_tibble(tot_edu_run[[5]])
edu_est = as_tibble(tot_edu_run[[2]])

