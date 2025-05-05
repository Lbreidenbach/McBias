#The below code creates the use case DAG####
t2d_dag = HydeNet::HydeNetwork(~PAD|T2D + CHD|T2D*WC*SBP + T2D|rs7903146*SBP*WC + WC|BMI + SBP|BMI)

#The below code sets the distributions and effect sizes of the nodes and edges
t2d_dag = HydeNet::setNode(t2d_dag, PAD, nodeType = "dbern", prob = paste0("ilogit(",.7," * T2D + ", set_p(0.225, 0.7*0.147),")"))
#intercept calculated as log(0.225/(1-0.225)) - (0.7*0.147)

t2d_dag = HydeNet::setNode(t2d_dag, T2D, nodeType = "dbern", prob = paste0("ilogit((",0.21," * WC)/15.6 + (", 0.055," * SBP)/14 + ", 0.34," * rs7903146 + ",
                                                                           set_p(0.147, (0.055*122)/14+(0.21*39.6)/15.6+0.34*0.41),")"))
#Each beta term is divided by its standard deviation to  scale probability between 0 and 1. This preserves prevalence
#The intercept, set by the set_p() is log(0.147/(1-0.147)) - ((0.055*122)/14+(0.21*39.6)/15.6+0.34*0.41)
#The intercept is essentially the prevalence subtracted by the average overall value of the model

# t2d_dag = setNode(t2d_dag, CHD, nodeType = "dbern", prob = paste0("ilogit(",1," * T2D + ", 0.029," * WC + ", 0.02," * SBP + ", set_p(0.175, 1*0.147 + 0.029 * 39.600 + 0.02*122),")"))

t2d_dag = HydeNet::setNode(t2d_dag, CHD, nodeType = "dbern", prob = paste0("ilogit(",1," * T2D + (", 0.029," * WC)/15.6 + (", 0.027," * SBP)/14 + ",
                                                                           set_p(0.175,(0.027*122)/14+(0.029*39.6)/15.6+1*0.147),")"))

#Each beta term is divided by its standard deviation to  scale probability between 0 and 1. This preserves prevalence

t2d_dag = HydeNet::setNode(t2d_dag, rs7903146, nodeType = "dbern", prob = 0.27)
#probability directly set bc it's not dependent on any other nodes

t2d_dag = HydeNet::setNode(t2d_dag, WC, nodeType = "dnorm", mu = paste0(0.821," * BMI + ",39.6 - 0.821*28.7), tau = 1/(15.6^2))
#intercept directly calculated in code, tau is the Gaussian distribution's precision. Precision = 1/variance = 1/(sd^2)

t2d_dag = HydeNet::setNode(t2d_dag, SBP, nodeType = "dnorm", mu = paste0(0.148," * BMI + ",122 - .148*28.7), tau = 1/(14^2))
#intercept directly calculated in code, tau = precision = 1/variance = 1/(sd^2)

t2d_dag = HydeNet::setNode(t2d_dag, BMI, nodeType = "dnorm", mu = paste0(28.7), tau = 1/(5^2))
#BMI is an independent node, mean is directly input, tau = precision = 1/variance = 1/(sd^2)

####
#QC
test_1 = create_data(t2d_dag,1000000)
qc_df = data.frame(measure = c("BMI mean", "BMI sd",
                               "WC mean", "WC sd",
                               "SBP mean", "SBP sd",
                               "rs7903146 prev",
                               "T2D prev",
                               "PAD prev",
                               "CHD prev"
                               ),
                   simulated = c(mean(test_1$BMI), sd(test_1$BMI),
                                 mean(test_1$WC), sd(test_1$WC),
                                 mean(test_1$SBP), sd(test_1$SBP),
                                 sum(test_1$rs7903146)/nrow(test_1),
                                 sum(test_1$T2D)/nrow(test_1),
                                 sum(test_1$PAD)/nrow(test_1),
                                 sum(test_1$CHD)/nrow(test_1)),
                   true = c(28.7, 5,
                            39.6, 15.6,
                            122, 14,
                            0.27,
                            0.147,
                            0.225,
                            0.175))
qc_df

#Below is the following code for running the use case's simulations in the paper####
no_n = 1000000
no_r = 500

#Scenario A, rs7903146's effect on body mass index


#a1
naive_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = NULL , sb = NULL , n = no_n)

#a2
t2d_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = "T2D" , sb = NULL , n = no_n)

#a3
all_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("T2D", "PAD", "CHD", "WC", "SBP") , sb = NULL ,  n = no_n)



bmi_results = reparse_runs(list(naive_bmi, t2d_bmi, all_bmi), "regression",
                           c("c naive (a1)", "b adjusts t2d (a2)", "a  adjusts all (a3)"))

#Create Scernario A's ridgeline plot
ci_ridges(bmi_results)

#Calculate Scerario A's summary statistics
beta_summary(bmi_results)


#Scenario B, rs7903146's effect on bmi

#b1
naive_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = NULL , sb = NULL , n = no_n)

#b2
t2d_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = "T2D" , sb = NULL ,  n = no_n)

#b3
all_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("T2D", "PAD", "CHD", "BMI", "SBP") , sb = NULL ,  n = no_n)

wc_results = reparse_runs(list(naive_wc, t2d_wc, all_wc), "regression",
                          c("c  naive (b1)",
                            "b adjusts t2d (b2)",
                            "a  adjusts all (b3)"
                           ))

#Create Scernario B's ridgeline plot
ci_ridges(wc_results)

#Calculate Scerario B's summary statistics
beta_summary(wc_results)


####
