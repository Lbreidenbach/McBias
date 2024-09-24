#The below code creates the use case DAG####
t2d_dag = HydeNetwork(~PAD|DM + CHD|DM*WC*SBP + DM|rs7903146*SBP*WC + WC|BMI + SBP|BMI)


#The below code sets the distributions and effect sizes of the nodes and edges
t2d_dag = setNode(t2d_dag, PAD, nodeType = "dbern", prob = paste0("ilogit(",.7," * DM + ", -1.339663,")"))
#intercept calculated as log(0.225/(1-0.225)) - (0.7*0.147) = -1.339663

t2d_dag = setNode(t2d_dag, DM, nodeType = "dbern", prob = paste0("ilogit(",0.21," * WC + ", 0.055," * SBP + ", 0.36," * rs7903146 + ", -17.02553,")"))
#intercept calculated as log(0.147/(1-0.147)) - (0.36*0.23 + 0.214*39.6 + 0.055*122) = -17.02553

t2d_dag = setNode(t2d_dag, CHD, nodeType = "dbern", prob = paste0("ilogit(",1," * DM + ", 0.029," * WC + ", 0.027," * SBP + ", -6.139997,")"))
#intercept calculated as log(0.175/(1-0.175)) - (1*0.147 + 0.029*39.6 + 0.027*122) = -6.139997

t2d_dag = setNode(t2d_dag, rs7903146, nodeType = "dbern", prob = paste0("ilogit(", -1.208311,")"))
#intercept calculated as log(0.23/(1-0.23)) = -1.208311

t2d_dag = setNode(t2d_dag, WC, nodeType = "dnorm", mu = paste0(0.821," * BMI + ",39.6 - 0.821*28.7), tau = 0.004109139)
#intercept directly calculated in code

t2d_dag = setNode(t2d_dag, SBP, nodeType = "dnorm", mu = paste0(0.148," * BMI + ",122 - .148*28.7), tau =  0.005102041)
#intercept directly calculated in code

t2d_dag = setNode(t2d_dag, BMI, nodeType = "dnorm", mu = 28.7, tau = 0.04)
#BMI is an independent node, mean is directly input

####

#Below is the following code for running the use case's simulations in the paper####
no_n = 1000000
no_r = 500

#Scenario A, rs7903146's effect on body mass index

#a1
chdpad_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("PAD", "CHD") , sb = NULL , n = no_n)

#a2
naive_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = NULL , sb = NULL , n = no_n)

#a3
wcsbp_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("WC", "SBP") , sb = NULL , n = no_n)

#a4
allbt2d_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("PAD", "CHD", "WC", "SBP") , sb = NULL , n = no_n)

#a5
all_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("DM", "PAD", "CHD", "WC", "SBP") , sb = NULL ,  n = no_n)

#a6
t2d_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = "DM" , sb = NULL , n = no_n)

#a7
chdpadt2d_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("PAD", "CHD", "DM") , sb = NULL , n = no_n)


bmi_results = reparse_runs(list(naive_bmi, t2d_bmi, all_bmi, allbt2d_bmi, wcsbp_bmi, chdpadt2d_bmi, chdpad_bmi), "linear_regression",
                           c("f naive (a2)", "b adjusts t2d (a6)", "c  adjusts all (a5)", "d  adjusts all but T2D (a4)", "e adjusts nodes affecting T2D (a3)", "a adjusts t2d and its children (a7)", "g adjusts for t2d's children (a1)"))

#Create Scernario A's ridgeline plot
ci_ridges(bmi_results)

#Calculate Scerario A's summary statistics
beta_summary(bmi_results)


#Scenario B, rs7903146's effect on bmi

#b1
chdpad_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("PAD", "CHD") , sb = NULL , n = no_n)

#b2
naive_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = NULL , sb = NULL , n = no_n)

#b3
bmisbp_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("BMI", "SBP") , sb = NULL , n = no_n)

#b4
allbt2d_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("PAD", "CHD", "BMI", "SBP") , sb = NULL ,  n = no_n)

#b5
all_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("DM", "PAD", "CHD", "BMI", "SBP") , sb = NULL ,  n = no_n)

#b6
t2d_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = "DM" , sb = NULL ,  n = no_n)

#b7
chdpadt2d_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("PAD", "CHD", "DM") , sb = NULL , n = no_n)

wc_results = reparse_runs(list(naive_wc, t2d_wc, all_wc, allbt2d_wc, bmisbp_wc, chdpadt2d_wc, chdpad_wc), "linear_regression",
                          c("f  naive (b2)", "b adjusts t2d (b6)", "c  adjusts all (b5)", "d  adjusts all but T2D (b4)", "e adjusts nodes affecting T2D (b3)", "a adjusts t2d and its children (b7)", "g adjusts for t2d's children (b1)"))

#Create Scernario B's ridgeline plot
ci_ridges(wc_results)

#Calculate Scerario B's summary statistics
beta_summary(wc_results)


####
