#Below is the following code run for the use case in the paper



no_n = 1000000
no_r = 500
#outcome CHD
naive_chd = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "CHD" , covariates = NULL , sb = NULL , n = no_n)
t2d_chd = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "CHD" , covariates = "DM" , sb = NULL ,  n = no_n)
all_chd = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "CHD" , covariates = c("DM", "PAD", "BMI", "WC", "SBP") , sb = NULL ,  n = no_n)
allbt2d_chd = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "CHD" , covariates = c("PAD", "BMI", "WC", "SBP") , sb = NULL ,  n = no_n)
bmiwcsbp_chd = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "CHD" , covariates = c("BMI", "WC", "SBP") , sb = NULL ,  n = no_n)
padt2d_chd = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "CHD" , covariates = c("PAD", "DM") , sb = NULL ,  n = no_n)
pad_chd = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "CHD" , covariates = c("PAD") , sb = NULL , n = no_n)

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
naive_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = NULL , sb = NULL ,  n = no_n)
t2d_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = "DM" , sb = NULL , n = no_n)
all_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("DM", "SBP", "CHD", "WC", "BMI") , sb = NULL ,  n = no_n)
allbt2d_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("SBP", "CHD", "WC", "BMI") , sb = NULL ,  n = no_n)
bmiwcsbp_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("BMI", "WC", "SBP") , sb = NULL ,  n = no_n)
chdt2d_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("CHD", "DM") , sb = NULL , n = no_n)
chd_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("CHD") , sb = NULL , n = no_n)

pad_results = reparse_runs(list(naive_pad, t2d_pad, all_pad, allbt2d_pad, bmiwcsbp_pad, chdt2d_pad, chd_pad), "logistic_or",
                           c("e  naive", "a adjusts t2d", "b  adjusts all", "f  adjusts all but T2D", "g adjusts nodes affecting T2D", "c adjusts t2d and its children", "d adjusts for t2d's children"))


#outcome bmi
naive_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = NULL , sb = NULL , n = no_n)
t2d_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = "DM" , sb = NULL , n = no_n)
all_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("DM", "PAD", "CHD", "WC", "SBP") , sb = NULL ,  n = no_n)
allbt2d_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("PAD", "CHD", "WC", "SBP") , sb = NULL , n = no_n)
wcsbp_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("WC", "SBP") , sb = NULL , n = no_n)
chdpadt2d_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("PAD", "CHD", "DM") , sb = NULL , n = no_n)
chdpad_bmi = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "BMI" , covariates = c("PAD", "CHD") , sb = NULL , n = no_n)

bmi_results = reparse_runs(list(naive_bmi, t2d_bmi, all_bmi, allbt2d_bmi, wcsbp_bmi, chdpadt2d_bmi, chdpad_bmi), "linear_regression",
                           c("f naive", "b adjusts t2d", "c  adjusts all", "d  adjusts all but T2D", "e adjusts nodes affecting T2D", "a adjusts t2d and its children", "g adjusts for t2d's children"))


#outcome wc
naive_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = NULL , sb = NULL , n = no_n)
t2d_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = "DM" , sb = NULL ,  n = no_n)
all_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("DM", "PAD", "CHD", "BMI", "SBP") , sb = NULL ,  n = no_n)
allbt2d_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("PAD", "CHD", "BMI", "SBP") , sb = NULL ,  n = no_n)
bmisbp_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("BMI", "SBP") , sb = NULL , n = no_n)
chdpadt2d_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("PAD", "CHD", "DM") , sb = NULL , n = no_n)
chdpad_wc = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "WC" , covariates = c("PAD", "CHD") , sb = NULL , n = no_n)

wc_results = reparse_runs(list(naive_wc, t2d_wc, all_wc, allbt2d_wc, bmisbp_wc, chdpadt2d_wc, chdpad_wc), "linear_regression",
                          c("f  naive", "b adjusts t2d", "c  adjusts all", "d  adjusts all but T2D", "e adjusts nodes affecting T2D", "a adjusts t2d and its children", "g adjusts for t2d's children"))

#outcome sbp
naive_sbp = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "SBP" , covariates = NULL , sb = NULL , ate = 0, n = no_n)
t2d_sbp = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "SBP" , covariates = "DM" , sb = NULL , ate = 0, n = no_n)
all_sbp = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "SBP" , covariates = c("DM", "PAD", "CHD", "WC", "BMI") , sb = NULL , ate = 0, n = no_n)
allbt2d_sbp = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "SBP" , covariates = c("PAD", "CHD", "WC", "BMI") , sb = NULL , ate = 0, n = no_n)
bmiwc_sbp = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "SBP" , covariates = c("BMI", "WC") , sb = NULL , ate = 0, n = no_n)
chdpadt2d_sbp = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "SBP" , covariates = c("PAD", "CHD", "DM") , sb = NULL , ate = 0, n = no_n)
chdpad_sbp = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "SBP" , covariates = c("PAD", "CHD") , sb = NULL , ate = 0, n = no_n)

sbp_results = reparse_runs(list(naive_sbp, t2d_sbp, all_sbp, allbt2d_sbp, bmiwc_sbp, chdpadt2d_sbp, chdpad_sbp), "linear_regression",
                           c("f  naive", "b adjusts t2d", "c  adjusts all", "d  adjusts all but T2D", "e adjusts nodes affecting T2D", "a adjusts t2d and its children", "g adjusts for t2d's children"))


#outcome pad
naive_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = NULL , sb = NULL , ate = 0, n = no_n)
t2d_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = "DM" , sb = NULL , ate = 0, n = no_n)
all_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("DM", "SBP", "CHD", "WC", "BMI") , sb = NULL , ate = 0, n = no_n)
allbt2d_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("SBP", "CHD", "WC", "BMI") , sb = NULL , ate = 0, n = no_n)
bmiwcsbp_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("BMI", "WC", "SBP") , sb = NULL , ate = 0, n = no_n)
chdt2d_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("CHD", "DM") , sb = NULL , ate = 0, n = no_n)
chd_pad = varied_runs(no_r, t2d_dag, exposure = "rs7903146" , outcome = "PAD" , covariates = c("CHD") , sb = NULL , ate = 0, n = no_n)

pad_results = reparse_runs(list(naive_pad, t2d_pad, all_pad, allbt2d_pad, bmiwcsbp_pad, chdt2d_pad, chd_pad), "logistic_or",
                           c("a  naive", "b adjusts t2d", "c  adjusts all", "d  adjusts all but T2D", "e adjusts nodes affecting T2D", "f adjusts t2d and its children", "g adjusts for t2d's children"))

