#creates the causal diagram
confounder_dag = HydeNetwork(~B|A + C|A + D|B*C)

#adds set effect sizes and distributions to the causal diagram
confounder_dag = setNode(confounder_dag, A, nodeType = "dbern", prob = 0.5)
confounder_dag = setNode(confounder_dag, B, nodeType = "dnorm", mu = paste0(1," * A + ",0 - 1*0.5), tau = 1)
confounder_dag = setNode(confounder_dag, C, nodeType = "dbern", prob = paste0("ilogit(",0.8," * A + ",set_p(0.1, 0.8*0.5),")"))
confounder_dag = setNode(confounder_dag, D, nodeType = "dbern", prob = paste0("ilogit(",0.05," * B + ", 1.2," * C + ",set_p(0.25, 1.2 * 0.1 + 0.05 * 0 ),")"))

#create data creates a single dataset.
#This lets users see if the simulated datasets behave as expected before committing to a large simulation with many iterations

sim_data = create_data(confounder_dag, 10000)

#outcome must be binary
or_result = odds_ratio(exposure = "A", outcome = "D", covariates = c("B","C"), df = sim_data)
or_result

rr_result = risk_ratio(exposure = "A", outcome = "D", covariates = c("B","C"), df = sim_data)
rr_result

#outcome must be continuous
lm_results = lm_beta(exposure = "C", outcome = "B", covariates = "A", df = sim_data)
lm_results
#outcome may be either
ps_df = get_ps(exposure = "C", covariates = "A", df = sim_data)
ps_results = ps_weight(exposure = "C", outcome = "B", covariates = "A", df = ps_df, weights = "weights")
ps_results

#user can use apply_methods() which will select the appropriate methods based on the outcome's distribution
binary_outcome_result = apply_methods(exposure = "A", outcome = "D", covariates = c("B","C"), df = sim_data)
binary_outcome_result
gaussian_outcome_result = apply_methods(exposure = "C", outcome = "B", covariates = "A", df = sim_data)
gaussian_outcome_result

#user can also input instructions for running MatchIt methods.
#Note these are computationally intensive and may take a while to run especially for data with large N.
#matching methods throw away unmatched controls/ duplicate controls to match to cases, This will be reflected in the n column

matching_results = apply_methods(exposure = "C", outcome = "B", covariates = "A", df = sim_data, ratio = 2, match_methods = c("logit", "Mahalanobis"))
matching_results

#if a continuous exposure is fed into matching methods, the exposure will be dichotimized and a warning will be issued

gaussian_exposure_results = apply_methods(exposure = "B", outcome = "A", covariates = "A", df = sim_data, ratio = 2, match_methods = c("logit", "Mahalanobis"))
gaussian_exposure_results
