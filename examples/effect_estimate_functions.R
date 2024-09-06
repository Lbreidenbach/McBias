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
rr_result = risk_ratio(exposure = "A", outcome = "D", covariates = c("B","C"), df = sim_data)

#outcome must be continuous
lm_results = lm_beta(exposure = "C", outcome = "B", covariates = "A", df = sim_data)

#outcome may be either
ps_df = get_ps(exposure = "C", covariates = "A", df = sim_data)
ps_results = ps_weight(exposure = "C", outcome = "B", covariates = "A", df = ps_df, weights = "weights")
