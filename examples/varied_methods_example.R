#creates the causal diagram
my_dag = HydeNetwork(~B|A + C|A*B + D|B*C)


#if we set up a flexible dag like this, we can quickly change the effect size between nodes A and C as well as the prevalence of node A
flex_dag = function(x,y){
  my_dag = setNode(my_dag, A, nodeType = "dbern", prob = x)
  my_dag = setNode(my_dag, B, nodeType = "dnorm", mu = paste0(0.8," * A + ",0 - 0.8*x), tau = 1)
  my_dag = setNode(my_dag, C, nodeType = "dbern", prob = paste0("ilogit(",y," * A + ",set_p(0.3, y*x),")"))
  my_dag = setNode(my_dag, D, nodeType = "dbern", prob = paste0("ilogit(",0.05," * B + ", 1.2," * C + ",set_p(0.25, 1.2 * 0.9 + 0.05 * 0 ),")"))
}

#To analyze the performance of 1 simulated data frame we can create data and analyze it like so:
sim_data = create_data(flex_dag, 10000,  x=0.5, y = 3)

run_result = apply_methods(exposure = "B", outcome = "C", covariates = "A", df = sim_data)
run_result

#However varied_runs() automatically repeats this process a set number of times and stores all results in a matrix. This allows for MCMC analysis.
small_conf_naive = varied_runs(200, flex_dag, exposure = "B", outcome = "C",  n = 10000, x = .5, y = 1)
small_conf_adj = varied_runs(200, flex_dag, exposure = "B", outcome = "C", covariates = "A",  n = 10000, x = .5, y = 1)
large_conf_naive = varied_runs(200, flex_dag, exposure = "B", outcome = "C",  n = 10000, x = .5, y = 3)
large_conf_adj = varied_runs(200, flex_dag, exposure = "B", outcome = "C", covariates = "A",  n = 10000, x = .5, y = 3)

#you can use reparse_runs() to combine matrices of different runs
confounder_comp = reparse_runs(list(small_conf_naive, small_conf_adj, large_conf_naive, large_conf_adj), list_names =  c("A->C = 1, naive", "A->C = 1, adjusted", "A->C = 3, naive", "A->C = 3, adjusted"))
#ci_ridges lets you quickly visualize results
ci_ridges(confounder_comp, title = "How well is the confounder adjusted for?", subtitle = "Comparison of confounder adjustment methods across different confounder strengths")
#beta_summary() will return summary statistics about the bias present in each scenario
beta_summary(confounder_comp)

#You can also use reparse_runs() to only look at one method in particular.
logistic_comp = reparse_runs(list(small_conf_naive, small_conf_adj, large_conf_naive, large_conf_adj), method = "regression", list_names =  c("A->C = 1, naive", "A->C = 1, confounder adjusted", "A->C = 3, naive", "A->C = 3, confounder adjusted"))
ci_ridges(logistic_comp, "How well is the confounder adjusted for?", subtitle = "Comparison of logistic regression across different confounder strengths")


#if a beta value between 2 binary nodes is too large, or the sample size is too small, one node may become completely predictive of another which violates separation.
#An error will occur if this is the case:

# very_large_conf = varied_runs(200, flex_dag, exposure = "B", outcome = "C",  n = 10000, x = .5, y = 100)
