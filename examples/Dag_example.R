#creates the causal diagram
my_dag = HydeNetwork(~B|A + C|A + D|B*C)

#adds set effect sizes and distributions to the causal diagram
my_dag = setNode(my_dag, A, nodeType = "dbern", prob = 0.5)
my_dag = setNode(my_dag, B, nodeType = "dnorm", mu = paste0(0.2," * A + ",0 - 0.2*0.5), tau = 1)
my_dag = setNode(my_dag, C, nodeType = "dbern", prob = paste0("ilogit(",0.8," * A + ",set_p(0.9, 0.8*0.5),")"))
my_dag = setNode(my_dag, D, nodeType = "dbern", prob = paste0("ilogit(",0.05," * B + ", 1.2," * C + ",set_p(0.25, 1.2 * 0.9 + 0.05 * 0 ),")"))

#ScenarioMatrix object
ideal_run = varied_runs(runs = 100, dag = my_dag, exposure = "B", outcome = "C", covariates = "A", n = 1000)

#Gives a dataframe of values for quantifying the bias and its effects on the reject null rate
beta_summary(ideal_run)

#Creates a ridgeline plot to help with bias and coverage visualization
ci_ridges(ideal_run)
