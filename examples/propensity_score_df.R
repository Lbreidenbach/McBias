#creates the causal diagram
confounder_dag = HydeNetwork(~B|A + C|A + D|B*C)

#adds set effect sizes and distributions to the causal diagram
confounder_dag = setNode(my_dag, A, nodeType = "dbern", prob = 0.5)
confounder_dag = setNode(my_dag, B, nodeType = "dnorm", mu = paste0(1," * A + ",0 - 1*0.5), tau = 1)
confounder_dag = setNode(my_dag, C, nodeType = "dbern", prob = paste0("ilogit(",0.8," * A + ",set_p(0.1, 0.8*0.5),")"))
confounder_dag = setNode(my_dag, D, nodeType = "dbern", prob = paste0("ilogit(",0.05," * B + ", 1.2," * C + ",set_p(0.25, 1.2 * 0.1 + 0.05 * 0 ),")"))

#create data creates a single dataset.
#This lets users see if the simulated datasets behave as expected before committing to a large simulation with many iterations

sim_data = create_data(confounder_dag, 10000)
lm(B~C, sim_data)

weighted_data = get_ps("C", "A", sim_data)
lm(B~C+weights, weighted_data)

#Node A acts as a confounder in this example. We can see that the when "A" is adjusted on the relationship between "B" and "C" shifts towards 0.
