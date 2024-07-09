#creates the causal diagram
my_dag = HydeNetwork(~B|A + C|A + D|B*C)

#adds set effect sizes and distributions to the causal diagram
my_dag = setNode(my_dag, A, nodeType = "dbern", prob = 0.5)
my_dag = setNode(my_dag, B, nodeType = "dnorm", mu = paste0(0.2," * A + ",0 - 0.2*0.5), tau = 1)
my_dag = setNode(my_dag, C, nodeType = "dbern", prob = paste0("ilogit(",0.8," * A + ",set_p(0.9, 0.8*0.5),")"))
my_dag = setNode(my_dag, D, nodeType = "dbern", prob = paste0("ilogit(",0.05," * B + ", 1.2," * C + ",set_p(0.25, 1.2 * 0.9 + 0.05 * 0 ),")"))

#create data creates a single dataset.
#This lets users see if the simulated datasets behave as expected before committing to a large simulation with many iterations

sim_data = create_data(my_dag, 10000)
head(sim_data)

#below we set up the DAG so we can quickly change the set prevalence of node A (arg x), and the effect size between A and B (arg y)
flex_dag = function(x,y){
  my_dag = setNode(my_dag, A, nodeType = "dbern", prob = x)
  my_dag = setNode(my_dag, B, nodeType = "dnorm", mu = paste0(y," * A + ",0 - y*x), tau = 1)
  my_dag = setNode(my_dag, C, nodeType = "dbern", prob = paste0("ilogit(",0.8," * A + ",set_p(0.9, 0.8*x),")"))
  my_dag = setNode(my_dag, D, nodeType = "dbern", prob = paste0("ilogit(",0.05," * B + ", 1.2," * C + ",set_p(0.25, 1.2 * 0.9 + 0.05 * 0 ),")"))
}


sim_data_1 = create_data(flex_dag, 10000, x = .2, y = 1)
x_prev_1 = sum(sim_data_1$A)/10000
y_effect_estimate_1 =lm(B~A, sim_data_1)$coefficients[2]
x_prev_1
y_effect_estimate_1

sim_data_2 = create_data(flex_dag, 10000, x = .7, y = .5)
x_prev_2 = sum(sim_data_2$A)/10000
y_effect_estimate_2 =lm(B~A, sim_data_2)$coefficients[2]
x_prev_2
y_effect_estimate_2

#If positivity = TRUE the program will make sure each binary column has both 0s and 1s
positivity_violation_1 = create_data(flex_dag, 10, x = .000001, y = 1)
positivity_restored_1 = create_data(flex_dag, 10, positivity = T, x = .00001, y = 1)
positivity_violation_1
positivity_restored_1

positivity_violation_2 = create_data(flex_dag, 10, x = .999999, y = 1)
positivity_restored_2 = create_data(flex_dag, 10, positivity = T, x = .99999, y = 1)
positivity_violation_2
positivity_restored_2

