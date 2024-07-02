
#'Create simulated data from a DAG object
#'
#'Creates a single data frame of a set sample size from a DAG object with set distributions
#' @param dag Hydenet directed acyclic graph object (DAG object) with set distributions for each node.
#'
#' @param n Integer, the number of samples in the simulated data frame
#'
#' @param ... if variables are written into the DAG object, they must all be numerically set here.
#' This allows the user to quickly change DAG values when generating simulated data
#'
#' @return This function returns a simulated data frame. Each column in the data frame corresponds to a node
#' in the DAG object. The data frame will have n rows. Nodes with continuous distributions will have a
#' double class, and nodes with binary distributions will have an integer class.
#'@seealso [Hydenet::HydeNetwork()],[Hydenet::setNode()]
#'@examples McBias/examples/make_model_create_data.R

#'@export
#'
create_data = function(dag, n, ...){
  require(HydeNet)
  reclassify = as.integer
  jag_dag = make_model(dag, ...)
  sim_df = HydeNet::bindSim(HydeNet::HydeSim(jag_dag, variable.names = colnames(jag_dag$dag), n.iter = n, bind = FALSE))
  relabel = lapply(sim_df, check_integer) # JAGS labels integers as numeric, have to reclassify them
  relabel = relabel[relabel != FALSE]
  relabel = names(relabel)
  sim_df[relabel] = lapply(sim_df[relabel], reclassify)
  sim_df = sim_df[c(-length(sim_df), -(length(sim_df)-1))]
  return(sim_df)
}
