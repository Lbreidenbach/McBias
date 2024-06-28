
#'Create simulated data from a defined directed acyclic graph
#'
#' @param dag Hydenet directed acyclic graph object (DAG object)
#'
#' @param n Integer, the number of samples in the simulated dataset
#'
#' @param ... if variables are written into the DAG object, they must all be numerically set here.
#' This allows the user to quickly change DAG values when generating simulated data
#'
#' @return This function returns a simulated data frame. Each column in the data frame corresponds to a node
#' in the DAG object. The data frame will have n rows. Nodes with continuous distributions will have a
#' double class, and nodes with binary distributions will have an integer class.
#'
#'@examples McBias/examples/make_model_create_data.R
#'@seealso [make_model()]
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
