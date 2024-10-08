
#'Create simulated data from a DAG object
#'
#'Creates a single data frame of a set sample size from a DAG object with set distributions
#' @param dag Hydenet directed acyclic graph object (DAG object) with set distributions for each node.
#'
#' @param n Integer, the number of samples in the simulated data frame
#'
#' @param positivity logical. If set to TRUE, checks for positivity violations among binary columns. If violated, it changes the value in the first row of the column to comply with positivity. Defaults to FALSE.
#'
#' @param ... if variables are written into the DAG object, they must all be numerically set here.
#' This allows the user to quickly change DAG values when generating simulated data
#'
#' @return This function returns a simulated data frame. Each column in the data frame corresponds to a node
#' in the DAG object. The data frame will have n rows. Nodes with continuous distributions will have a
#' double class, and nodes with binary distributions will have an integer class.
#'@example examples/create_data_example.R
#'@export
#'

create_data = function(dag, n, positivity = F, ...){
  reclassify = as.integer
  jag_dag = make_model(dag, ...)
  sim_df = bindSim(HydeSim(jag_dag, variable.names = colnames(jag_dag$dag), n.iter = n, bind = FALSE))
  sim_df = sim_df[c(-length(sim_df), -(length(sim_df)-1))]

  relabel = lapply(sim_df, check_integer) # JAGS labels integers as numeric, have to reclassify them
  relabel = relabel[relabel != FALSE]
  relabel = names(relabel)
  sim_df[relabel] = lapply(sim_df[relabel], reclassify)

  sep_check = length(which(duplicated(t(sim_df))==TRUE))
  if(sep_check != 0){
    stop("complete separation occured (one variable completly predicts another). A beta value may be too large, or a sample size may be too small")
  }
  if(positivity==T){
    binary_cols = names(which(lapply(sim_df, is.integer)==TRUE))
    cont_cols = names(which(lapply(sim_df, is.integer)==FALSE))
    bi_sim = sim_df[binary_cols]
    cont_sim = sim_df[cont_cols]
    pos_check = lapply(bi_sim, function(x) sum(x)/length(x))
    all_1s = names(which(pos_check==1))
    bi_sim[1, all_1s]=0
    all_0s = names(which(pos_check==0))
    bi_sim[1, all_0s]=1
    names_order = colnames(sim_df)
    sim_df = cbind(bi_sim, cont_sim)
    sim_df = sim_df[,names_order]
  }

  return(sim_df)
}


