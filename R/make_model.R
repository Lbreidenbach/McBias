#' @noRd

make_model = function(dag,
                      seed = NULL,
                      .RNG.name = "base::Mersenne-Twister", ...){
  arg_list = list(...)
  if(class(dag)== "HydeNetwork"){
    dag_1 = dag
  }else if(length(arg_list)==0){
    dag_1 = dag()

  }else{
    dag_1 = do.call(dag, arg_list)
  }

  #writeNetworkModel(dag_1, pretty = TRUE)
  if(is.null(seed)){
    comp_dag = compileJagsModel(dag_1)
  }else{
    comp_dag = compileJagsModel(dag_1,inits=list(.RNG.name=.RNG.name, .RNG.seed=seed))
  }

  return(comp_dag)
}
