#' @noRd

make_model = function(dag, ...){
  require(HydeNet)
  arg_list = list(...)
  if(class(dag)== "HydeNetwork"){
    dag_1 = dag
  }else if(length(arg_list)==0){
    dag_1 = dag()

  }else{
    dag_1 = do.call(dag, arg_list)
  }

  HydeNet::writeNetworkModel(dag_1, pretty = TRUE)
  comp_dag = HydeNet::compileJagsModel(dag_1)
  return(comp_dag)
}
