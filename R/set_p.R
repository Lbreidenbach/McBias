#' Set the prevalence of a dependent binary node in a regression equation
#' @param p A double value between 0 and 1 that represents the desired prevalence of a binary node.
#' @param model The rest of the regression model the node is dependent on
#' @returns A constant value that acts as the intercept in the regression equation.
#' @example McBias/examples/Dag_example.R
#' @seealso [Hydenet::setNode()]
#' @export


set_p = function(p,model){
  p2 = log(p/(1-p))
  b0 = p2-model
  return(b0)
}
