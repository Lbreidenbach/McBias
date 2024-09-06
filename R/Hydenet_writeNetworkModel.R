#' @name writeNetworkModel
#' @export writeNetworkModel
#'
#' @title Generate JAGS Code for a Network's Model
#' @description Based on the parameters given to a network, the code for
#'   each node is generated and all of the node models are pasted into a
#'   single JAGS model script.
#'
#' @param network an object of class \code{HydeNetwork}
#' @param pretty Logical. When \code{TRUE}, the model is printed to the console
#'   using the \code{cat} function (useful if you wish to copy and paste the
#'   code for manual editing).  Otherwise, it is returned as a character
#'   string.
#'
#' @author Jarrod Dalton and Benjamin Nutter
#'
#' @seealso \code{\link{writeJagsModel}}, \code{\link{writeJagsFormula}}
#'
#' @example examples/create_data_example.R
#'

writeNetworkModel <- function(network, pretty=FALSE)
{
  model <- lapply(network[["nodes"]], function(x) writeJagsModel(network, x))
  model <- paste("  ", unlist(model), collapse="\n")
  model <- paste("model{", model, "}\n", sep="\n")

  if (pretty) cat(model) else return(model)
}
