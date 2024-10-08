#' @name expectedVariables
#' @export expectedVariables
#'
#' @title List Expected Parameter Names and Expected Variables Names
#' @description To assist in formula that defines the relationship to a node,
#'   \code{expectedVariables} returns to the console
#'   a sample string that can be pasted into \code{setNode}
#'   and populated with the desired coefficients.
#'
#' @param network A \code{HydeNetwork} object.
#' @param node A node name within \code{network}
#' @param returnVector Logical.  If FALSE, the sample string for use in
#'   \code{setNode} is returned.  Otherwise, the vector of parent names
#'   is returned.
#'
#' @details Each node is calculated as a model of its parents.  If no
#' training data are provided to the network, the user is expected to provide
#' appropriate estimates of the regression coefficients for the model.
#'
#' \code{returnVector} will generally be set to \code{FALSE} for most uses,
#' but can be set to \code{TRUE} for use in error checking.  For example,
#' in \code{setNode}, if not all of the parents have been given a coefficient
#' (or if too few coefficients have been given), the vector of names is supplied.
#'
#' @author Jarrod Dalton and Benjamin Nutter
#' @example examples/Dag_example.R

expectedVariables <- function(network, node, returnVector=FALSE)
{
  node <- as.character(substitute(node))
  inputs <- network[["parents"]][[node]]

  if (returnVector)
  {
    return(inputs)
  }

  if (is.null(inputs))
  {
    cat(paste(node, "~ 1"))
  }
  else
  {
    cat(paste(node, "~", paste(inputs, collapse=" + ")))
  }
}

#' @rdname expectedVariables
#' @export expectedParameters

expectedParameters <- function(network, node, returnVector=FALSE)
{
  node <- as.character(substitute(node))
  inputs <- network[["nodeType"]][[node]]

  params <- jagsDists[["RParameter"]][jagsDists[["FnName"]] == inputs]

  names(params) <- jagsDists[["Parameters"]][jagsDists[["FnName"]] == inputs]

  if (returnVector)
  {
    return(params)
  }
  else
  {
    cat(
      paste(
        paste(
          paste0(params, "= "),
          collapse=", "
        )
      )
    )
  }
}


expectedParameters_ <- function(network, node, returnVector=FALSE)
{
  inputs <- network[["nodeType"]][[node]]

  params <- jagsDists[["RParameter"]][jagsDists[["FnName"]] == inputs]

  names(params) <- jagsDists[["Parameters"]][jagsDists[["FnName"]] == inputs]

  if (returnVector)
  {
    return(params)
  }
  else
  {
    cat(
      paste(
        paste(
          paste0(params, "= "),
          collapse=", "
        )
      )
    )
  }
}
