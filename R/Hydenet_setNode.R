#' @name setNode
#' @export setNode
#' @importFrom plyr is.formula
#'
#' @title Set Node Relationships
#' @description The relationship between a node and its parents must be defined
#'   before the appropriate JAGS model statement can be constructed.
#'   \code{setNode} is the utility by which a user can define the distribution
#'   of the node and its relationship to its parents (usually through a model
#'   of some sort).
#'
#' @param network A \code{HydeNetwork}.
#' @param node A node within \code{network}.  This does not have to be quoted.
#' @param nodeType a valid distribution.  See the data set
#'   in \code{data(jagsDists)} for a complete list of available distributions.
#'   See "Choosing a Node Type"
#' @param nodeFitter the fitting function, such as \code{lm} or \code{glm}.  This
#'   will probably only be needed when \code{fromData = TRUE}.
#' @param nodeFormula A formula object specifying the relationship between a
#'   node and its parents.  It must use as a term every parent of \code{node}. This formula
#'   will be pushed through the unexported function \code{factorFormula}.  See
#'   "Coding Factor Levels" for more details.
#' @param fitterArgs Additional arguments to be passed to \code{fitter}.
#' @param decision A value of either \code{"current"} or a logical value.
#'   If \code{"current"}, the current value of the setting is retained.  This allows
#'   decision nodes set by \code{setDecisionNode} to retain the classification as a
#'   decision node if \code{setNode} is run after \code{setDecisionNode}.
#'   If \code{TRUE}, the node will be considered a
#'   decision node in \code{compileDecisionNetwork}.  This is only a valid
#'   option when the node is of type \code{"dbern"} or \code{"dcat"}. Note: if any
#'   character value other than \code{"current"} is given, \code{setNode} will assume
#'   you intended \code{"current"}.
#' @param utility A value of either \code{"current"} or a logical value.
#'   If \code{"current"}, the current value of the setting is retained.  This allows
#'   utility nodes set by \code{setUtilityNode} to retain the classification as a
#'   utility node if \code{setNode} is run after \code{setUtilityNode}.
#'   If \code{TRUE}, the node will be considered a
#'   utility node.  This is only a valid option when the node is of type
#'   \code{"determ"} and it has no children.
#'   Note: if any
#'   character value other than \code{"current"} is given, \code{setNode} will assume
#'   you intended \code{"current"}.
#' @param fromData Logical.  Determines if a node's relationship is calculated
#'   from the data object in \code{network}.  Defaults to \code{TRUE} whenever
#'   \code{network} has a data object.
#' @param ... parameters to be passed to the JAGS distribution function.  Each parameter
#'   in the distribution function must be named.  For
#'   example, the parameters to pass to \code{dnorm} would be \code{mean='', sd=''}.
#'   The required parameters can be looked up using the
#'   \code{expectedParameters} function.  If parameters are to be estimated
#'   from the data, the functions \code{fromData} and \code{fromFormula} may
#'   be used as placeholders.
#' @param nodeData A data frame with the appropriate data to fit the model for the node.
#'   Data passed in this argument are applied only to this specific node.  No checks are
#'   performed to ensure that all of the appropriate variables (the node and its parents)
#'   are included.
#' @param factorLevels A character vector used to specify the levels of factors
#'   when data are not provided for a node.  The order of factors follows the
#'   order provided by the user.  This argument is only used when the node type
#'   is either \code{dcat} or \code{dbern}, the node Fitter is not \code{cpt},
#'   \code{nodeData} is \code{NULL}, and no variable for the node exists in
#'   the network's \code{data} element.  If any of those conditions is not met,
#'   \code{factorLevels} is ignored.  This proves particularly important when
#'   data are specified in order to prevent a user specification from conflicting
#'   with expected factors across nodes.
#' @param validate Logical.  Toggles validation of parameters given in \code{...}.
#'   When passing raw JAGS code (ie, character strings), this will be ignored
#'   (with a message),
#'   as the validation is applicable to numerical/formula values.
#' @param fitModel Logical. Toggles if the model is fit within the function call.
#'   This may be set globally using \code{options('Hyde_fitModel')}.  See Details
#'   for more about when to use this option.
#' @param policyValues A vector of values to be used in the policy matrix when
#'   the node is decision node.  This may be left \code{NULL} for factor
#'   variables, which will then draw on the factor levels.  For numerical
#'   variables, it can be more important: if left \code{NULL} and data are
#'   available for the node, the first, second, and third quartiles will
#'   be used to populate the policy values.  If no data are available and no
#'   values are provided, \code{policyMatrix} and \code{compileDecisionModel}
#'   are likely to return errors when they are called.  Policy values may
#'   also be set with \code{setPolicyValues} after a network has been defined.
#'
#' @details
#'   The functions \code{fromFormula()} and \code{fromData()} help to control
#'   how \code{Hyde} determines the values of parameters passed to JAGS.  If the
#'   parameters passed in \code{params} argument are to be calculated from the
#'   data or inferred from the formula, these functions may be used as placeholders
#'   instead of writing JAGS code in the \code{params} argument.
#'
#'   By default, \code{options(Hyde_fitModel=FALSE)}.  This prevents \code{setNode}
#'   from fitting any models.  Instead, the fitting is delayed until the user
#'   calls \code{writeJagsModel} and all of the models are fit at the same time.
#'   When using large data sets that may require time to run, it may be better to
#'   leave this option \code{FALSE} so that the models can all be compiled together
#'   (especially if you are working interactively).  Using \code{fitModel=TRUE}
#'   will cause the model to be fit and the JAGS code for the parameters to be
#'   stored in the \code{nodeParams} attribute.
#'
#' @return
#' Returns the modified \code{HydeNetwork} object.
#'
#' @section Choosing a Node Type:
#' Many of the distribution functions defined in JAGS have an equivalen
#' distribution function in R.  You may inspect the \code{jagsDists} data
#' frame to see the function names in each language.  You may specify
#' the distribution function using the R name and it will be translated
#' to the equivalent JAGS function.
#'
#' You may still use the JAGS names, which allows you to specify a
#' distribution in JAGS that does not have an R equivalent listed. Note,
#' however, that where R functions are supported, \code{McBias} anticipates
#' the parameter names to be given following R conventions (See
#' the \code{RParameter} column of \code{jagsDists}.)
#'
#' Of particular interest are \code{dbern} and \code{dcat}, which are
#' functions in JAGS that have no immediate equivalent in R. They provide
#' Bernoulli and Multinomial distributions, respectively.
#'
#' @section Coding Factor Levels:
#' The \code{nodeFormula} argument will accept any valid R formula.  If desired, you
#' may use a specific formulation to indicate the presence of factor levels in the
#' formula.  For instance, consider the case of a variable \code{y} with a binary
#' categorical parent \code{x} coded as 0 = No, and 1 = Yes.  JAGS expects the
#' formula \code{y ~ c * x == 1} (where \code{c} is a constant).  However, in
#' factor variables with a large number of levels, it can be difficult to remember
#' what value corresponds to what level.
#'
#' \code{McBias} uses an internal (unexported) function within \code{setNode} to allow
#' an alternate specification: \code{y ~ c * (x == "Yes")}.  So long as the factors in
#' the formula are previously defined within the network structure, \code{McBias}
#' will translate the level into its numeric code.
#'
#' Note that it is required to write \code{x == "Yes"}.  \code{"Yes" == x} will not
#' translate correctly.
#'
#' @section Validation:
#' The validation of parameters is performed by comparing the values provided with
#' the limits defined in the \code{jagsDists$paramLogic} variable. (look at
#' \code{data(jagsDists, data='McBias')}.  For most node types, validation will
#' be performed for numeric variables.  For deterministic variables, the validation
#' will only check that the parameter definition is a formula.
#'
#' It is possible to pass character strings as definitions, but when this is done,
#' \code{McBias} assumes you are passing JAGS code.  Unfortunately, \code{McBias}
#' doesn't have to capability to validate JAGS code, so if there is an error in
#' the character string definition, it won't show up until you try to compile the
#' network.  If you pass a character string as a parameter and leave
#' \code{validate = TRUE}, a message will be printed to indicate that validation
#' is being ignored.  This message can be avoided by using \code{validate = FALSE}
#'
#' The two exceptions to this rule are when you pass \code{fromFormula()} and
#' \code{fromData()} as the parameter definition.  These will skip the validation
#' without warning, since the definition will be built by \code{McBias} and be
#' proper JAGS code (barring any bugs, of course).
#'
#' @author Jarrod Dalton and Benjamin Nutter
#'
#' @example examples/Dag_example.R
setNode <- function(network, node, nodeType,
                    nodeFitter, nodeFormula,
                    fitterArgs = list(),
                    decision = "current",
                    utility = "current",
                    fromData=!is.null(network$data), ...,
                    nodeData = NULL, factorLevels = NULL,
                    validate=TRUE, fitModel=getOption("Hyde_fitModel", default = FALSE),
                    policyValues = factorLevels)
{

  network.t <- as.character(substitute(network))
  node.t <- as.character(substitute(node))

  coll <- checkmate::makeAssertCollection()

  if (is.character(decision))
  {
    if (decision != "current")
    {
      warning("'decision' must be logical or 'current'.  You provided ",
              "an unrecognized character value.  'HydeNet' is assuming you mean ",
              "'current'.")
      decision <- "current"
    }
    decision <- network[["nodeDecision"]][[node.t]]
  }

  if (is.character(utility))
  {
    if (utility != "current")
    {
      warning("'utility' must be logical or 'current'.  You provided ",
              "an unrecognized character value.  'HydeNet' is assuming you mean ",
              "'current'.")
      utility <- "current"
    }
    utility <- network[["nodeUtility"]][[node.t]]
  }

  if (missing(nodeType))
  {
    nodeType <- network[["nodeType"]][[node.t]]
  }
  else
  {
    if (length(nodeType) > 1)
    {
      warning("nodeType must have length 1. The first element is being used.")
      nodeType <- nodeType[1]
    }
  }

  checkmate::assertSubset(x = nodeType,
                          choices = jagsDists[["FnName"]],
                          add = coll)

  if (!missing(nodeType)) network[["nodeType"]][[node.t]] <- nodeType
  exp_param <- eval(substitute(expectedParameters(network = network,
                                                  node = node,
                                                  returnVector = TRUE)))

  params <- list(...)

  names(params)[!exp_param %in% names(params)] <-
    exp_param[!exp_param %in% names(params)]


  params <- params[exp_param]

  # JAGS dnorm expects tau = 1/variance.
  # HydeNet accepts sd, so we need to transform this.
  if (nodeType == "dnorm")
  {
    if ("sd" %in% names(params))
      if (is.numeric(params[["sd"]]))
        params[["sd"]] <- 1 / params[["sd"]]^2
  }

  checkmate::assertSubset(x = exp_param,
                          choices = names(params),
                          add = coll)
  orig_names <- names(params)

  names(params) <-
    jagsDists[["Parameters"]][jagsDists$FnName == nodeType &
                                jagsDists$RParameter %in% names(params)]

  if (validate){
    valid <- validateParameters(params, network[["nodeType"]][[node.t]])

    if (any(sapply(params, is.character) &
            !sapply(params, function(p) p %in% c("fromData", "fromFormula"))))
    {
      message("Validation has been ignored for parameters defined with character strings")
      valid[sapply(params, is.character)] <- TRUE
    }

    if (!all(valid))
    {
      not_valid <- which(!valid)
      msg <- paste0("Please define ", orig_names[not_valid], " such that ", names(valid)[not_valid],
                    " (or use validate=FALSE).")
      msg <- paste(msg, collapse="\n")
      coll$push(msg)
    }
  }

  if (decision){
    if (!nodeType %in% c("dbern", "dcat")){
      warning("Only nodes of type 'dbern' and 'dcat' may be decision nodes. ",
              "'decision' has been set to FALSE")
      decision <- FALSE
    }
  }

  if (utility){
    checkmate::assertSubset(x = nodeType,
                            choices = "determ",
                            add = coll)

    if (any(sapply(X = network$parents,
                   FUN = function(p, t) t %in% p, node.t)))
      coll$push("Utility nodes may not have children.")

  }

  if (length(list(...)))
  {
    network[["nodeParams"]][[node.t]] <- list(...)
  }

  if (!missing(nodeFormula))
  {
    network[["nodeFormula"]][[node.t]] <-
      factorFormula(nodeFormula, network)
  }

  if (!missing(nodeFitter))
  {
    network[["nodeFitter"]][[node.t]] <- nodeFitter
  }

  if (length(fitterArgs)){
    network[["nodeFitterArgs"]][[node.t]] <- fitterArgs
  }

  if (!is.null(nodeData))
  {
    network[["nodeData"]][[node.t]] <- nodeData
  }

  if (!is.null(factorLevels))
  {
    nodeFitter <-
      if (is.null(network$nodeFitter[[node.t]]))
      {
        ""
      }
    else
    {
      network$nodeFitter[[node.t]]
    }

    if (!(network[["nodeType"]][[node.t]] %in% c("dcat", "dbern")) ||
        nodeFitter == "cpt" ||
        !is.null(network[["nodeData"]][[node.t]]) ||
        (node.t %in% names(network[["data"]])))
    {
      warning("'", node.t, "' does not satisfy the conditions ",
              "to use 'factorLevels'.  See '?setNode' for details.")

      if (nodeFitter == "cpt")
      {
        if (!is.null(network[["nodeData"]][[node.t]]))
        {
          network[["factorLevels"]][[node.t]] <-
            if (!is.factor(network[["nodeData"]][[node.t]][[node.t]]))
            {
              sort(unique(network[["nodeData"]][[node.t]][[node.t]]))
            }
          else
          {
            levels(network[["nodeData"]][[node.t]][[node.t]])
          }
        }
        else if (!is.null(network[["data"]][[node.t]]))
        {
          network[["factorLevels"]][[node.t]] <-
            if (!is.factor(network[["data"]][[node.t]]))
            {
              sort(unique(network[["data"]][[node.t]]))
            }
          else
          {
            levels(network[["data"]][[node.t]])
          }
        }
      }
    }
    else
    {
      network[["factorLevels"]][[node.t]] <- factorLevels
    }
  }
  else{
    if (!is.null(network[["nodeData"]][[node.t]]))
    {
      network[["factorLevels"]][[node.t]] <-
        levels(network[["nodeData"]][[node.t]][[node.t]])
    }
    else if (!is.null(network[["data"]]))
    {
      network[["factorLevels"]][[node.t]] <-
        levels(network[["data"]][[node.t]])
    }
    else
    {
      network[["factorLevels"]][[node.t]] <- NULL
    }
  }
  network[["nodeDecision"]][[node.t]] <- decision
  network[["nodeUtility"]][[node.t]] <- utility

  network[["nodePolicyValues"]][[node.t]] <- policyValues

  if (fitModel)
  {
    fit <- do.call(network[["nodeFitter"]][[node.t]],
                   c(list(formula = network[["nodeFormula"]][[node.t]],
                          data =
                            if (is.null(network[["nodeData"]][[node.t]]))
                            {
                              network[["data"]]
                            }
                          else
                          {
                            network[["nodeData"]][[node.t]]
                          }
                   ),
                   network[["nodeFitterArgs"]][[node.t]]
                   )
    )
    network[["nodeModel"]][[node.t]] <- fit

    if (network[["nodeType"]][[node.t]] == "dbern")
    {
      network[["nodeParams"]][[node.t]][["p"]] <-
        writeJagsFormula(fit, network[["nodes"]])
    }
    else if (network[["nodeType"]][[node.t]] == "dcat")
    {
      network[["nodeParams"]][[node.t]][["pi"]] <-
        writeJagsFormula(fit, network[["nodes"]])
    }
    else if (network$nodeType[[node.t]] == "dnorm")
    {
      network[["nodeParams"]][[node.t]][["mu"]] <-
        writeJagsFormula(fit, network[["nodes"]])
      network[["nodeParams"]][[node.t]][["tau"]] <- 1/summary(fit)[["sigma"]]
    }
    else if (network[["nodeType"]][[node.t]] == "dpois")
    {
      network[["nodeParams"]][[node.t]][["lambda"]] <-
        writeJagsFormula(fit, network[["nodes"]])
    }
  }


  checkmate::reportAssertions(coll)
  return(network)
}

#' @rdname setNode
#' @export fromData

fromData <- function(){ "fromData" }

#' @rdname setNode
#' @export fromFormula

fromFormula <- function(){ "fromFormula" }
