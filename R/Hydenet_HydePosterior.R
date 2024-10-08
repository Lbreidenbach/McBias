#' @name HydeSim
#' @export HydeSim
#'
#' @title Simulated Distributions of a Decision Network
#' @description The simulated distributions of the decision network can be
#'   evaluated to determine the probabilistic outcomes based on the decision
#'   inputs in the model as well as subject specific factors.
#'
#' @param cHN A \code{compiledHydeNetwork} object as returned by
#'   \code{compileJagsNetwork}.
#' @param variable.names a character vector giving the names of variables to be monitored.
#' @param n.iter number of iterations to monitor.
#' @param thin thinning interval for monitors.
#' @param ... options arguments that are passed to the update method for
#'   jags model objects.
#' @param monitor_observed If TRUE, the observed or fixed variables (those
#'   passed to the \code{data} argument in \code{compileJagsNetwork}) are
#'   forced into \code{variable.names} if not already provided.  This is
#'   recommended, especially if you will be binding multiple JAGS runs
#'   together.
#' @param bind Logical. If \code{TRUE}, simulated distributions will be bound into
#'   a single data frame.  If \code{FALSE}, the standard output from \code{rjags}
#'   is returned.
#'
#' @details This is essentially a wrapper around \code{coda.samples} that
#'   returns in a list the output for each run of \code{coda.samples} over
#'   the rows of the policy/decision matrix given in the \code{data} argument
#'   of \code{compileJagsNetwork}.
#'
#' @return A list of class \code{HydeSim} with elements \code{codas}
#'   (the MCMC matrices from \code{coda.samples}), \code{observed} (the values
#'   of the variables that were observed), \code{dag} (the dag object for
#'   convenience in displaying the network), and \code{factorRef} (giving the
#'   mappings of factor levels to factor variables).
#'
#' @author Jarrod Dalton and Benjamin Nutter
#'
#' @example examples/create_data_example.R

HydeSim <- function(cHN, variable.names, n.iter, thin=1, ...,
                    monitor_observed=TRUE, bind=TRUE)
{
  if (monitor_observed)
  {
    variable.names <-
      if (class(cHN[["jags"]]) == "jags")
      {
        unique(c(variable.names, names(cHN$observed)))
      }
    else
    {
      unique(c(variable.names, names(cHN[[1]]$observed)))
    }
  }



  if (class(cHN$jags) == "jags")
  {
    codas <- rjags::coda.samples(model = cHN[["jags"]],
                                 variable.names = variable.names,
                                 n.iter = n.iter,
                                 thin = thin,
                                 ...)
    HydePost <- list(codas = codas,
                     observed = cHN$observed,
                     dag = cHN$dag,
                     factorRef = cHN$factorRef)
  }
  else
  {
    codas <- lapply(X = 1:length(cHN),
                    FUN =
                      function(j, ...)
                      {
                        rjags::coda.samples(model = cHN[[j]][["jags"]],
                                            variable.names = variable.names,
                                            n.iter = n.iter,
                                            thin = thin,
                                            ...)
                      },
                    ...)
    observed <- do.call("rbind",
                        lapply(X = cHN,
                               FUN = function(x) x[["observed"]]))

    HydePost <- list(codas = codas,
                     observed = observed,
                     dag = cHN[[1]]$dag,
                     factorRef = cHN[[1]]$factorRef)
  }



  class(HydePost) <- "HydeSim"
  if (bind)
  {
    bindSim(HydePost)
  }
  else
  {
    HydePost
  }

}

#' @rdname HydeSim
#' @export

HydePosterior <- function(...)
{
  message("`HydePoseterior` has been deprecated and replaced by `HydeSim`")
  HydeSim(...)
}
