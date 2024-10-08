#' @name bindSim
#' @export bindSim
#'
#' @title Bind Output From coda Samples
#'
#' @description After determining the simulated distributions are satisfactory,
#'   it can be advantageous to bind the simulated distributions together in
#'   order to aggregate values and perform other manipulations and analyses.
#'
#' @param hydeSim An object of class \code{HydeSim}
#' @param relabel_factor Logical.  If \code{TRUE}, factors that had been
#'   converted to integers for the JAGS code can be relabelled as factors
#'   for additional analysis in R.
#'
#' @details For the purposes of this function, it is assumed that if the
#'   simulated distributions are satisfactory, the multiple chains in a run
#'   can be bound together.  Subsequently, the multiple runs are bound
#'   together.  Lastly, the factors are relabeled, if requested.
#'
#' @author Jarrod Dalton and Benjamin Nutter
#'
#' @example examples/Dag_example.R
bindSim <- function(hydeSim, relabel_factor=TRUE)
{
  if (class(hydeSim$codas) == "mcmc.list")
  {
    bound <-
      do.call("rbind",
              lapply(seq_along(hydeSim[["codas"]]),
                     bind_chains_mcmclist,
                     hydeSim
              )
      )
  }
  else
  {
    bound <-
      do.call("rbind",
              lapply(hydeSim[["codas"]],
                     bind_chains_list
              )
      )
  }

  #* JAGS returns integers in place of factors.  If requested,
  #* replace the integers as factors.
  if (relabel_factor)
  {
    factors_to_relabel <- names(bound)[names(bound) %in% names(hydeSim$factorRef)]
    for(i in factors_to_relabel)
    {
      bound[i] <- factor(bound[[i]],
                         levels=hydeSim$factorRef[[i]]$value,
                         labels=hydeSim$factorRef[[i]]$label)
    }
  }

  as.data.frame(bound)
}



#**** UTILITY FUNCTIONS
#**** bind_chains_mcmclist is used when there is a single network (not a decision network)
#**** bind_chains_list is used when a list of mcmclists is being bound, such as
#****                  when a decision network was run.
bind_chains_mcmclist <- function(mcmc, hydeSim)
{
  out <- as.data.frame(hydeSim$codas[[mcmc]])
  out$chain_index <- mcmc
  out$obs_index <- seq_along(mcmc)
  out
}

bind_chains_list <- function(mcmc)
{
  bound <-
    lapply(1:length(mcmc),
           function(chain)
           {
             out <- as.data.frame(mcmc[[chain]])
             out$chain_index <- chain
             out$obs_index <- seq_along(chain)
             out
           }
    )
  do.call("rbind", bound)
}


#' @rdname bindSim
#' @export

bindPosterior <- function(hydeSim, relabel_factor=TRUE)
{
  message("`bindPosterior` is deprecated and replaced by `bindSim`")
  bindSim(hydeSim, relabel_factor=relabel_factor)
}
