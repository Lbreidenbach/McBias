#' @noRd
#'

makeFactorRef <- function(network)
{
  network_factors <-
    names(network[["factorLevels"]])[!vapply(X = network[["factorLevels"]],
                                             FUN = is.null,
                                             FUN.VALUE = logical(1))]

  if (length(network_factors) == 0)
  {
    return(NULL)
  }

  Ref <-
    lapply(X = network_factors,
           FUN =
             function(f)
             {
               data.frame(value = 1:length(network[["factorLevels"]][[f]]),
                          label = network[["factorLevels"]][[f]],
                          stringsAsFactors = FALSE)
             }
    )
  names(Ref) <- network_factors

  types <- unlist(network[["nodeType"]][network_factors])
  types <- types[types %in% "dbern"]

  Ref[names(types)] <-
    lapply(X = Ref[names(types)],
           FUN =
             function(f)
             {
               f$value <- f$value - 1
               f
             }
    )

  Ref[unique(names(Ref))]
}

