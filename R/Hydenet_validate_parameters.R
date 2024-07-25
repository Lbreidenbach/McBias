#' @noRd

validateParameters <- function(params, dist)
{
  expr <- jagsDists[["paramLogic"]][jagsDists[["FnName"]] == dist]
  valid <- sapply(X = expr,
                  FUN = function(e) with(params, eval(parse(text=e))))
  valid[sapply(X = params,
               FUN = function(p) p %in% c("fromData", "fromFormula"))] <- TRUE
  valid
}
