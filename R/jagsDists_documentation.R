#' JAGS Probability Distributions try.
#'
#' A dataset listing the JAGS probability distributions and their parameters
#'
#' @format A data frame with 30 rows and 7 variables:
#' \describe{
#'   \item{DistName}{Distribution Name}
#'   \item{FnName}{JAGS Function Name}
#'   \item{FnNameR}{R Function Name}
#'   \item{xLow}{Minimum value for x, the random variable}
#'   \item{xHigh}{Maximum value for x, the random variable}
#'   \item{Parameters}{Names of the JAGS parameters}
#'   \item{RParameter}{R function argument name}
#'   \item{paramLimit}{Limits on the parameter}
#'   \item{paramLogic}{The text of a logical check used in \code{setNode} to
#'     ensure stated parameters are valid.}
#'   \item{Rsupport}{Logical value, indicating if an R equivalent is
#'     supported by \code{McBias}}
#' }
#' @source \url{http://people.stat.sc.edu/hansont/stat740/jags_user_manual.pdf}
"jagsDists"
