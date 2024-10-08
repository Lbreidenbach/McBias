#' Pulmonary Embolism Dataset
#'
#' These are simulated data on 10,000 cases with suspected pulmonary embolism at a hospital.
#'
#' @format A data frame with 10000 rows and 7 variables:
#' \describe{
#'   \item{wells}{Wells score (integer ranging from 1 to 10 indicating the degree to which PE is suspected based on clinical review of symptoms)}
#'   \item{pregnant}{Factor indicating pregnancy (No, Yes)}
#'   \item{pe}{Factor indicating pulmonary embolism has occurred (No,Yes)}
#'   \item{angio}{Result of pulmonary angiography test (Negative, Positive)}
#'   \item{d.dimer}{Numeric result of diagnostic blood test called D-Dimer.}
#'   \item{treat}{Factor indicating whether or not treatment for PE was administered (No,Yes)}
#'   \item{death}{Factor indicating patient mortality (No,Yes)}
#' }
#' @source Simulated data - not from real patients.
"PE"
