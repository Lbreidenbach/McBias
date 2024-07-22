#' A hydenet object with the set distributions shown in the paper use case
#'
#' A dataset containing the prices and other attributes of almost 54,000
#'  diamonds. The variables are as follows:
#'
#' @format A HydeNetwork with 7 nodes and set node distributions
#' \describe{
#'   \item{BMI}{Gaussian distribution node reflecting body mass index}
#'   \item{WC}{Gaussian distribution node reflecting waist circumference}
#'   \item{SBP}{Gaussian distribution node reflecting systolic blood pressure}
#'   \item{DM}{binary distribution reflecting type 2 diabetes prevalence}
#'   \item{PAD}{binary distribution reflecting peripherial artery disease prevalence}
#'   \item{CHD}{binary distribution reflecting coranary heart disease prevalence}
#'   \item{rs7903146}{binary distribution reflecting the prevalence of the single nucleotide polymorphism rs7903146}
#' }


t2d_dag = HydeNetwork(~PAD|DM + CHD|DM*WC*SBP + DM|rs7903146*SBP*WC + WC|BMI + SBP|BMI)
t2d_dag = setNode(t2d_dag, PAD, nodeType = "dbern", prob = paste0("ilogit(",.7," * DM + ", -1.339663,")"))
t2d_dag = setNode(t2d_dag, DM, nodeType = "dbern", prob = paste0("ilogit(",0.21," * WC + ", 0.055," * SBP + ", 0.36," * rs7903146 + ", -16.8671269607129,")"))
t2d_dag = setNode(t2d_dag, CHD, nodeType = "dbern", prob = paste0("ilogit(",1," * DM + ", 0.029," * WC + ", 0.027," * SBP + ", -6.13999741241117,")"))
t2d_dag = setNode(t2d_dag, rs7903146, nodeType = "dbern", prob = paste0("ilogit(", -1.208311,")"))
t2d_dag = setNode(t2d_dag, WC, nodeType = "dnorm", mu = paste0(0.821," * BMI + ",39.6 - 0.821*28.7), tau = 0.253)
t2d_dag = setNode(t2d_dag, SBP, nodeType = "dnorm", mu = paste0(0.148," * BMI + ",122 - .148*28.7), tau = 0.267)
t2d_dag = setNode(t2d_dag, BMI, nodeType = "dnorm", mu = paste0(28.7), tau = 0.447)

