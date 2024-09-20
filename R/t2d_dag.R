#' A hydenet object with the set distributions shown in the paper use case
#'
#' A DAG object that represents a predictive model of type 2 diabetes development and development of conditions related to type 2 diabetes.
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

#Code for the use case DAG
t2d_dag = HydeNetwork(~PAD|DM + CHD|DM*WC*SBP + DM|rs7903146*SBP*WC + WC|BMI + SBP|BMI)


t2d_dag = setNode(t2d_dag, PAD, nodeType = "dbern", prob = paste0("ilogit(",.7," * DM + ", -1.339663,")"))
#intercept calculated as log(0.225/(1-0.225)) - (0.7*0.147) = -1.339663

t2d_dag = setNode(t2d_dag, DM, nodeType = "dbern", prob = paste0("ilogit(",0.21," * WC + ", 0.055," * SBP + ", 0.36," * rs7903146 + ", -17.02553,")"))
#intercept calculated as log(0.147/(1-0.147)) - (0.36*0.23 + 0.214*39.6 + 0.055*122) = -17.02553

t2d_dag = setNode(t2d_dag, CHD, nodeType = "dbern", prob = paste0("ilogit(",1," * DM + ", 0.029," * WC + ", 0.027," * SBP + ", -6.139997,")"))
#intercept calculated as log(0.175/(1-0.175)) - (1*0.147 + 0.029*39.6 + 0.027*122) = -6.139997

t2d_dag = setNode(t2d_dag, rs7903146, nodeType = "dbern", prob = paste0("ilogit(", -1.208311,")"))
#intercept calculated as log(0.23/(1-0.23)) = -1.208311

t2d_dag = setNode(t2d_dag, WC, nodeType = "dnorm", mu = paste0(0.821," * BMI + ",39.6 - 0.821*28.7), tau = 0.253)
#intercept directly calculated in code

t2d_dag = setNode(t2d_dag, SBP, nodeType = "dnorm", mu = paste0(0.148," * BMI + ",122 - .148*28.7), tau = 0.267)
#intercept directly calculated in code

t2d_dag = setNode(t2d_dag, BMI, nodeType = "dnorm", mu = 28.7, tau = 0.447)
#BMI is parent node, mean is directly input
