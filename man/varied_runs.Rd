% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varied_runs.R
\name{varied_runs}
\alias{varied_runs}
\title{Creates a list of summary statistics for methods testing and bias quantification}
\usage{
varied_runs(
  runs,
  dag,
  exposure,
  outcome,
  covariates = NULL,
  sb = NULL,
  n = 10000,
  positivity = F,
  misdiagnosis_v = outcome,
  under_r = 0,
  over_r = 0,
  ratio = 1,
  match_methods = NULL,
  ...
)
}
\arguments{
\item{runs}{The number of data frames to be simulated}

\item{dag}{Hydenet directed acyclic graph object (DAG object) with set distributions for each node.}

\item{exposure}{Character value. The column name in a data frame that represents exposure in an}

\item{outcome}{Character value. The column name in a data frame that represents the outcome.}

\item{covariates}{Character value/vector. The name(s) of the columns that the user wants adjust on. Defaults to NULL meaning no covariates are adjusted on.}

\item{sb}{Character value/vector. The name(s) of \strong{binary} column(s). Stratifies the data frame to include only the rows with 1s in every listed column.}

\item{n}{Integer. The number of samples in each data frame}

\item{positivity}{logical. If set to TRUE, checks for positivity violations among binary columns. If violated, it changes the value in the first row of the column to comply with positivity. Defaults to FALSE.}

\item{misdiagnosis_v}{Character value. A \strong{binary} column name in a data frame that the user wishes to set misdiagnosis rates on. Defaults to the column set as the outcome.}

\item{under_r}{double between 0 and 1. The percentage of 1s to be changed to 0s in the column set by misdiagnosis_v}

\item{over_r}{double between 0 and 1.  The percentage of 0s to be changed to 1s in the column set by misdiagnosis_v}

\item{ratio}{Integer. number of controls that should be matched to each case. Defaults to 1}

\item{match_methods}{Character value/vector. The method(s) in which distance between a matched case and control is measured as specified by MatchIt. Defaults to NULL which does no matching analysis.}

\item{...}{If the DAG has any unset variables, define them here}
}
\value{
A list of 11 elements with each element bulleted below in order
\itemize{
\item odds_ratio, NA if not applicable to the method used
\item calculated_ate, the calculated effect size estimate between the set exposure and outcome
\item lower_int, the lower bound of the 95\% confidence interval for the calculated_ate
\item upper_int, the upper bound of the 95\% confidence interval for the calculated_ate
\item p_values, the p value for the calculated_ate
\item exp_prevalence, the exposure prevalence if the exposure variable is binary, returns NAs if not
\item out_prevalence, the outcome prevalence if the outcome variable is binary, returns NAs if not
\item sample_population, the number of samples in the dataset. If the analysis requires stratifying or throwing data samples, sample_population will change to reflect that.
\item set_ate, the effect size between the exposure and outcome that is directly set in the DAG object. Acts as the true effect size.
\item over_r, the overdiagnosis rate that was set. defaults to 0
\item under_r, the overdiagnosis rate that was set. defaults to 0
}

each list element will contain x number of columns and y number of rows, where x is the number of different methods tested, and y is the number of simulated data sets created.
}
\description{
Creates a set number of simulated data frames and analyzes them with the methods offered in apply_methods(). Returns a list of summary statistics from each analysis
}
\examples{
#creates the causal diagram
my_dag = HydeNetwork(~B|A + C|A*B + D|B*C)


#if we set up a flexible dag like this, we can quickly change the effect size between nodes A and C as well as the prevalence of node A
flex_dag = function(x,y){
  my_dag = setNode(my_dag, A, nodeType = "dbern", prob = x)
  my_dag = setNode(my_dag, B, nodeType = "dnorm", mu = paste0(0.8," * A + ",0 - 0.8*x), tau = 1)
  my_dag = setNode(my_dag, C, nodeType = "dbern", prob = paste0("ilogit(",y," * A + ",set_p(0.3, y*x),")"))
  my_dag = setNode(my_dag, D, nodeType = "dbern", prob = paste0("ilogit(",0.05," * B + ", 1.2," * C + ",set_p(0.25, 1.2 * 0.9 + 0.05 * 0 ),")"))
}

#To analyze the performance of 1 simulated data frame we can create data and analyze it like so:
sim_data = create_data(flex_dag, 10000,  x=0.5, y = 3)

run_result = apply_methods(exposure = "B", outcome = "C", covariates = "A", df = sim_data)
run_result

#However varied_runs() automatically repeats this process a set number of times and stores all results in a matrix. This allows for MCMC analysis.
small_conf_naive = varied_runs(200, flex_dag, exposure = "B", outcome = "C",  n = 10000, x = .5, y = 1)
small_conf_adj = varied_runs(200, flex_dag, exposure = "B", outcome = "C", covariates = "A",  n = 10000, x = .5, y = 1)
large_conf_naive = varied_runs(200, flex_dag, exposure = "B", outcome = "C",  n = 10000, x = .5, y = 3)
large_conf_adj = varied_runs(200, flex_dag, exposure = "B", outcome = "C", covariates = "A",  n = 10000, x = .5, y = 3)

#you can use reparse_runs() to combine matrices of different runs
confounder_comp = reparse_runs(list(small_conf_naive, small_conf_adj, large_conf_naive, large_conf_adj), list_names =  c("A->C = 1, naive", "A->C = 1, adjusted", "A->C = 3, naive", "A->C = 3, adjusted"))
#ci_ridges lets you quickly visualize results
ci_ridges(confounder_comp, title = "How well is the confounder adjusted for?", subtitle = "Comparison of confounder adjustment methods across different confounder strengths")
#beta_summary() will return summary statistics about the bias present in each scenario
beta_summary(confounder_comp)

#You can also use reparse_runs() to only look at one method in particular.
logistic_comp = reparse_runs(list(small_conf_naive, small_conf_adj, large_conf_naive, large_conf_adj), method = "regression", list_names =  c("A->C = 1, naive", "A->C = 1, confounder adjusted", "A->C = 3, naive", "A->C = 3, confounder adjusted"))
ci_ridges(logistic_comp, "How well is the confounder adjusted for?", subtitle = "Comparison of logistic regression across different confounder strengths")


#if a beta value between 2 binary nodes is too large, or the sample size is too small, one node may become completely predictive of another which violates separation.
#An error will occur if this is the case:

# very_large_conf = varied_runs(200, flex_dag, exposure = "B", outcome = "C",  n = 10000, x = .5, y = 100)
}
\seealso{
\code{\link[=apply_methods]{apply_methods()}},\code{\link[=ci_ridges]{ci_ridges()}},\code{\link[=beta_summary]{beta_summary()}}, \code{\link[=reparse_runs]{reparse_runs()}}
}
