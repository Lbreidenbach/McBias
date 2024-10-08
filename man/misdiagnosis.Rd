% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misdiagnosis.R
\name{misdiagnosis}
\alias{misdiagnosis}
\title{Changes a data frame's values to represent a misdiagnosis rate}
\usage{
misdiagnosis(df, variable, under_rate = 0, over_rate = 0)
}
\arguments{
\item{df}{data frame with at least one binary column set as an integer}

\item{variable}{Character value. The column in the data frame that the misdiagnosis rate will be set to. Must be integer class and only contain 0s or 1s.}

\item{under_rate}{The percentage of 0s in the column the user wishes to change to 1s}

\item{over_rate}{The percentage of 1s in the column the user wishes to change to 0s}
}
\value{
A data frame with the values of the specifed column changed as set by the user
}
\description{
Changes values of a specified binary column in a data frame to simulate information bias and or add noise. If under_rate is set it will change a percentage of 1s to 0s and vice versa if over_rate is set. Both under_rate and over_rate can be set.
}
\examples{
#creates the causal diagram
my_dag = HydeNetwork(~B|A + C|A + D|B*C)

#adds set effect sizes and distributions to the causal diagram
my_dag = setNode(my_dag, A, nodeType = "dbern", prob = 0.5)
my_dag = setNode(my_dag, B, nodeType = "dnorm", mu = paste0(0.2," * A + ",0 - 0.2*0.5), tau = 1)
my_dag = setNode(my_dag, C, nodeType = "dbern", prob = paste0("ilogit(",0.8," * A + ",set_p(0.9, 0.8*0.5),")"))
my_dag = setNode(my_dag, D, nodeType = "dbern", prob = paste0("ilogit(",0.05," * B + ", 1.2," * C + ",set_p(0.25, 1.2 * 0.9 + 0.05 * 0 ),")"))

#create data creates a single dataset.
#This lets users see if the simulated datasets behave as expected before committing to a large simulation with many iterations

sim_data = create_data(my_dag, 10000)

under_diagnosis = misdiagnosis(sim_data, "A", under_rate = 0.2)
sum(under_diagnosis$A)/10000

over_diagnosis = misdiagnosis(sim_data, "A", over_rate = 0.2)
sum(over_diagnosis$A)/10000

#Note that the the rates change the percentage of existing cases. If there are 10 "cases" in a dataset and the under_rate is 10\% then only 1 "case" will be changed to a "control".
}
\seealso{
\code{\link[=varied_runs]{varied_runs()}}
}
