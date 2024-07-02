#'Changes a data frame's values to represent a misdiagnosis rate
#'
#'Changes values of a specified binary column in a data frame to simulate information bias and or add noise. If under_rate is set it will change a percentage of 1s to 0s and vice versa if over_rate is set. Both under_rate and over_rate can be set.
#'
#' @param df data frame with at least one binary column set as an integer
#'
#' @param variable Character value. The column in the data frame that the misdiagnosis rate will be set to. Must be integer class and only contain 0s or 1s.
#'
#' @param under_rate The percentage of 0s in the column the user wishes to change to 1s
#'
#' @param over_rate The percentage of 1s in the column the user wishes to change to 0s
#'
#' @return A data frame with the values of the specifed column changed as set by the user
#'
#'@examples McBias/examples/misdiagnosis_example.R
#'@seealso [varied_runs()]
#'@export
#'

misdiagnosis = function(df, variable, under_rate=0, over_rate=0){
  index_1 = which(df[variable] == 1)
  index_0 = which(df[variable] == 0)

  over = round(length(index_0)*over_rate)
  under = round(length(index_1)*under_rate)

  if(under != 0){
    df[index_1, variable][1:under] = 0
  }
  if(over != 0){
    df[index_0, variable][1:over] = 1
  }

  return(df)
}
