#'@noRd

dichotomize = function(column, df, div){
  x = as.numeric(quantile(df[,column])[div])
  index_0 = which(df[column] < x)
  index_1 = which(df[column] >= x)
  df[index_0, column] = 0
  df[index_1, column] = 1
  df[,column] = as.integer(df[,column])
  df = rbind(df[index_1,], df[index_0,])
  rownames(df) = NULL
  return(df)
}
