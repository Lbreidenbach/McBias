#'@noRd

bi_strat = function(x, df, column){
  index_1 = which(df[column] == x)
  index_0 = which(df[column] != x)
  df[index_0, column] = 0
  df[index_1, column] = 1
  return(list(df[index_1,], df[index_0,]))
}
