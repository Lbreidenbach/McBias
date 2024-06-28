#'@noRD

tot_bind <- function(datalist) {
  require(plyr)
  temp = plyr::rbind.fill(datalist)
  rownames(temp) <- unlist(lapply(datalist, row.names))
  return(temp)
}
