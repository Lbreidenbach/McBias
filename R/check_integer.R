#'@noRd

check_integer = function(x){
  check = all.equal(x, as.integer(x))
  return(isTRUE(check))
}
