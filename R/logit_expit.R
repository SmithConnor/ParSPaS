#' @title  logit function
#' @param x numeric
#' @export

logit = function(x){
  if(base::any(x <= 0) | base::any(x >= 1)){
    base::stop("x must be between 0 and 1.")
  }
  base::return(base::log(x) - base:log(1-x))
}

#' @title Expit function
#' @param x numeric
#' @export

expit = function(x){
  base::return(1/(1+base::exp(-x)))
}
