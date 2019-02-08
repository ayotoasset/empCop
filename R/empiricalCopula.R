#' @include generics.R
NULL


############################### Empirical copula class ######
#' Empirical Copula class (virtual mother class)
#'
#' @slot pseudo_data matrix : pseudo_data that the empirical copula is based on.
#'
#' @export
setClass(Class = "empiricalCopula", contains = c("VIRTUAL", "Copula"),
         slots = c(pseudo_data = "data.frame"), validity = function(object) {
           errors <- c()
           if (prod(apply(object@pseudo_data, 1:2, is.numeric)) != 1) {
             errors <- c(errors, "The data argument must be a numeric data.frame")
           }
           if (prod(object@pseudo_data <= 1) * prod(object@pseudo_data >=
                                                    0) == 0) {
             errors <- c(errors, "The pseudo-data should be numeric between 0 and 1 (both included)")
           }
           if (length(errors) == 0)
             TRUE else errors
         })
setMethod(f = "dim", signature = (x = "empiricalCopula"), definition = function(x) {
  return(ncol(x@pseudo_data))
})

