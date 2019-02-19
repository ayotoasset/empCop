#' @include generics.R
#' @useDynLib empCop, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

number2binary = function(number, noBits) {
    binary_vector = rev(as.numeric(intToBits(number)))
    binary_vector[-(1:(length(binary_vector) - noBits))]
}


#' @rdname vCopula-methods
#' @aliases vCopula,vector,vector,Copula
setMethod("vCopula", signature = c(u = "vector", v = "vector", copula = "Copula"),
    definition = function(u, v, copula) {

        # can handle any copula thant pCopula could handle.
        # shoul be better vetorised...
        # u and v must be numeric, copula must be a copula, and v must be
        # smaller than u
        # if (nrow(u) != nrow(v)) {
        #     stop("u and v must have same shape (same number of row and columns)")
        # }
        #
        # if (any(v < u)) {
        #     stop("u must be smaller than v !")
        # }

        d = dim(copula)
        p <- t(sapply(1:(2^d),function(i){number2binary(i-1,d)}))
        sign <- (-1)^rowSums(p)
        eval_points <-t(t(p) * as.vector(u) + t(1-p) * as.vector(v))

        return(sum(sign * pCopula(eval_points,copula)))
        # browser()


        # to_apply <- function(u,v){



        # }
        # if (nrow(u) > 1) {
        #     # recursive if asked for more than one.
        #     return(sapply(1:nrow(u), function(i) {
        #         to_apply(u[i, ], v[i, ])
        #     }))
        # } else {
        #    return(to_apply(u,v))
        # }

    })

#' @rdname vCopula-methods
#' @aliases vCopula,matrix,matrix,Copula
setMethod("vCopula", signature = c(u = "matrix", v = "matrix", copula = "Copula"),
          definition = function(u, v, copula) {

              # can handle any copula thant pCopula could handle.
              # shoul be better vetorised...
              # u and v must be numeric, copula must be a copula, and v must be
              # smaller than u
              if (nrow(u) != nrow(v)) {
                  stop("u and v must have same shape (same number of row and columns)")
              }

              if (any(v < u)) {
                  stop("u must be smaller than v !")
              }

              d = dim(copula)
              p <- t(sapply(1:(2^d),function(i){number2binary(i-1,d)}))
              sign <- (-1)^rowSums(p)
              return(sapply(1:nrow(u),function(i){
                  if(all(u[i,] == v[i,])){return(0)}
                  eval_points <-t(t(p) * as.vector(u[i,]) + t(1-p) * as.vector(v[i,]))
                  return(sum(sign * pCopula(eval_points,copula)))
              }))
          })






















