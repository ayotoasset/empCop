#' @include generics.R
#' @useDynLib empCop, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

number2binary = function(number, noBits) {
    binary_vector = rev(as.numeric(intToBits(number)))
    binary_vector[-(1:(length(binary_vector) - noBits))]
}


# should be passe only ONE observation at a time.
# u and v must have same length (dim of cop)
# u must be smaller than v
# the call to pCopula is done with a matrix so no overhead for calling functions.
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

























