#' @include generics.R
#' @useDynLib empCop, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

number2binary = function(number, noBits) {
    binary_vector = rev(as.numeric(intToBits(number)))
    binary_vector[-(1:(length(binary_vector) - noBits))]
}

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

        to_apply <- function(u,v){
            if (any(u == v)) {
                return(0)
            }
            rez <- sapply(1:(2^d),function(i){
                p <- number2binary(i-1,d)
                return((-1)^sum(p) * pCopula(u * p + v * (1 - p), copula))
            })
            return(sum(rez))
        }

        if (nrow(u) > 1) {
            # recursive if asked for more than one.
            return(sapply(1:nrow(u), function(i) {
                to_apply(u[i, ], v[i, ])
            }))
        } else {
            return(to_apply(u,v))
        }

    })

























