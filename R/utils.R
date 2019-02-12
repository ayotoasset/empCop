#' @include generics.R
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

intersect <- function(x_min, x_max, y_min, y_max) {
    # retourne l'intersection es deux rectangles [x_min,x_max] et
    # [y_min,y_max] sous la forme d'un rectangle [rez_min,rez_max]

    rez <- list()
    rez$min <- pmax(x_min, y_min)
    rez$max <- pmin(x_max, y_max)

    # checker que c'est pas l'ensemble vide :
    if (any(rez$min > rez$max)) {
        warning("Ensemble vide ! On return NULL.")
        return(NULL)
    }
    return(rez)
}

























