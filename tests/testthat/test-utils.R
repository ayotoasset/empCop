# number2binary = function(number, noBits) {
#     binary_vector = rev(as.numeric(intToBits(number)))
#     binary_vector[-(1:(length(binary_vector) - noBits))]
# }
#
#
# setMethod("vCopula", signature = c(u = "matrix", v = "matrix", copula = "Copula"),
#     definition = function(u, v, copula) {
#
#         # can handle any copula thant pCopula could handle.
#
#         # u and v must be numeric, copula must be a copula, and v must be
#         # smaller than u
#         if (nrow(u) != nrow(v)) {
#             stop("u and v must have same shape (same number of row and columns)")
#         }
#         if (nrow(u) > 1) {
#             # recursive if asked for more than one.
#             return(sapply(1:nrow(u), function(i) {
#                 vCopula(u[i, ], v[i, ], copula)
#             }))
#         }
#
#         if (any(v < u)) {
#             stop("u must be smaller than v !")
#         }
#         if (any(u == v)) {
#             return(0)
#         }
#
#         d = dim(copula)
#         rez <- vector(length = (2^d - 1))
#
#         for (i in 0:(2^d - 1)) {
#             p <- number2binary(i, d)
#             rez[i + 1] <- (-1)^sum(p) * pCopula(u * p + v * (1 - p), copula)
#         }
#
#         return(sum(rez))
#
#     })
#
# intersect <- function(x_min, x_max, y_min, y_max) {
#     # retourne l'intersection es deux rectangles [x_min,x_max] et
#     # [y_min,y_max]
#
#     # sous la forme d'un rectangle [rez_min,rez_max]
#
#
#
#     # pour chaque dimensions, il faut prendre le maximum des min et le
#     # minimum des max.
#     rez <- list()
#     rez$min <- pmax(x_min, y_min)
#     rez$max <- pmin(x_max, y_max)
#
#     # checker que c'est pas l'ensemble vide :
#     if (any(rez$min > rez$max)) {
#         warning("Ensemble vide ! On return NULL.")
#         return(NULL)
#     }
#
#     return(rez)
# }
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
