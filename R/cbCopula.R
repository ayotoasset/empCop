#' @include generics.R empiricalCopula.R
NULL

############################### Checkerboard copula class #######
.cbCopula = setClass(Class = "cbCopula", contains = "empiricalCopula",
                     slots = c(m = "numeric"), validity = function(object) {
                       errors <- c()
                       if ((nrow(object@pseudo_data)%%object@m) != 0) {
                         errors <- c(errors, "m should divide the number of row")
                       }
                       if (length(errors) == 0)
                         TRUE else errors
                     })
#' cbCopula contructor
#'
#' @param x the data to be used
#' @param m checkerboard parameter
#' @param pseudo Boolean, defaults to `FALSE`. Set to `TRUE` if you are already
#'  providing pseudo datas into the `x` argument.
#'
#' @return a cbCopula object
#' @export
cbCopula = function(x, m = nrow(x), pseudo = FALSE) {
  if (missing(x)) {
    stop("The argument x must be provided")
  }
  if (!pseudo) {
    x <- apply(x, 2, rank, na.last = "keep")/(nrow(x) + 1)
  }
  .cbCopula(pseudo_data = as.data.frame(x), m = m)
}
setMethod(f = "show", signature = c(object = "cbCopula"), definition = function(object) {
  cat("This is a cbCopula , with : \n", "  dim =", dim(object), "\n   n =",
      nrow(object@pseudo_data), "\n   m =", object@m, "\n")
  cat("The variables names are : ", colnames(object@pseudo_data))
})
setMethod(f = "rCopula", signature = c(n = "numeric", copula = "cbCopula"),
          definition = function(n, copula) {

            # The parameter n represent the number of generated values.  if n=0,
            # return a 0xdim matrix :
            if (n == 0) {
              return(matrix(NA, nrow = 0, ncol = ncol(copula@pseudo_data)))
            }
            # The pseudo_data should be a matrix or a dataframe, with one row per
            # value and one column per variable.

            # The parameter m represent the size of the grid : bigger m means
            # smaller grid.

            # The structure (column names) will be preserved in the output, the
            # row_names will indicate the riginal row that builded the box and the
            # result will be converted to data.frame

            # First, let's define the boxes containing the pseudo observations :
            # Since it's a checkerboard, the boxes a regular with side length 1/m,
            # so calculating the right boxes is easy :
            seuil_inf = floor(copula@pseudo_data * copula@m)/copula@m
            seuil_sup = seuil_inf + 1/copula@m

            # Then, let's sample rows coresponding to observations, i.e let's
            # sample thoose boxes with probabilities proportional to the number of
            # observations inside the box.  notes that boxes with probability 0,
            # i.e without observation, were not included here.  This makes the
            # algorythme fast.
            rows <- sample(x = 1:nrow(copula@pseudo_data), size = n, replace = TRUE)
            seuil_inf <- seuil_inf[rows, ]
            seuil_sup <- seuil_sup[rows, ]

            # Finaly, sample some random uniform, and bound them inside the sampled
            # boxes :
            rng <- matrix(runif(ncol(copula@pseudo_data) * n), nrow = n, byrow = FALSE)
            result <- as.data.frame(seuil_inf + rng * (seuil_sup - seuil_inf))
            rownames(result) <- NULL
            colnames(result) <- NULL
            result <- as.matrix(result)

            return(result)
          })
setMethod(f = "pCopula", signature = c(u = "matrix", copula = "cbCopula"),
          definition = function(u, copula) {

            # remind that pCopula and dCopula generics already transform inputs
            # into matrices...

            if (ncol(u) != dim(copula)) {
              stop("the input value must be coerçable to a matrix with dim(copula) columns.")
            }

            seuil_inf = floor(copula@pseudo_data * copula@m)/copula@m
            d = dim(copula)
            n = nrow(copula@pseudo_data)
            rez <- vector(length = nrow(u))

            for (i in 1:nrow(u)) {
              ponderation <- t(apply(seuil_inf, 1, function(y) {
                u[i, ] - y
              }))
              ponderation <- pmax(pmin(ponderation, 1/copula@m), 0)
              rez[i] <- sum(apply(ponderation, 1, function(v) {
                (prod(v) * copula@m^d)
              }))/n
            }

            return(rez)
          })

