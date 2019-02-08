#' @include generics.R
NULL


############################### ConvexComCopula class #######


#' ConvexCombCopula class
#'
#' @slot copulas list of copulas of same dimension
#' @slot alpha numeric. A vector of (positive) weights.
#'
#' @return a ConvexCombCopula object.
#' @export
.ConvexCombCopula = setClass(Class = "ConvexCombCopula", contains = "Copula",
                             slots = c(copulas = "list", alpha = "numeric"), validity = function(object) {
                               errors <- c()
                               if (length(object@copulas) != length(object@alpha)) {
                                 errors <- c(errors, "the weights parameter alpha must have same length as the copulas list")
                               }
                               if (!all(sapply(object@copulas, function(x) {
                                 is(x, "Copula")
                               }))) {
                                 errors <- c(errors, "parameter copulas should contains a list of copulas")
                               }
                               if (!all(object@alpha >= 0)) {
                                 errors <- c(errors, "weights should be positive")
                               }
                               if (!(length(unique(sapply(object@copulas, dim))) == 1)) {
                                 errors <- c(errors, "all copulas must have same dimension")
                               }
                               if (length(errors) == 0)
                                 TRUE else errors
                             })
#' ConvexCombCopula
#'
#' @param copulas a list of copulas of same dimention
#' @param alpha a vector of (positive) weights
#'
#' The convexcombcopula class is used to build convex combinations of copulas,
#' with given positives weights. The rCopula and pCopula functions works for
#' thoose copulas, assuming they work for the given copulas that we combined
#' in a convex way.
#'
#' @return a ConvexCombCopula object
#' @export
#'
#' @examples
#' copulas <- list(
#'   copula::archmCopula('gumbel',3,dim=2),
#'   copula::archmCopula('clayton',-1,dim=2)
#' )
#' alpha <- c(1,4)
#'
#' (cop <- ConvexCombCopula(copulas,alpha))
#'
#' plot(rCopula(100,cop))
#' pCopula(c(0.5,0.7),cop)
ConvexCombCopula = function(copulas, alpha = rep(1, length(copulas))) {
  if (missing(copulas) || (!is(copulas, "list"))) {
    stop("The argument copulas must be provided as a list of copulas")
  }
  .ConvexCombCopula(copulas = copulas, alpha = alpha)
}
setMethod(f = "dim", signature = (x = "ConvexCombCopula"), definition = function(x) {
  return(dim(x@copulas[[1]]))
})
setMethod(f = "show", signature = c(object = "ConvexCombCopula"), definition = function(object) {
  cat("This is a ConvexCombCopula , with : \n", "  dim =", dim(object@copulas[[1]]),
      "\n   number of copulas =", length(object@copulas), "\n   alpha =",
      object@alpha, "\n")
  cat("sub-copulas can be accessed trhough the @copulas slot")
})
setMethod(f = "rCopula", signature = c(n = "numeric", copula = "ConvexCombCopula"),
          definition = function(n, copula) {

            # to choose wich copulas will be simulated from, sample
            # 1:length(copulas) with weights equal to alpha, with replacement OFC
            n_cop = length(copula@copulas)
            sampled_copulas <- sample(1:n_cop, size = n, replace = TRUE, prob = copula@alpha)

            # then sample from each of thoose copulas the right number of times :
            samples <- mapply(function(cop, how_much) {
              rCopula(n = how_much, copula = cop)
            }, copula@copulas, sapply(1:n_cop, function(x) {
              sum(x == sampled_copulas)
            }))

            # then rbind all of them and mix rows :
            samples <- do.call(rbind, samples)
            samples <- samples[sample(1:nrow(samples), size = nrow(samples),
                                      replace = FALSE), ]
            return(samples)
          })
setMethod(f = "pCopula", signature = c(u = "matrix", copula = "ConvexCombCopula"),
          definition = function(u, copula) {

            # remind that pCopula and dCopula generics already transform inputs
            # into matrices...  remind that pCopula and dCopula generics already
            # transform inputs into matrices...
            if (ncol(u) != dim(copula)) {
              stop("the input value must be coerçable to a matrix with dim(copula) columns.")
            }

            outputs <- lapply(copula@copulas, function(cop) {
              pCopula(u, cop)
            })

            rez <- outputs[[1]] * copula@alpha[1]
            for (i in 2:length(outputs)) {
              rez <- rez + outputs[[i]] * copula@alpha[i]
            }

            return(rez)

          })


# Okay.

# next step will be to use this class to perform an automatic-weighting
# of the copulas;

# e.g we could perform a weighting by looking at some distance on the
# copula space to the empirical copula of some data.  2 questions
# arises naturaly : Q1 : Wich distance ?  Q2 : how to be shure we are
# not overfitting with this distance ? i.e penalisation.

# So we need a distance on the copula space, and some tests based on
# it.  The distance of the model to the empirical copula on the
# empirical points will be our 'goodness of fit' measure.

# We need to implement distances on the copula space to the empirical
# copula... This seems rather dificult

# We also need to implement the pcopula function for all of thoose
# copulas.

# for exemple, we could :

# 1) use the quadratic distance on a pseudo dataset? with bootstrap of
# this dataset ?

#