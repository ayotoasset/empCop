#' @include generics.R
NULL


############################### ConvexComCopula class #######
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
                                 errors <- c(errors, "weights should be positives")
                               }
                               if(sum(object@alpha) != 1){
                                 errors <- c(errors,"weights should add up to 1.")
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
  .ConvexCombCopula(copulas = copulas, alpha = alpha/sum(alpha))
}
setMethod(f = "dim",     signature = (x = "ConvexCombCopula"),                      definition = function(x)         {
  return(dim(x@copulas[[1]]))
})
setMethod(f = "show",    signature = c(object = "ConvexCombCopula"),                definition = function(object)    {
  cat("This is a ConvexCombCopula , with : \n", "  dim =", dim(object@copulas[[1]]),
      "\n   number of copulas =", length(object@copulas), "\n   alpha =",
      object@alpha, "\n")
  cat("sub-copulas can be accessed trhough the @copulas slot")
})
setMethod(f = "rCopula", signature = c(n = "numeric", copula = "ConvexCombCopula"), definition = function(n, copula) {

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
setMethod(f = "pCopula", signature = c(u = "matrix", copula = "ConvexCombCopula"),  definition = function(u, copula) {

            # remind that pCopula and dCopula generics already transform inputs
            # into matrices...  remind that pCopula and dCopula generics already
            # transform inputs into matrices...
            if (ncol(u) != dim(copula)) {
              stop("the input value must be coerçable to a matrix with dim(copula) columns.")
            }
#
#             outputs <- lapply(1:length(copula@copulas), function(i) {
#               pCopula(u, copula@copulas[[i]]) * copula@alpha[i]
#             })
#
#             rez <- Reduce("+",outputs)
#             return(rez)
#             # taken from the mixCopula class.
              as.vector(
                vapply(copula@copulas, pCopula, FUN.VALUE=numeric(nrow(u)), u=u)
                %*%
                  copula@alpha)




          })


# Okay.

# next step will be to use this class to perform an automatic-weighting
# of the copulas;

# We need a function to fit a ConvexCombCopula, given marginals copulas and data.


# e.g we could weights them on the pCopula of each data point. Weights more the copula that has better likelyhood.
#
# mixing_crit <- function(copulas,pseudo_data){
#   # for each copula, calculate the value of the copula on each data point :
#
#   errors <- sapply(copulas,function(cop){
#     (pCopula(u=pseudo_data,copula=cop)-pCopula(u=pseudo_data,copula=empCopula(pseudo_data)))^2
#   })
#
#   #errors is a matrix with n_copula columns and nrow(pseudo_data) rows.
#   return(sqrt(colSums(errors)))
# }
#
#
# availiables_m <- function(n){
#   m <- 1:n
#   m[n%%m==0]
# }
#
# m <- availiables_m(nrow(pseudo_data))
# copulas <- sapply(m,function(i){cbCopula(pseudo_data,pseudo=FALSE,m = i)})
# rmse <- mixing_crit(copulas,pseudo_data)
# plot(m,rmse,type="b")
#
#


