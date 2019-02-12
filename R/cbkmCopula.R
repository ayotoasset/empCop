#' @include generics.R empiricalCopula.R
NULL

############################### Checkerboard with known margin copula class #######
.cbkmCopula = setClass(Class = "cbkmCopula", contains = "empiricalCopula",
                       slots = c(m = "numeric", margins = "numeric", known_cop = "Copula",
                                 box_inf = "matrix", precalc = "list"), validity = function(object) {
                                   errors <- c()
                                   if ((nrow(object@pseudo_data)%%object@m) != 0) {
                                     errors <- c(errors, "m should divide the number of row")
                                   }
                                   if (length(object@margins) != dim(object@known_cop)) {
                                     errors <- c(errors, "number of known margins hsuld be equal to dimention of known copula")
                                   }
                                   if (length(object@margins) > ncol(object@pseudo_data)) {
                                     errors <- c(errors, "the number of known margins should be smaller than the number of total margins in the empirical data !!")
                                   }
                                   if (!all(object@margins %in% 1:ncol(object@pseudo_data))) {
                                     errors <- c(errors, "provided margins number should be smaller than the number of dimention of empirical data")
                                   }
                                   if (length(errors) == 0)
                                     TRUE else errors
                                 })
#' cbkmCopula contructor
#'
#' Given some empirical data, and given some knwon copula estimation on a sub-vector of this data,
#' the checkerboar with known margins construction consist in
#' a conditional pattern where the checkerboar part is conditional on the known part of the copula
#' This allows for high-dimensional-aware contructions.
#'
#'
#' @param x the data to be used
#' @param m checkerboard parameter
#' @param pseudo Boolean, defaults to `FALSE`. Set to `TRUE` if you are already providing pseudo datas into the `x` argument.
#' @param margins_numbers numeric integers refering to the margins you want to assign the known_cop to
#' @param known_cop Copula a copula object representing the known copula for the selected margins.
#'
#'
#' @return a cbCopula object
#' @export
#'
#' @examples
#' true_copula <- onacopulaL(family = 'Clayton',
#' nacList = list(iTau(getAcop('Clayton'), 0.6), 1:4))
#'
#' dataset <- rCopula(50,true_copula)
#'
#' known_margins <- c(2,3)
#' known_clayton <- onacopulaL(
#'   family = 'Clayton',
#'   nacList = list(iTau(getAcop('Clayton'), 0.6), 1:2)
#' )
#'
#' cop <- cbkmCopula(x = dataset,
#'                   m = 5,
#'                   pseudo = TRUE,
#'                   margins_numbers = known_margins,
#'                   known_cop = known_clayton)
#'
#'
#' u=rbind(rep(0,4),matrix(rep(0.7,12),nrow=3),rep(1,4))
#'
#' pCopula(u,cop)
cbkmCopula = function(x, m = nrow(x), pseudo = FALSE, margins_numbers = NULL, known_cop = NULL) {
  if (missing(x)) {
    stop("The argument x must be provided")
  }
  if ((is.null(known_cop) && (!is.null(margins_numbers))) || (is.null(known_cop) &&
                                                              (!is.null(margins)))) {
    stop("known_cop argument and margins argument must both be provided.")
  }

  if (!pseudo) {
    x <- apply(x, 2, rank, na.last = "keep")/(nrow(x) + 1)
  }
  if (all(is.null(known_cop), is.null(margins_numbers))) {
    .cbCopula(pseudo_data = as.data.frame(x), m = m)
  } else {

    ######## pCopula precalculations :
    message("Doing precalculations...")
    # construct boxes :
    box_inf <- do.call(expand.grid, lapply(1:ncol(x), function(x) {
      seq(0, 1 - 1/m, length = m)
    }))
    attr(box_inf, "out.attrs") <- NULL
    box_inf <- as.matrix(box_inf)
    colnames(box_inf) <- NULL

    # now calculate the empirical measure for each box :

    seuil_inf <- as.matrix(floor(x * m)/m)

    nb_emp <- sapply(1:(m^ncol(x)), function(i) {
      sum(apply(seuil_inf, 1, function(x) {
        all(round(m * x, 0) == round(m * box_inf[i, ], 0))
      }))
    })

    # idem pour les cases J :
    nb_emp_J <- sapply(1:(m^ncol(x)), function(i) {
      sum(apply(seuil_inf, 1, function(x) {
        all(round(m * x[margins_numbers], 0) == round(m * box_inf[i,
                                                                  margins_numbers], 0))
      }))
    })

    weights <- vector(length = length(nb_emp))
    weights[nb_emp_J != 0] <- nb_emp[nb_emp_J != 0]/nb_emp_J[nb_emp_J !=
                                                               0]
    weights[nb_emp_J == 0] <- m^(length(margins_numbers) - ncol(x))

    pCopula_precalculations <- list(box_inf = box_inf, nb_emp = nb_emp,
                                    nb_emp_J = nb_emp_J, weights = weights)
    ######## Returning the objec :
    message("Done !")
    .cbkmCopula(pseudo_data = as.data.frame(x), m = m, margins = margins_numbers,
                known_cop = known_cop, box_inf = box_inf, precalc = list(pCopula = pCopula_precalculations))
  }


}
setMethod(f = "show",    signature = c(object = "cbkmCopula"),                definition = function(object)    {
  cat("This is a cbkmCopula , with : \n", "  dim =", dim(object), "\n   n =",
      nrow(object@pseudo_data), "\n   m =", object@m, "\n")
  cat("The variables names are : ", colnames(object@pseudo_data), "\n")
  cat("The variables ", object@margins, " have a known copula  given by :\n")
  writeLines(paste("\t", capture.output(show(object@known_cop)), sep = ""))
})
setMethod(f = "rCopula", signature = c(n = "numeric", copula = "cbkmCopula"), definition = function(n, copula) {

            # if n=0, return a 0xdim(copula) matrix :
            if (n == 0) {
              return(matrix(NA, nrow = 0, ncol = dim(copula)))
            }

            # get copula infos :
            J <- copula@margins
            d = dim(copula)
            p = length(J)
            m = copula@m
            boxes <- copula@precalc$pCopula$box_inf
            n_box <- nrow(boxes)
            weights <- copula@precalc$pCopula$weights

            # Preliminary : a `sample` function more efficient (cf ?sample,
            # exemples)
            resample <- function(x, ...) x[sample.int(length(x), ...)]

            # First step : simulate the known copula model.
            rez <- matrix(NA,nrow=n,ncol=d)
            rez[,J] <- rCopula(n, copula@known_cop)

            # Second step : Calculate the boxes that corespond to thoose simulations
              # find out wich boxes were simulated on the J part :
              simu_boxes <- floor(rez[, J] * m)/m
              # get the corresponding boxes number
              simu_boxes_nb <- apply(simu_boxes,1,function(x){
                # several boxes are avaliable with thoose J coordinates.
                possibles_boxes <- which(colSums(t(boxes[,J]) == x) == p)

                # Differenciate simulation fallen in existing boxes et
                # simulations outside existing boxes. Indeed, the checkerboard part of
                # the simulation will be delt with differently on thoose 2 cases.

                # We will construct an exeptional box for that :
                # sample one of thoose with the weights, OR the [0,1]^(d-p) box if all weights are 0
                if(sum(weights[possibles_boxes]) == 0){
                  return(n_box+1) # we return the box number n_box+1
                } else {
                  return(resample(possibles_boxes,size=1,prob = weights[possibles_boxes],replace = TRUE))
                }
              })

              #construct the exeptional box :
              boxes <- rbind(boxes, rep(0,d))
              boxes_sup <- rbind(boxes+1/m, rep(1,d))

              # then simulate from thoose boxes :
              inf_seuil <- boxes[simu_boxes_nb,-J]
              sup_seuil <- boxes_sup[simu_boxes_nb,-J]
              rng <- matrix(runif((d - p) * n), nrow = n, ncol=d-p)
              rez[, -J] <- inf_seuil + rng * (sup_seuil - inf_seuil)

            return(rez)

          })
setMethod(f = "pCopula", signature = c(u = "matrix", copula = "cbkmCopula"),  definition = function(u, copula) {
            # this function implements the formula for the mesure of the copula
            # given in the paper.  remind that pCopula and dCopula generics already
            # transform inputs into matrices...

            # could be much better vectorised...


            if (ncol(u) != dim(copula)) {
              stop("the input value must be coerçable to a matrix with dim(copula) columns.")
            }

            # Prerequisites :
            J <- copula@margins
            d = dim(copula)
            p = length(J)
            m = copula@m
            boxes <- copula@precalc$pCopula$box_inf
            weights <- copula@precalc$pCopula$weights
            y_min = rep(0, d)


            # Function that does the calculations for one value of u :
            unit_calculation <- function(u){
              # Let's calculate the intersection of [0,u] with boxes :
              intersections <- apply(boxes, 1, function(box_inf) {
                suppressWarnings(intersect(x_min = box_inf,
                                           x_max = box_inf + 1/m,
                                           y_min = y_min,
                                           y_max = u))
              })

              # Contribution of empty intersections will clearly be zero
              are_empty <- sapply(intersections, is.null)
              intersections <- intersections[!are_empty]
              inter_min <- sapply(intersections,function(x){x$min}) # d x nb_inter matrix
              inter_max <- sapply(intersections,function(x){x$max}) # d x nb_inter matrix

              # mesure of the known copula on it's margins, per box :
              mes_known <- vCopula(t(inter_min[J,]),t(inter_max[J,]),copula@known_cop)

              # lebegue copula measure on it's margins, per box :
              mes_lebesgue <- apply(inter_max[-J,] - inter_min[-J,],2,prod)*(m^(d-p))

              # final value :
              sum(mes_known * mes_lebesgue * weights[!are_empty])
            }

            # Applying :
            if (nrow(u) > 1) {
              return(apply(u, 1, unit_calculation))
            } else {
              unit_calculation(u)
            }

          })
setMethod(f = "dCopula", signature = c(u = "matrix", copula = "cbkmCopula"),  definition = function(u, copula) {
  stop("Checkerboard copula with known margins has no density")
})



