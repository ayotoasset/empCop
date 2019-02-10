context("cbCopula tests")
suppressWarnings(library(copula))
library(empCop)
data(LifeCycleSavings)
data(SMI.12)
data(gasoil)
cop <- cbCopula(LifeCycleSavings,m=10)
u=matrix(rep(0,15),ncol=5)
v=matrix(seq(0,1,length.out=15),ncol=5)
w=matrix(rep(1,15),ncol=5)

test_that("zero-row or null data.frame are coerced to indepcopula", {
  expect_error(cbCopula(as.data.frame(NULL)))
  expect_equal(cbCopula(as.data.frame(matrix(0,nrow=0,ncol=5))),indepCopula(5))
})








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

cbkmCopula = function(x, m = nrow(x), pseudo = FALSE, margins_numbers = NULL,
                      known_cop = NULL) {
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
setMethod(f = "show", signature = c(object = "cbkmCopula"), definition = function(object) {
  cat("This is a cbkmCopula , with : \n", "  dim =", dim(object), "\n   n =",
      nrow(object@pseudo_data), "\n   m =", object@m, "\n")
  cat("The variables names are : ", colnames(object@pseudo_data), "\n")
  cat("The variables ", object@margins, " have a known copula  given by :\n")
  writeLines(paste("\t", capture.output(show(object@known_cop)), sep = ""))
})
setMethod(f = "rCopula", signature = c(n = "numeric", copula = "cbkmCopula"),
          definition = function(n, copula) {

            # if n=0, return a 0xdim matrix :
            if (n == 0) {
              return(matrix(NA, nrow = 0, ncol = ncol(copula@pseudo_data)))
            }

            # Preliminary : a `sample` function more efficient (cf ?sample,
            # exemples)
            resample <- function(x, ...) x[sample.int(length(x), ...)]

            # Préliminary : supression of row.names
            row.names(copula@pseudo_data) <- NULL

            # Préliminary : calculation of boxes
            seuil_inf = floor(copula@pseudo_data * copula@m)/copula@m
            seuil_sup = seuil_inf + 1/copula@m


            # First step : simulate the known copula model.
            simu_known_cop <- rCopula(n, copula@known_cop)

            # Second step : Calculate the boxes that corespond to thoose
            # simulations, and add to them the number of rows corresponding to
            # observations (if they exist)
            simulated_box_with_row_number <- (floor(simu_known_cop * copula@m)/copula@m) %>%
              as.data.frame()
            colnames(simulated_box_with_row_number) <- colnames(copula@pseudo_data)[copula@margins]
            simulated_box_with_row_number <- simulated_box_with_row_number %>%
              mutate(n_sim = 1:n) %>% left_join(as.data.frame(seuil_inf[,
                                                                        copula@margins]) %>% mutate(num_row = 1:nrow(seuil_inf)))

            # Third step : Differenciate simulation fallen in existing boxes et
            # simulations outside existing boxes. Indeed, the checkerboard part of
            # the simulation will be delt with differently on thoose 2 cases.

            # Les simulations qui sont tombées dans des boites existantes : For
            # simulations that felt in existing boxes, we need to choose a line to
            # attach them alowing us to simulate in the right box.
            rows_not_na <- simulated_box_with_row_number %>% filter(!is.na(num_row)) %>%
              group_by(n_sim) %>% summarise(num_row = resample(num_row, 1))

            # Corresponding Bounds
            seuil_inf_not_na <- cbind(rows_not_na, seuil_inf[rows_not_na$num_row,
                                                             -copula@margins])
            seuil_sup_not_na <- cbind(rows_not_na, seuil_sup[rows_not_na$num_row,
                                                             -copula@margins])

            # For simulations that did NOT felt in existing boxes, we simulate with
            # all possible range, i.e the Bounds are 0 and 1 for the checkerboard
            # part.
            rows_na <- simulated_box_with_row_number %>% filter(is.na(num_row)) %>%
              select(colnames(rows_not_na))

            # Bounds for non-existing boxes : 0 or 1.
            seuil_inf_na <- matrix(0, ncol = ncol(copula@pseudo_data) - length(copula@margins),
                                   nrow = nrow(rows_na)) %>% magrittr::set_colnames(colnames(seuil_inf[,
                                                                                                       -copula@margins])) %>% {
                                                                                                         cbind(rows_na, .)
                                                                                                       }
            seuil_sup_na <- matrix(1, ncol = ncol(copula@pseudo_data) - length(copula@margins),
                                   nrow = nrow(rows_na)) %>% magrittr::set_colnames(colnames(seuil_inf[,
                                                                                                       -copula@margins])) %>% {
                                                                                                         cbind(rows_na, .)
                                                                                                       }

            # FInaly, grouping thoose bounds :
            seuil_inf_final <- rbind(seuil_inf_na, seuil_inf_not_na) %>% arrange(n_sim) %>%
              select(-n_sim, -num_row)
            seuil_sup_final <- rbind(seuil_sup_na, seuil_sup_not_na) %>% arrange(n_sim) %>%
              select(-n_sim, -num_row)


            # Fourth step : simulate the checkerboard part via uniforms :
            rng <- matrix(runif((ncol(copula@pseudo_data) - length(copula@margins)) *
                                  n), nrow = n, byrow = FALSE)
            simu_checkerboard <- as.data.frame(seuil_inf_final + rng * (seuil_sup_final -
                                                                          seuil_inf_final))


            # Last step : add the known part of the simulation and return the
            # result :
            simu_known_cop %>% magrittr::set_colnames(colnames(copula@pseudo_data)[copula@margins]) %>%
              cbind(simu_checkerboard) %>% select(colnames(copula@pseudo_data)) %>%
              return

          })
setMethod(f = "pCopula", signature = c(u = "matrix", copula = "cbkmCopula"),
          definition = function(u, copula) {
            # this function implements the formula for the mesure of the copula
            # given in the paper.  remind that pCopula and dCopula generics already
            # transform inputs into matrices...
            if (ncol(u) != dim(copula)) {
              stop("the input value must be coerçable to a matrix with dim(copula) columns.")
            }
            if (nrow(u) > 1) {
              return(apply(u, 1, pCopula, copula))
            }

            J <- copula@margins
            d = dim(copula)
            k = length(J)
            m = copula@m
            boxes <- copula@precalc$pCopula$box_inf
            weights <- copula@precalc$pCopula$weights

            # Let's calculate the intersection of [0,u] with boxes :
            y_min = rep(0, d)
            intersections <- apply(boxes, 1, function(box_infi) {
              suppressWarnings(intersect(x_min = box_infi, x_max = box_infi +
                                           1/m, y_min = y_min, y_max = u))
            })

            # Contribution of empty intersections will clearly be zero
            are_empty <- sapply(intersections, is.null)
            intersections <- intersections[!are_empty]

            # mesure of the known copula on it's margins, per box :
            mes_known <- sapply(intersections, function(inter) {
              vCopula(inter$min[J], inter$max[J], copula@known_cop)
            })

            # lebegue copula measure on it's margins, per box :
            mes_lebesgue <- sapply(intersections, function(inter) {
              prod(inter$max[-J] - inter$min[-J])
            }) * (m^(d - k))  # renormalised by size of a -J box

            # final value :
            sum(mes_known * mes_lebesgue * weights[!are_empty])

          })

