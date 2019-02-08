#' @import copula
#' @import magrittr
#' @import dplyr
NULL

############ Generics #####
#' Copula volume on hyper-boxes
#'
#' @param u numeric matrix : minimum point of the hyper-rectangles, one row per observation.
#' @param v numeric matrix : maximum point of the hyper-rectangle, one row per observation.
#' @param copula the copula to calcule it's measure on [u,v]
#' @param ... other parameter to be passed to methods for this generic.
#'
#' u must be piecewise smaller than v, otherwise the function will return an error.
#'
#' A method is currently implemented for the main virtual class "Copula", but it assumes
#' that a pCopula method is avaliable for the given copula.
#'
#' This function calculates the measure of the copula according to the algorythme proposed by :
#' Umberto Cherubini & Silvia Romagnoli (2009) Computing the
#' Volume of n-Dimensional Copulas, Applied Mathematical Finance, 16:4, 307-314, DOI:
#'   10.1080/13504860802597311 link : http://dx.doi.org/10.1080/13504860802597311
#'
#'
#'
#' @return the measure of the copula
#' @export
#'
#' @examples
#' # For a simple one-dimentional input :
#' cop = copula::archmCopula("Clayton",0.7,3)
#' vCopula(rep(0,3),rep(1,3),cop)
#'
#' # the function is vectorised :
#' v=matrix(seq(0,1,length.out=12),ncol=3)
#' v=matrix(rep(0,12),ncol=3)
#' vCopula(u,v,cop)
setGeneric("vCopula", function(u, v, copula, ...) {

  # taken from the generic of pCopula, does mainly the same...

  if(!is.matrix(u)) u <- rbind(u, deparse.level = 0L)
  ## here as well, 'outside' and 'on-boundary' are equivalent:
  u[] <- pmax(0, pmin(1, u))

  if(!is.matrix(v)) v <- rbind(v, deparse.level = 0L)
  ## here as well, 'outside' and 'on-boundary' are equivalent:
  v[] <- pmax(0, pmin(1, v))

  standardGeneric("vCopula")
})


############################### Empirical copula class ######
#' Empirical Copula class (virtual mother class)
#'
#' @slot pseudo_data matrix : pseudo_data that the empirical copula is based on.
#'
#' @export
setClass(Class = "empiricalCopula",
         contains=c("VIRTUAL","Copula"),
         slots = c(pseudo_data = "data.frame"),
         validity = function(object){
           errors <- c()
           if(prod(apply(object@pseudo_data,1:2,is.numeric))!=1){
             errors <- c(errors,"The data argument must be a numeric data.frame")
           }
           if(prod(object@pseudo_data <= 1)*prod(object@pseudo_data >= 0)==0){
             errors <- c(errors,"The pseudo-data should be numeric between 0 and 1 (both included)")
           }
           if (length(errors) == 0) TRUE else errors
          })
setMethod(f="dim",  signature=(x="empiricalCopula"),      definition = function(x){
  return(ncol(x@pseudo_data))
})

############################### Checkerboard copula class #######
.cbCopula = setClass(Class = "cbCopula",
          contains = "empiricalCopula",
          slots = c(m = "numeric"),
          validity = function(object){
            errors <- c()
            if((nrow(object@pseudo_data)%%object@m) != 0 ){
              errors <- c(errors,"m should divide the number of row")
            }
            if (length(errors) == 0) TRUE else errors
          })
#' cbCopula contructor
#'
#' @param x the data to be used
#' @param m checkerboard parameter
#' @param pseudo Boolean, defaults to `FALSE`. Set to `TRUE` if you are already providing pseudo datas into the `x` argument.
#'
#' @return a cbCopula object
#' @export
cbCopula = function(x,m=nrow(x),pseudo=FALSE){
  if(missing(x)){
    stop("The argument x must be provided")
  }
  if(!pseudo){
    x <- apply(x,2,rank,na.last="keep")/(nrow(x)+1)
  }
  .cbCopula(pseudo_data=as.data.frame(x),m=m)
}
setMethod(f="show",    signature=c(object="cbCopula"),            definition = function(object){
            cat("This is a cbCopula , with : \n",
            "  dim =",dim(object), "\n   n =",nrow(object@pseudo_data),"\n   m =",object@m,"\n")
            cat("The variables names are : ", colnames(object@pseudo_data))
          })
setMethod(f="rCopula", signature=c(n="numeric",copula="cbCopula"),   definition = function(n,copula){

  # The parameter n represent the number of generated values.
  # if n=0, return a 0xdim matrix :
  if(n==0){
    return(matrix(NA,nrow=0,ncol=ncol(copula@pseudo_data)))
  }
            # The pseudo_data should be a matrix or a dataframe,
            # with one row per value and one column per variable.

            # The parameter m represent the size of the grid : bigger m means smaller grid.

            # The structure (column names) will be preserved in the output, the row_names will
            # indicate the riginal row that builded the box and the result will be converted
            # to data.frame

            # First, let's define the boxes containing the pseudo observations :
            # Since it's a checkerboard, the boxes a regular with side length 1/m,
            # so calculating the right boxes is easy :
            seuil_inf = floor(copula@pseudo_data*copula@m)/copula@m
            seuil_sup = seuil_inf+1/copula@m

            # Then, let's sample rows coresponding to observations, i.e let's sample thoose boxes with
            # probabilities proportional to the number of observations inside the box.
            # notes that boxes with probability 0, i.e without observation, were not included here.
            # This makes the algorythme fast.
            rows <- sample(x = 1:nrow(copula@pseudo_data),size=n,replace = TRUE)
            seuil_inf <- seuil_inf[rows,]
            seuil_sup <- seuil_sup[rows,]

            # Finaly, sample some random uniform, and bound them inside the sampled boxes :
            rng <- matrix(runif(ncol(copula@pseudo_data)*n),nrow=n,byrow=FALSE)
            result <- as.data.frame(seuil_inf+rng*(seuil_sup-seuil_inf))
            rownames(result)<-NULL
            colnames(result)<- NULL
            result <- as.matrix(result)

            return(result)
          })
setMethod(f="pCopula", signature=c(u="matrix",copula="cbCopula"), definition = function(u,copula){

            # remind that pCopula and dCopula generics already transform inputs into matrices...

            if(ncol(u) != dim(copula)){
              stop("the input value must be coerçable to a matrix with dim(copula) columns.")
            }

              seuil_inf = floor(copula@pseudo_data*copula@m)/copula@m
              d=dim(copula)
              n=nrow(copula@pseudo_data)
              rez <- vector(length = nrow(u))

              for(i in 1:nrow(u)){
                ponderation <- t(apply(seuil_inf,1,function(y){u[i,]-y}))
                ponderation <- pmax(pmin(ponderation,1/copula@m),0)
                rez[i] <- sum(apply(ponderation,1,function(v){
                  (prod(v)*copula@m^d)
                }))/n
              }

              return(rez)
          })

############################### Checkerboard with known margin copula class #######
.cbkmCopula = setClass(Class = "cbkmCopula",
                     contains = "empiricalCopula",
                     slots = c(m = "numeric",
                               margins="numeric",
                               known_cop="Copula",
                               box_inf = "matrix",
                               precalc = "list"),
                     validity = function(object){
                       errors <- c()
                       if((nrow(object@pseudo_data)%%object@m) != 0 ){
                         errors <- c(errors,"m should divide the number of row")
                       }
                       if(length(object@margins) != dim(object@known_cop)){
                         errors <- c(errors,"number of known margins hsuld be equal to dimention of known copula")
                       }
                       if(length(object@margins) > ncol(object@pseudo_data)){
                         errors <- c(errors,"the number of known margins should be smaller than the number of total margins in the empirical data !!")
                       }
                       if(!all(object@margins %in% 1:ncol(object@pseudo_data))){
                         errors <- c(errors,"provided margins number should be smaller than the number of dimention of empirical data")
                       }
                       if (length(errors) == 0) TRUE else errors
                     }
)
#' cbkmCopula contructor
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
#' true_copula <- onacopulaL(family = "Clayton",
#' nacList = list(iTau(getAcop("Clayton"), 0.6), 1:4))
#'
#' dataset <- rCopula(50,true_copula)
#'
#' known_margins <- c(2,3)
#' known_clayton <- onacopulaL(
#'   family = "Clayton",
#'   nacList = list(iTau(getAcop("Clayton"), 0.6), 1:2)
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
cbkmCopula = function(x,m=nrow(x),pseudo=FALSE,margins_numbers=NULL,known_cop=NULL){
  if(missing(x)){
    stop("The argument x must be provided")
  }
  if((is.null(known_cop) && (!is.null(margins_numbers))) || (is.null(known_cop) && (!is.null(margins)))){
    stop("known_cop argument and margins argument must both be provided.")
  }

  if(!pseudo){
    x <- apply(x,2,rank,na.last="keep")/(nrow(x)+1)
  }
  if(all(is.null(known_cop),is.null(margins_numbers))){
    .cbCopula(pseudo_data=as.data.frame(x),m=m)
  } else {

    ######## pCopula precalculations :
        message("Doing precalculations...")
        # construct boxes :
        box_inf <- do.call(expand.grid,lapply(1:ncol(x),function(x){seq(0,1-1/m,length=m)}))
        attr(box_inf,"out.attrs") <- NULL
        box_inf <- as.matrix(box_inf)
        colnames(box_inf) <- NULL

        # now calculate the empirical measure for each box :

        seuil_inf <- as.matrix(floor(x*m)/m)

        nb_emp <- sapply(1:(m^ncol(x)),function(i){
          sum(apply(seuil_inf,1,function(x){
            all(round(m*x,0) == round(m*box_inf[i,],0))
          }))
        })

        # idem pour les cases J :
        nb_emp_J <- sapply(1:(m^ncol(x)),function(i){
          sum(apply(seuil_inf,1,function(x){
            all(round(m*x[margins_numbers],0) == round(m*box_inf[i,margins_numbers],0))
          }))
        })

        weights <- vector(length = length(nb_emp))
        weights[nb_emp_J != 0] <- nb_emp[nb_emp_J != 0] / nb_emp_J[nb_emp_J != 0]
        weights[nb_emp_J == 0] <- m^(length(margins_numbers)-ncol(x))

        pCopula_precalculations <- list(box_inf = box_inf,
                                        nb_emp = nb_emp,
                                        nb_emp_J = nb_emp_J,
                                        weights=weights)
    ######## Returning the objec :
    message("Done !")
    .cbkmCopula(pseudo_data=as.data.frame(x),
                m=m,
                margins=margins_numbers,
                known_cop=known_cop,
                box_inf=box_inf,
                precalc = list(pCopula = pCopula_precalculations))
  }


}
setMethod(f="show",   signature=c(object="cbkmCopula"),            definition = function(object){
  cat("This is a cbkmCopula , with : \n",
      "  dim =",dim(object), "\n   n =",nrow(object@pseudo_data),"\n   m =",object@m,"\n")
  cat("The variables names are : ", colnames(object@pseudo_data),"\n")
  cat("The variables ",object@margins," have a known copula  given by :\n")
  writeLines(paste("\t", capture.output(show(object@known_cop)), sep=""))
})
setMethod(f="rCopula",signature=c(n="numeric",copula="cbkmCopula"),definition = function(n,copula){

    # if n=0, return a 0xdim matrix :
    if(n==0){
      return(matrix(NA,nrow=0,ncol=ncol(copula@pseudo_data)))
    }

    # Preliminary : a `sample` function more efficient (cf ?sample, exemples)
    resample <- function(x, ...) x[sample.int(length(x), ...)]

    # Préliminary : supression of row.names
    row.names(copula@pseudo_data) <- NULL

    # Préliminary : calculation of boxes
    seuil_inf = floor(copula@pseudo_data*copula@m)/copula@m
    seuil_sup = seuil_inf+1/copula@m


    # First step : simulate the known copula model.
    simu_known_cop <- rCopula(n,copula@known_cop)

    # Second step : Calculate the boxes that corespond to thoose simulations, and add to
    # them the number of rows corresponding to observations (if they exist)
    simulated_box_with_row_number <- (floor(simu_known_cop*copula@m)/copula@m) %>%
      as.data.frame()
    colnames(simulated_box_with_row_number) <- colnames(copula@pseudo_data)[copula@margins]
    simulated_box_with_row_number <- simulated_box_with_row_number %>%
      mutate(n_sim=1:n) %>%
      left_join(
        as.data.frame(seuil_inf[,copula@margins]) %>%
          mutate(num_row=1:nrow(seuil_inf))
      )

    # Third step : Differenciate simulation fallen in existing boxes et simulations outside
    # existing boxes. Indeed, the checkerboard part of the simulation will be delt with
    # differently on thoose 2 cases.

    # Les simulations qui sont tombées dans des boites existantes :
    # For simulations that felt in existing boxes, we need to choose a line to attach them
    # alowing us to simulate in the right box.
    rows_not_na <- simulated_box_with_row_number %>%
      filter(!is.na(num_row)) %>%
      group_by(n_sim) %>%
      summarise(num_row=resample(num_row,1))

    # Corresponding Bounds
    seuil_inf_not_na <- cbind(rows_not_na,seuil_inf[rows_not_na$num_row,-copula@margins])
    seuil_sup_not_na <- cbind(rows_not_na,seuil_sup[rows_not_na$num_row,-copula@margins])

    # For simulations that did NOT felt in existing boxes, we simulate with all possible
    # range, i.e the Bounds are 0 and 1 for the checkerboard part.
    rows_na <- simulated_box_with_row_number %>%
      filter(is.na(num_row)) %>%
      select(colnames(rows_not_na))

    # Bounds for non-existing boxes : 0 or 1.
    seuil_inf_na <-
      matrix(0,ncol=ncol(copula@pseudo_data)-length(copula@margins),nrow=nrow(rows_na)) %>%
      magrittr::set_colnames(colnames(seuil_inf[,-copula@margins])) %>%
      {cbind(rows_na,.)}
    seuil_sup_na <-
      matrix(1,ncol=ncol(copula@pseudo_data)-length(copula@margins),nrow=nrow(rows_na)) %>%
      magrittr::set_colnames(colnames(seuil_inf[,-copula@margins])) %>%
      {cbind(rows_na,.)}

    # FInaly, grouping thoose bounds :
    seuil_inf_final <- rbind(seuil_inf_na,seuil_inf_not_na) %>% arrange(n_sim) %>% select(-n_sim,-num_row)
    seuil_sup_final <- rbind(seuil_sup_na,seuil_sup_not_na) %>% arrange(n_sim) %>% select(-n_sim,-num_row)


    # Fourth step : simulate the checkerboard part via uniforms :
    rng <- matrix(runif((ncol(copula@pseudo_data)-length(copula@margins))*n),nrow=n,byrow=FALSE)
    simu_checkerboard <- as.data.frame(seuil_inf_final+rng*(seuil_sup_final-seuil_inf_final))


    # Last step : add the known part of the simulation and return the result :
    simu_known_cop %>%
      magrittr::set_colnames(colnames(copula@pseudo_data)[copula@margins]) %>%
      cbind(simu_checkerboard) %>%
      select(colnames(copula@pseudo_data)) %>%
      return

})
setMethod(f="pCopula", signature=c(u="matrix",copula="cbkmCopula"), definition = function(u,copula){
  # this function implements the formula for the mesure of the copula given in the paper.
  # remind that pCopula and dCopula generics already transform inputs into matrices...
  if(ncol(u) != dim(copula)){
    stop("the input value must be coerçable to a matrix with dim(copula) columns.")
  }
  if(nrow(u) > 1){
    return(apply(u,1,pCopula,copula))
  }

  J <- copula@margins
  d=dim(copula)
  k=length(J)
  m=copula@m
  boxes <- copula@precalc$pCopula$box_inf
  weights <- copula@precalc$pCopula$weights

  # Let's calculate the intersection of [0,u] with boxes :
  y_min = rep(0,d)
  intersections <- apply(boxes,1,function(box_infi){
    suppressWarnings(intersect(x_min=box_infi,
                               x_max=box_infi+1/m,
                               y_min = y_min,
                               y_max=u))
  })

  # Contribution of empty intersections will clearly be zero
  are_empty <- sapply(intersections,is.null)
  intersections <- intersections[!are_empty]

  # mesure of the known copula on it's margins, per box :
  mes_known <- sapply(intersections,function(inter){
    vCopula(inter$min[J],inter$max[J],copula@known_cop)
  })

  # lebegue copula measure on it's margins, per box :
  mes_lebesgue <- sapply(intersections,function(inter){
    prod(inter$max[-J]-inter$min[-J])
  })*(m^(d-k)) # renormalised by size of a -J box

  # final value :
  sum(mes_known * mes_lebesgue * weights[!are_empty])

})

############################### ConvexComCopula class #######


#' ConvexCombCopula class
#'
#' @slot copulas list of copulas of same dimension
#' @slot alpha numeric. A vector of (positive) weights.
#'
#' @return a ConvexCombCopula object.
#' @export
.ConvexCombCopula = setClass(Class = "ConvexCombCopula",
                       contains = "Copula",
                       slots = c(copulas = "list",
                                 alpha="numeric"),
                       validity = function(object){
                         errors <- c()
                         if(length(object@copulas) != length(object@alpha)){
                           errors <- c(errors,"the weights parameter alpha must have same length as the copulas list")
                         }
                         if(!all(sapply(object@copulas,function(x){is(x,"Copula")}))){
                           errors <- c(errors,"parameter copulas should contains a list of copulas")
                         }
                         if(!all(object@alpha>=0)){
                           errors <- c(errors,"weights should be positive")
                         }
                         if(!(length(unique(sapply(object@copulas,dim))) == 1)){
                           errors <- c(errors,"all copulas must have same dimension")
                         }
                         if (length(errors) == 0) TRUE else errors
                       }
)
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
#'   copula::archmCopula("gumbel",3,dim=2),
#'   copula::archmCopula("clayton",-1,dim=2)
#' )
#' alpha <- c(1,4)
#'
#' (cop <- ConvexCombCopula(copulas,alpha))
#'
#' plot(rCopula(100,cop))
#' pCopula(c(0.5,0.7),cop)
ConvexCombCopula = function(copulas,alpha=rep(1,length(copulas))){
  if(missing(copulas) || (!is(copulas,"list"))){
    stop("The argument copulas must be provided as a list of copulas")
  }
  .ConvexCombCopula(copulas=copulas,alpha=alpha)
}
setMethod(f="dim",    signature=(x="ConvexCombCopula"),                  definition = function(x){
  return(dim(x@copulas[[1]]))
})
setMethod(f="show",   signature=c(object="ConvexCombCopula"),            definition = function(object){
  cat("This is a ConvexCombCopula , with : \n",
      "  dim =",dim(object@copulas[[1]]), "\n   number of copulas =",length(object@copulas),"\n   alpha =",object@alpha,"\n")
  cat("sub-copulas can be accessed trhough the @copulas slot")
})
setMethod(f="rCopula",signature=c(n="numeric",copula="ConvexCombCopula"),definition = function(n,copula){

  # to choose wich copulas will be simulated from,
  # sample 1:length(copulas) with weights equal to alpha, with replacement OFC
  n_cop = length(copula@copulas)
  sampled_copulas <- sample(1:n_cop,size = n,replace = TRUE,prob = copula@alpha)

  # then sample from each of thoose copulas the right number of times :
  samples <- mapply(
    function(cop,how_much){
      rCopula(n=how_much,copula=cop)
    },
    copula@copulas,
    sapply(1:n_cop,function(x){sum(x == sampled_copulas)})
  )

  # then rbind all of them and mix rows :
  samples <- do.call(rbind,samples)
  samples <- samples[sample(1:nrow(samples),size = nrow(samples),replace = FALSE),]
  return(samples)
})
setMethod(f="pCopula",signature=c(u="matrix",copula="ConvexCombCopula"),definition = function(u,copula){

  # remind that pCopula and dCopula generics already transform inputs into matrices...
  # remind that pCopula and dCopula generics already transform inputs into matrices...
  if(ncol(u) != dim(copula)){
    stop("the input value must be coerçable to a matrix with dim(copula) columns.")
  }

  outputs <- lapply(copula@copulas,function(cop){ pCopula(u,cop) })

  rez     <- outputs[[1]]*copula@alpha[1]
  for(i in 2:length(outputs)){
    rez <- rez + outputs[[i]]*copula@alpha[i]
  }

  return(rez)

})


# Okay.

# next step will be to use this class to perform an automatic-weighting of the copulas;

# e.g we could perform a weighting by looking at some distance on the copula space to
# the empirical copula of some data.
# 2 questions arises naturaly :
  # Q1 : Wich distance ?
  # Q2 : how to be shure we are not overfitting with this distance ? i.e penalisation.

# So we need a distance on the copula space, and some tests based on it.
# The distance of the model to the empirical copula on the empirical points will be
# our "goodness of fit" measure.

# We need to implement distances on the copula space to the empirical copula... This seems rather dificult

# We also need to implement the pcopula function for all of thoose copulas.

# for exemple, we could :

# 1) use the quadratic distance on a pseudo dataset? with bootstrap of this dataset ?

#


############################### Helpers functions #####

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  binary_vector[-(1:(length(binary_vector) - noBits))]
}


setMethod("vCopula",signature = c(u="matrix",v="matrix",copula="Copula"),definition = function(u,v,copula){

  # can handle any copula thant pCopula could handle.

  # u and v must be numeric, copula must be a copula,
  # and v must be smaller than u
  if(nrow(u) != nrow(v)){
    stop("u and v must have same shape (same number of row and columns)")
  }
  if(nrow(u) > 1){
    # recursive if asked for more than one.
    return(sapply(1:nrow(u),function(i){vCopula(u[i,],v[i,],copula)}))
  }

  if(any(v<u)){
    stop("u must be smaller than v !")
  }
  if(any(u==v)){
    return(0)
  }

  d=dim(copula)
  rez <- vector(length=(2^d-1))

  for(i in 0:(2^d-1)){
    p <- number2binary(i,d)
    rez[i+1] <- (-1)^sum(p) * pCopula(u*p + v*(1-p),copula)
  }

  return(sum(rez))

})

intersect <- function(x_min,x_max,y_min,y_max){
  # retourne l'intersection es deux rectangles
  # [x_min,x_max] et [y_min,y_max]

  # sous la forme d'un rectangle
  #[rez_min,rez_max]



  # pour chaque dimensions, il faut prendre le maximum des min et le minimum des max.
  rez <- list()
  rez$min <- pmax(x_min,y_min)
  rez$max <- pmin(x_max,y_max)

  # checker que c'est pas l'ensemble vide :
  if(any(rez$min > rez$max)){
    warning("Ensemble vide ! On return NULL.")
    return(NULL)
  }

  return(rez)
}


#################################### End


























