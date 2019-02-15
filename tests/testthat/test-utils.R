# context("utils.cpp tests")
# suppressWarnings(library(copula))
# library(empCop)
# data(LifeCycleSavings)
# data(SMI.12)
# data(gasoil)
# cop <- cbCopula(LifeCycleSavings,m=10)
# u=matrix(rep(0,15),ncol=5)
# v=matrix(seq(0,1,length.out=15),ncol=5)
# w=matrix(rep(1,15),ncol=5)
#
#
# intersect_rectangles_R_framework <- function(x_min, x_max, y_min, y_max) {
#   # retourne l'intersection es deux rectangles [x_min,x_max] et
#   # [y_min,y_max] sous la forme d'un rectangle [rez_min,rez_max]
#
#   rez <- list()
#   rez$min <- pmax(x_min, y_min)
#   rez$max <- pmin(x_max, y_max)
#
#   # checker que c'est pas l'ensemble vide :
#   if (any(rez$min > rez$max)) {
#     warning("Ensemble vide ! On return NULL.")
#     return(NULL)
#   }
#   return(rez)
# }
#
# test_that("values returnes by Cpp_intersect are as they should be", {
#   expect_equal(intersect_rectangles_R_framework(c(0,0,0),c(1,1,1),c(0.2,0.2,0.2),c(0.8,0.8,3)),
#                Cpp_intersect(c(0,0,0),c(1,1,1),c(0.2,0.2,0.2),c(0.8,0.8,3)))
#   expect_equal(intersect_rectangles_R_framework(c(0,0,0),c(1,1,1),c(0,0.2,0.2),c(0.8,0.8,3)),
#                Cpp_intersect(c(0,0,0),c(1,1,1),c(0,0.2,0.2),c(0.8,0.8,3)))
#
# })