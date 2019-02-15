context("cbkmCopula tests")
suppressWarnings(library(copula))
library(empCop)

## ----fig.cap="constructing a copula"-----------------------
true_copula <- onacopulaL(
  family = "Clayton",
  nacList = list(iTau(getAcop("Clayton"), 0.6), 1:4)
)
dataset <- rCopula(50,true_copula)
known_clayton <- onacopulaL(
  family = "Clayton",
  nacList = list(iTau(getAcop("Clayton"), 0.6), 1:2)
)
cop <- cbkmCopula(x = dataset,
                  m = 5,
                  pseudo = TRUE,
                  margins_numbers = c(2,3),
                  known_cop = known_clayton)

u=matrix(rep(0,12),ncol=4)
v=matrix(seq(0,1,length.out=12),ncol=4)
w=matrix(rep(1,12),ncol=4)

test_that("zero-row or null data.frame are coerced to indepcopula", {
  expect_error(cbkmCopula(as.data.frame(NULL),))
  expect_equal(cbkmCopula(as.data.frame(matrix(0,nrow=0,ncol=5))),indepCopula(5))
})

test_that("Inheritance and methods are there", {
  expect_s4_class(cop,"Copula")
  expect_s4_class(cop,"empiricalCopula")
})

test_that("non-dividors m are not allowed", {
  expect_error(cbkmCopula(dataset,m=3))
  expect_error(cbkmCopula(dataset,m=7))
  expect_error(cbkmCopula(dataset,m=11))
  expect_error(cbkmCopula(dataset,m=49))
})

test_that("dimention of cbCopula is equal dimention of data", {
  expect_equal(dim(cbCopula(dataset)),ncol(dataset))
})

test_that("pCopula values are between 0 and 1 with OK bounds.",{
  expect_true(all(pCopula(matrix(seq(0.3,1,length.out = 8),nrow=2),cop) > c(0,0)))
  expect_true(all(pCopula(matrix(seq(0.3,1,length.out = 8),nrow=2),cop) < c(1,1)))
  expect_equal(pCopula(matrix(seq(1,1,length.out = 8),nrow=2),cop),c(1,1))
  expect_equal(pCopula(matrix(seq(0,0,length.out = 8),nrow=2),cop),c(0,0))
})

test_that("vCopula did not change",{
  expect_equal(vCopula(u,v,cop)[1],0)
  expect_error(vCopula(v,u,cop))
  expect_equal(vCopula(u,w,cop),rep(1,3))
})

test_that("dim is ok",{
  expect_equal(dim(cop),4)
})

test_that("rCopula output is ok",{
  expect_equivalent(rCopula(0,cop),matrix(ncol=4,nrow=0))
  expect_is(rCopula(10,cop),"matrix")
  expect_equal(ncol(rCopula(10,cop)),dim(cop))
})

test_that("dCopula returns an error",{
  expect_error(dCopula(rep(0.5,dim(cop)),cop))
})

























