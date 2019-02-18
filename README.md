[![Travis build status](https://travis-ci.org/lrnv/empCop.svg?branch=master)](https://travis-ci.org/lrnv/empCop)

# Empirical Copulas

The goal of empCop is to give tools to work with Empirical checkerboard copula, empirical checkerboard copula with known margins and convex combinaisons of copulas. It uses the main classes and formal generics from the `copula` package. It aims at implmeenting a lot of empirical model to asses multivaraite dependance structures that are not already implement in the main `copula` package. 

## Installation

WARNING : This package is in active developpement an is clearly NOT stable. use with caution. If you are still not scared, you can install the released version of empCop from Github with :

``` r
devtools::install_github("lrnv/empCop")
```

## Example

Starting with the `LifeCycleSavings` dataset, we could first plot and then compute the checkerboard copula ith parameter $m=5$ of this dataset with the following code : 

``` r
data("LifeCycleSavings")
pseudo_data <- (apply(LifeCycleSavings,2,rank,ties.method="max")/(nrow(LifeCycleSavings)+1))
pairs(pseudo_data) # plot pairs of pseudo_data

cop <- cbCopula(x = pseudo_data,m = 5,pseudo = TRUE) # add pseudo=TRUE if you provided pseudo observation
```

And then this copula can be easily worked with, using for exemple `rCopula` to simulate, `pCopula` to calculate it's values, and other common generics from the `copula` package.

On the other hand, the empirical checkerboard copula with known margins can be constructed with the following code : 


``` r
  true_copula <- onacopulaL(
    family = "Clayton",
    nacList = list(iTau(getAcop("Clayton"), 0.6), 1:4)
  )
  dataset <- rCopula(50,true_copula) 
  colnames(dataset) <- c("u","v","w","x")
  pairs(dataset,lower.panel=NULL) # show the data that we start with

  known_clayton <- onacopulaL(
    family = "Clayton",
    nacList = list(iTau(getAcop("Clayton"), 0.6), 1:2)
  )

  cop <- cbkmCopula(x = dataset,m = 5,pseudo = TRUE,margins_numbers = c(2,3),known_cop = known_clayton)
```

And then, as before, this copula can be easily handled in the classical framework of `copula`'s S4 classes.

You may want to see the vignettes for more details about this models, include the mathematical definitions associated with them.