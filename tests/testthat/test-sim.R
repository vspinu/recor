library(qlcMatrix)
library(proxy)
library(testthat)
source("~/VC/RecommendR/R/weight.R")
source("~/VC/RecommendR/R/norm.R")
source("~/VC/RecommendR/R/simMethods.R")

m <- .mat(, 3, 5)
sp <- .sp(, 3, 5)

m1 <- .mat(1, 1000, 1000)
sp1 <- .sp(1, 1000, 1000)

m2 <- .mat(2, 1000, 1000)
sp2 <- .sp(2, 1000, 1000)

expect_equal(cosSparse(sp), simCosine(sp))
expect_equal(cosSparse(t(sp)), simCosine(sp, by_rows = T))

expect_equal(cosSparse(sp1, sp2), simCosine(sp1, sp2))
expect_equal(cosSparse(t(sp1), t(sp2)), simCosine(sp1, sp2, by_rows = T))

expect_equal(simCosine(sp1, m2), simCosine(sp1, sp2))
expect_equal(simCosine(m2, sp1), simCosine(sp2, sp1))
expect_equal(simCosine(m2, sp1), cosSparse(m2, sp1))

expect_equal(cosSparse(sp2, sp1), t(cosSparse(sp1, sp2)))

expect_equal(cosSparse(sp), simCosine(sp))
## expect_equal(as.matrix(simil(m, by_rows = F, method = "cosine"), diag = 1),
##              as.matrix(simCosine(sp)))

## as.matrix(simil(m, by_rows = F, method = "cosine"), diag = NA)
## as.matrix(simil(t(m), by_rows = T, method = "cosine"), diag = 1)

expect_equal(as.matrix(simil(m, by_rows = F, method = "cosine"), diag = 1),
             as.matrix(simCosine(sp, by_rows =  F)))

round(as.matrix(simil(t(m), by_rows = F, method = "cosine"), diag = 1) - 
          as.matrix(simCosine(sp, by_rows =  T)), 10)

expect_equal(as.matrix(simil(m, method = "cosine"), diag = 1),
             as.matrix(simCosine(t(sp))))

expect_equal(simCosine(sp, by_rows = T), simCosine(t(sp)))
expect_equal(as.matrix(simCosine(sp, by_rows = T)), as.matrix(simCosine(t(sp))))

expect_equal(as.matrix(simil(m, method = "cosine"), diag = 1), 
                 as.matrix(simil(t(m), by_rows = F, method = "cosine"), diag = 1))

## expect_equal(simil(m, m2, method = "cosine"), as.matrix(simCosine(m, m2)))
## expect_equal(simil(sp, method = "cosine"), as.matrix(simCosine(m, m2)))

expect_equal(cosSparse(sp, weight = idf), simCosine(sp, weight = weightIDF))
expect_equal(cosSparse(sp, sp2,  weight = idf), simCosine(sp, sp2,  weight = weightIDF))

## expect_equal(simCosine(sp, sp), simCosine(sp))

library(microbenchmark)
microbenchmark(cosSparse(sp), simCosine(sp))
microbenchmark(cosSparse(t(sp)), simCosine(sp, by_rows = T))
microbenchmark(cosSparse(sp, weight = idf), simCosine(sp, weight = weightIDF))
microbenchmark(simCosine(sp, sp), simCosine(sp))

