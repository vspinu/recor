
test_that("non0ix")

test_that("binarize works", {
    mat <- matrix(sample(c(rep.int(0, 1000), 0:100), 20000, T), 200)
    rrm <- Matrix(mat, sparse = T)
    ngc <- as(mat > 0, "ngCMatrix")
    bin <- binarize(rrm, 1)
    expect_equivalent(ngc, bin)
})

test_that("split know unknown works as expected",{
    .splitKU <-  RecommendR:::.splitKU
    rrm <- .rrm(, 100, 20)
    rrm[, 1] <- 1
    out <- .splitKU(rrm, 1L)
    expect_equal(rrm, out$known + out$unknown)
    expect_true(all(rowSums(out$known > 0) == 1L))

    rrm[, 2:3] <- 3
    out <- .splitKU(rrm, 3)
    expect_equivalent(rrm, out$known + out$unknown)
    expect_true(all(rowSums(out$known > 0) == 3L))

    out <- .splitKU(rrm, .3)
    expect_equivalent(rrm, out$known + out$unknown)
    rck <- rowCounts(out$known)
    rc <- rowCounts(rrm)
    expect_true(all(rck > 0 & rck <= ceiling(rc *.3)))
})

## rrm <- as(Matrix(sample(c(rep.int(0, 1000), 0:100), 1000000, T), 1000, sparse = T),
##              "realRatingMatrix")

## microbenchmark(a = out <- .splitKU(rrm, 5), 
##                b = out <- .splitKnownUnknown(rrm, 5))
