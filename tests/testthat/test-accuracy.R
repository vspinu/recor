
## fixme: need more tests

test_that("accurace works with binary matrices", {

    mat <- .mat()
    sm <- .sm()
    rrm <- .rrm()
    brm <- .brm()

    ac <- accuracy(brm, brm, byrows = F)
    expect_true(all(ac["precision", ] == 1, na.rm=TRUE))
    expect_true(all(ac["recall", ] == 1, na.rm=TRUE))

    brm2 <- brm
    brm2[brm2 > 0] <- F
    ac <- accuracy(brm2, brm, byrows = F)
    expect_true(all(ac["TP", ] == 0))
    expect_true(all(ac["TP", ] == 0))
    expect_true(all(ac["recall", ] == 0))
    expect_true(all(is.na(ac["precision", ])))
}) 
