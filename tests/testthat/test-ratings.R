
test_that("rating removal works", {

    R <- 200
    C <- 100
    mr <- .mat(, R, C)
    smr <- .sm(, R, C)
    brmr <- .brm(, R, C)

    seedk <- 1211
    mk <- .mat(seedk, R, C)
    smk <- .sm(seedk, R, C)
    brmk <- .brm(seedk, R, C)

    val <- mr
    val[mk != 0] <- 0

    expect_equal(removeKnownRatings(mr, mk), val)
    expect_equal(removeKnownRatings(mr, smk), val)
    expect_equal(removeKnownRatings(mr, brmk), val)
    expect_equal(as.matrix(removeKnownRatings(smr, smk)), val)
    expect_equal(as.matrix(removeKnownRatings(smr, brmk)), val)

    lval <- val
    storage.mode(lval) <- "logical"
    expect_equal(as.matrix(removeKnownRatings(brmr, brmk)), lval)
})
