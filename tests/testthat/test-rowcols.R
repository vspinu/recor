
test_that("row/col Count/Sums works", {
    mat <- matrix(sample(c(rep.int(0, 1000), 0:100), 20000, T), 200)
    smat <- Matrix(mat, sparse = T)
    rrm <- as(smat, "realRatingMatrix")

    expect_equal(rowCounts(smat), rowSums(mat != 0))
    expect_equal(rowCounts(rrm), rowSums(mat != 0))
    expect_equal(colCounts(smat), colSums(mat != 0))
    expect_equal(colCounts(rrm), colSums(mat != 0))

    ngc <- as(mat > 0, "ngCMatrix")
    brm <- as(mat > 0, "binaryRatingMatrix")

    expect_equal(rowCounts(ngc), rowSums(mat != 0))
    expect_equal(rowCounts(brm), rowSums(mat != 0))
    expect_equal(colCounts(ngc), colSums(mat != 0))
    expect_equal(colCounts(brm), colSums(mat != 0))
})

test_that("keepK works", {

    .knn <- function(sim, k)
        apply(sim, MARGIN=1, FUN=function(x)
            head(order(x, decreasing=TRUE, na.last=TRUE), k))
        
    m <- .mat()
    sp <- .sp()
    
    expect_equal(keepK(m, 2), as.matrix(keepK(sp, 2)))
    expect_equal(keepK(m, 2, byrow = T), as.matrix(keepK(sp, 2, byrow = T)))

    tkeep <- sample(1:20, ncol(sp), T)
    expect_equal(keepK(m, tkeep), as.matrix(keepK(sp, tkeep)))
    tkeep <- sample(1:20, nrow(sp), T)
    expect_equal(keepK(m, tkeep, byrow = T), as.matrix(keepK(sp, tkeep, byrow = T)))

    sp3 <- keepK(sp, 2, replace = 3)
    m3 <- keepK(m, 2, replace = 3)
    m3[which(m == 0)] <- 0
    expect_equal(m3, as.matrix(sp3))

    
    m <- .mat(, 1000, 1000)
    sp <- .sp(, 1000, 1000)

    microbenchmark(keepK(m, 10), keepK(sp, 10))
    microbenchmark(keepK(m, 2), keepK(sp, 2))

})

test_that("sampleMissing works", {
    sm <- .sm()
    
    sms <- sampleMissing(sm, ratio = 2, byrows = F, unavailable = 0)
    expect_true(all(as.character(unique(sms$cols)) %in% unique(colnames(sm))))
    expect_equal(nrow(sms), 3*nnzero(sm))
    expect_equal(as.vector(tapply(sms$ratings, sms$col_ix, function(x) length(x[x > 0]))), 
                 as.vector(colCounts(sm)))
    expect_true(all(sm[sms$ix] == sms$ratings))
    expect_true(all(sm[cbind(sms$row_ix, sms$col_ix)] == sms$ratings))

    sms <- sampleMissing(sm, ratio = 2, byrows = T, unavailable = 0)
    expect_true(all(as.character(unique(sms$rows)) %in% unique(rownames(sm))))
    expect_equal(nrow(sms), 3 * nnzero(sm))
    rc <- rowCounts(sm)
    rc <- rc[rc > 0]
    ## rc <- rc[sort(names(rc))]
    expect_equal(as.vector(tapply(sms$ratings, sms$row_ix, function(x) length(x[x > 0]))), 
                 as.vector(rc))

    expect_true(all(sm[sms$ix] == sms$ratings))
    expect_true(all(sm[cbind(sms$row_ix, sms$col_ix)] == sms$ratings))

})
