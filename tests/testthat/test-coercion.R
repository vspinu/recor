context("coercion")

setClass("A", contains = c("Z", "ngCMatrix"))
ngc <- as(diag(3), "ngCMatrix")
ngc[2, 3] <- T
a <- as(ngc, "A")
a > 0

rowSums(a)

setClass("Z", representation(new_slot = "list"))

setClass("C", contains = c("Z", "dgCMatrix"))
dgc <- as(diag(3), "dgCMatrix")
dgc[2, 3] <- 23
c <- as(dgc, "C")
c > 0

str(ngc)


(dgc <- as(matrix(c(0, 0, 2, 4, 5,0,  0, 0, 9), 3, byrow = T), "dgCMatrix"))
rrm <- as(dgc, "realRatingMatrix")
rowSums(rrm)
rrm > 0
is(dgc, "CsparseMatrix")
