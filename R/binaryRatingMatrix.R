##' @include ratingMatrix.R

##' @export
setClass("binaryRatingMatrix", contains = c("ngCMatrix", "ratingMatrix"))

setAs("matrix", "binaryRatingMatrix",
      function(from) new("binaryRatingMatrix", as(from, "ngCMatrix")))

setAs("CsparseMatrix", "binaryRatingMatrix",
      function(from) new("binaryRatingMatrix", i = from@i, p = from@p,
                         Dim = from@Dim, Dimnames = from@Dimnames))

setAs("ldenseMatrix", "binaryRatingMatrix",
      function(from) as(as(from, "lgCMatrix"), "binaryRatingMatrix"))

## missing method: reported as #6186
setAs("ngCMatrix", "ngTMatrix",
      function(from) new("ngTMatrix",
                         i = from@i,
                         j = .Call(Matrix:::Matrix_expand_pointers, from@p),
                         Dim = from@Dim, Dimnames = from@Dimnames))

## setAs("itemMatrix", "binaryRatingMatrix",
## 	function(from) new("binaryRatingMatrix", from@data))

## ## itemMatrix stores data transposed!
## setAs("binaryRatingMatrix", "itemMatrix",
##       function(from) new("itemMatrix", data = t(as(from, "ngCMatrix"))))

setAs("binaryRatingMatrix", "dgTMatrix",
      function(from) as(as(from, "dgCMatrix"), "dgTMatrix"))

setMethod("t", signature(x = "binaryRatingMatrix"),
          function(x) new("binaryRatingMatrix", .Call(Matrix:::Csparse_transpose, x, FALSE)))

## remove this once the multiple inheritance bug is fixed in Matrix
setMethod("[", signature(x = "binaryRatingMatrix", i = "ANY", j = "ANY", drop = "ANY"),
          function (x, i, j, ..., drop){
            new(class(x), callNextMethod())
          })

