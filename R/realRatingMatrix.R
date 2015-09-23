##' @include ratingMatrix.R

## setClass("realRatingMatrix", contains = c("ratingMatrix", "dgCMatrix"))
## do it directly,  but https://r-forge.r-project.org/tracker/index.php?func=detail&aid=6185&group_id=61&atid=294

##' @export
setClass("realRatingMatrix",
         representation(normalize = "listOrNull"), 
         contains = c("dgCMatrix"))

## coercion
setAs("matrix", "realRatingMatrix",
      function(from) new("realRatingMatrix", dropNA(from)))

setAs("dgTMatrix", "realRatingMatrix",
      function(from) as(as(from, "dgCMatrix"), "realRatingMatrix"))

setAs("denseMatrix", "realRatingMatrix",
      function(from) as(as(from, "dgCMatrix"), "realRatingMatrix"))

setMethod("t", signature(x = "realRatingMatrix"),
          function(x) new("realRatingMatrix", .Call(Matrix:::Csparse_transpose, x, FALSE)))

## remove this once the multiple inheritance bug is fixed in Matrix
setMethod("[", signature(x = "realRatingMatrix", i = "ANY", j = "ANY", drop = "ANY"),
          function (x, i, j, ..., drop){
              callGeneric(x = as(x, "dgCMatrix"), i = i, j = j, drop = drop)
          })

## fixme: report. R picks wrong method for in sm[ix] - "Matrix#index#missing#missing"
setMethod("[", signature(x = "realRatingMatrix", i = "integer", j = "missing", drop = "missing"),
          function (x, i, j, ..., drop){
              class <- class(x)
              x <- as(x, "dgCMatrix")
              callGeneric(x = x, i = i, ...)
          })

## fixme: report. R picks wrong method for in sm[ix] - "Matrix#index#missing#missing"
setMethod("[", signature(x = "realRatingMatrix", i = "matrix", j = "missing", drop = "missing"),
          function (x, i, j, ..., drop){
              class <- class(x)
              x <- as(x, "dgCMatrix")
              callGeneric(x = x, i = i, ...)
          })
