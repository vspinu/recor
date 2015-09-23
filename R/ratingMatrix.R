##' @include commonS4.R

##' @export
setClass("ratingMatrix", representation(normalize = "listOrNull"))

setAs("ratingMatrix", "list", function(from) getList(from))

##' @export
setGeneric("nratings",
           function(x, ...) standardGeneric("nratings"),
           useAsDefault = function(x, ...) nnzero(x, na.counted = FALSE))

##' @export
setMethod("sample", signature(x = "ratingMatrix"),
          function(x, size, replace = FALSE, prob = NULL){
              index <- sample(c(1:nrow(x)), size = size,
                              replace = replace, prob = prob)
              x[index,]
          })

##' @export
setGeneric("removeKnownRatings",
           def = function(predicted, known) standardGeneric("removeKnownRatings"),
           useAsDefault =
               function(predicted, known){
                   if(is.null(dim(predicted) || is.null(dim(known))))
                       stop("'predicted' and 'known' must be matrix like objects")
                   if(!identical(dim(predicted), dim(known)))
                       stop("dimensions of 'predicted' and 'know' objects must match" )
                   predicted[as.logical(known != 0)] <- 0
                   predicted
               })

## setMethod("removeKnownRatings", c("ANY", "CsparseMatrix"),
##           function(predicted, known){
##               ## maybe fixme, NAs in known are treated as known
##               stop("not implemented")
##           })

setMethod("removeKnownRatings", c("CsparseMatrix", "CsparseMatrix"),
          function(predicted, known){

              if(!identical(dim(predicted), dim(known)))
                  stop("dimensions of 'predicted' and 'know' objects must match" )

              ## fixme: both of the followint are  super inneficent; report/fix in Matrix
              ## predicted[cbind(known@i, j) + 1L] <- 0
              ## ix <- (known@i + 1L) + nrow(known)*(0:(ncol(known) - 1L))[j + 1L]
              ## predicted[ix] <- 0

              rowcounts <- nrow(known)*(0:(ncol(known) - 1L))

              ## fixme: use non0ind here
              pj <- .Call(Matrix:::Matrix_expand_pointers, predicted@p)
              kj <- .Call(Matrix:::Matrix_expand_pointers, known@p)
              pix <- predicted@i + rowcounts[pj + 1L]
              kix <- known@i + rowcounts[kj + 1L]
              same <- which(pix %in% kix)

              pclass <- class(predicted)

              if(.hasSlot(predicted, "x")){
                  predicted@x[same] <- 0
                  new(pclass, drop0(predicted, is.Csparse = T))
              } else {
                  predicted <- as(predicted, "dgCMatrix")
                  predicted@x[same] <- 0
                  predicted <- drop0(predicted, is.Csparse = T)
                  as(predicted, pclass)
              }
          })

##' @export
setMethod("show", signature(object = "ratingMatrix"),
          function(object) {
              cat(nrow(object), 'x', ncol(object), "rating matrix of class",
                  sQuote(class(object)), "with",
                  nratings(object), "ratings.\n")
              if(!is.null(object@normalize)) {
                  cat("Normalized using",object@normalize$method,"on ") 
                  if(object@normalize$row) cat("rows.")
                  else cat("columns.")
                  cat("\n")
              }
              invisible(NULL)
          })

##' @export
setMethod("image", signature(x = "ratingMatrix"),
          function(x, xlab = "Items (Columns)", ylab = "Users (Rows)",  colorkey=TRUE, ...) {

              ## binaryRatingMatrix does not need a colorkey
              if(is(x, "binaryRatingMatrix")) colorkey <- FALSE
              
              Matrix::image(as(x, "dgTMatrix"), ylab = ylab, xlab = xlab, colorkey = colorkey, ...)
          })

setMethod("[", signature(x = "ratingMatrix", i = "ANY", j = "ANY", drop = "ANY"),
          function (x, i, j, ..., drop){
              out <- new(class(x), callNextMethod())
              out@normalize <- x@normalize
              out
          })
