
.qsample <- function(x, n){
    ## R's 'sample' is cretinous
    x[sample.int(length(x), n)]
}

.qsort <- function(x){
    if(is.atomic(x) && is.numeric(x))
        .Internal(qsort(x, FALSE))
    else
        sort(x)
}

.qorder <- function(x){
    order(x)
    ## if(is.atomic(x) && is.numeric(x))
    ##     .Internal(qsort(x, TRUE))$ix
    ## else
    ##     order(x)
}

##' @export
.mat <- function(seed = NULL, R = 100, C = 25){
    if(is.null(seed))
        seed <- getOption("recolab_test_seed", 17)
    set.seed(seed)
    dimnames <- list(1:R, suppressWarnings(make.names(rep_len(letters, C))))
    matrix(sample(c(rep.int(0, 1000), 1:100), R*C, T), R, C, dimnames = dimnames)
}
##' @export
.sm <- .sp <- function(seed = NULL, R = 100, C = 25){
    Matrix(.mat(seed, R, C), sparse = T)
}
##' @export
.rrm <- function(seed = NULL, R = 100, C = 25){
    as(.sm(seed, R, C), "realRatingMatrix")
}
##' @export
.brm <- function(seed = NULL, R = 100, C = 25){
    as(.sm(seed, R, C), "binaryRatingMatrix")
}

##' @export
dropNA <- function(x) {
    
    if(is(x, "matrix")){
        x[is.na(x)] <- 0
        x
    } else {
        if(.hasSlot(x, "x")){
            x@x[is.na(x)] <- 0
            drop(x)
        } else {
            ## nothing to do
            x
        }   
    }
}


##' @export
setGeneric("binarize",
           function(x, minRating = 0, ...) standardGeneric("binarize"),
           signature = "x",
           useAsDefault = function(x, minRating, ...){
               as(drop0(x >= minRating), "binaryRatingMatrix")
           })


## VS[10-09-2015]: FIXME,  this transformations shouldn't be done this way

## ## FIXME: we could do this cheaper
## setAs("data.frame", "binaryRatingMatrix",
##       function(from) {
##           rr <- as(from, "realRatingMatrix")
##           binarize(rr, minRating=-Inf)
##       })

## ## from a data.frame with columns user, item, rating
## ## this perserves 0s
## setAs("data.frame", "realRatingMatrix", function(from) {
##     user	<- from[,1]
##     item	<- from[,2]
##     if(ncol(from)>=3) rating <- as.numeric(from[,3])
## 		else rating <- rep(1, length(item))

##     i <- factor(user)
##     j <- factor(item)

##     dgT <- new("dgTMatrix", i = as.integer(i)-1L, j = as.integer(j)-1L, 
##                x = rating,
##                Dim = c(length(levels(i)), length(levels(j))),
##                Dimnames = list(levels(i),levels(j)))
    
##     new("realRatingMatrix", data = as(dgT, "dgCMatrix"))
## })

## ## this expects all ratingMatrices to be coercable to dgTMatrix 
## setMethod("getData.frame", signature(from = "ratingMatrix"),
##           function(from, decode = TRUE, ratings = TRUE,...) {
##               dgT <- as(from, "dgTMatrix")

##               if(decode) {
##                   df <- data.frame(user=rownames(from)[dgT@i+1L],
##                                    item=colnames(from)[dgT@j+1L],
##                                    rating=dgT@x)
##               }else{
##                   df <- data.frame(user=dgT@i+1L,
##                                    item=dgT@j+1L,
##                                    rating=dgT@x)
##               }

##               if(!ratings) df <- df[,-3]

##               ## sort by users
##               df[order(df[,1]),]
##           })

## setAs("ratingMatrix", "data.frame", function(from) getData.frame(from))
