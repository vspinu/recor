##' @export
setClass("RecoPartition",
         representation(
             method       = "character",
             given        = "numeric",
             k            = "integer",
             train_prop   = "numeric",	
             folds        = "list",
             data         = "ANY", 
             data_known   = "ANY",
             data_unknown = "ANY",
             good_rating  = "numeric"
         ))

##' @export
setGeneric("RecoPartition",
           def = function(data, method = c("split", "CV", "bootstrap"), 
                          train_prop = 0.9, k = if(method == "split") 1 else 10, 
                          given = .3, good_rating = NA)
               standardGeneric("RecoPartition"),
           signature = "data", 
           valueClass = "RecoPartition")

setMethod("RecoPartition", "CsparseMatrix", 
          function(data, method = c("split", "CV", "bootstrap"), 
                   train_prop = 0.9, k = if(method == "split") 1 else 10, 
                   given = .3, good_rating = NA){
              nrows <- nrow(data)

              if(any(given < 0))
                  stop("All values in `given` must to be >0")

              method <- match.arg(method, c("split", "CV", "bootstrap", "cross-validation"))
              switch(method,
                     `split` = {
                         if(train_prop < 0 || train_prop > 1) stop("'train_prop' must be in [0, 1]")
                         if(k != 1) stop("'k' must be 1 if method is 'split'")
                         take <- round(nrows*train_prop)
                         folds <- replicate(k, sample(1:nrows, take), simplify = FALSE)
                         names(folds) <- "1"
                     },
                     `cross-validation` = , 
                     `CV` = {
                         train_prop <- NA_real_
                         fold_ids <- sample(1:k, nrows, TRUE)
                         folds <- split(1:nrows, fold_ids, drop = T)
                     },
                     `bootstrap` = {
                         take <- round(nrows*train_prop)
                         folds <- replicate(k, sample(1:nrows, take, replace = TRUE), simplify = FALSE)
                     })
              
              ku <- .splitKU(data, given)
              
              new("RecoPartition", 
                  method       = method, 
                  given        = given, 
                  k            = as.integer(k),
                  train_prop   = train_prop, 
                  folds        = folds,
                  data         = data, 
                  data_known   = ku$known, 
                  data_unknown = ku$unknown,
                  good_rating  = as.numeric(good_rating))
          })


## taken with modifications from recommenderlab
setMethod("show", signature(object = "RecoPartition"),
          function(object) {
              if(length(object@given)==1) {
                  cat(sprintf("RecoPartition with %d proportions/items given", 
                              object@given))
              }else{
                  writeLines(c(
                      "RecoPartition with multiple proportions/items given",
                      "Summary:"
                  ))
                  print(summary(object@given))
              }
              
              cat(sprintf("Method: %s with %d fold(s).",  sQuote(object@method), object@k))
              
              if(!is.na(object@train_prop)) {
                  cat(sprintf("Training set proportion: %1.3f", object@train_prop))
              }
              
              if(!is.na(object@good_rating))
                  cat(sprintf("Good ratings: >=%f", object@good_rating))
              else
                  cat(sprintf("Good ratings: NA"))

              cat("\n")
              
              invisible(NULL)
          })


## temporary
##' @export
.splitKU <- function(mat, given){
    if(!is(mat, "CsparseMatrix"))
        stop("Splitting is impleented for 'CsparseMatrix' only. Let us know if need this for other types",
             class(mat))

    nrows <- nrow(mat)
    given <- rep_len(given, nrows)
    if(any(rowCounts(mat) < given))
        stop("Some rows have less than `given` observations")

    tmat <- t(mat)
    
    take <- lapply(1:nrows, function(i){
        if(tmat@p[[i]] == tmat@p[[i + 1L]]){
            return(integer(0))
        } else {
            range <- (tmat@p[[i]] + 1L):tmat@p[[i + 1L]]
            n <-
                if(given[[i]] < 1) ceiling(length(range) * given[[i]])
                else given[[i]]
            .qsort(.qsample(range, n))
        }})

    given <- unlist(lapply(take, length), FALSE, FALSE)
    taken <- unlist(take, FALSE, FALSE)
    
    pknown <- c(0L, cumsum(given))
    punknown <- tmat@p - pknown
    
    k <- u <- tmat
    k@i <- tmat@i[taken]
    k@p <- pknown
    u@i <- tmat@i[-taken]
    u@p <- punknown

    if(.hasSlot(tmat, "x")){
        k@x <- tmat@x[taken]
        u@x <- tmat@x[-taken]
    }
    
    list(known = t(k), unknown = t(u))
}

.get_fold <- function(x, nth, type = c("train", "known", "unknown")) {
    switch(match.arg(type),
           train = x@data[x@folds[[nth]], ],
           known = x@data_known[-x@folds[[nth]], ],
           unknown = x@data_unknown[-x@folds[[nth]], ])
}
