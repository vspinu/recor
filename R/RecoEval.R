
##' @export
RecoEval <- function(partition, reco_list, ns = c(1, 3, 10, 20), good_rating = NULL,
                     binarize = TRUE, progress = TRUE, ...){

    names(reco_list) <- .get_reco_names(reco_list)

    results <-
        lapply(names(reco_list), function(nm){
            cbind(reco = nm,
                  eval_one(partition, reco_list[[nm]], ns = ns, good_rating = good_rating,
                           binarize = binarize, progress = progress, ...))
        })
    
    ## errs <- sapply(results, is, "try-error")
    ## if(any(errs)) {
    ##     warning(paste("\n  Recommender '", names(results)[errs], 
    ##                   "' has failed and has been removed from the results.\n", sep=''),
    ##             call. = FALSE)
    ##     results[errs] <- NULL
    ## }
    out <- do.call(rbind, results)
    class(out) <- c("RecoEvalResult", class(out))
    out
}

eval_one <- function(partition, params, ns, good_rating, binarize, progress,  ...) {

    P <- .separate_name(params)
    
    if(progress) cat(P$reco_name, "run:\n")

    out <-
        lapply(1:partition@k, function(nth){
            if(progress) cat(" ", nth, "\t")
            cbind(fold = nth,
                  run_nth(partition, nth, ns, P, good_rating, binarize, progress, ...))
        })

    if(progress) cat("\n")

    do.call(rbind, out)
}

## note! we are intentionally ignoring new newdata here
run_nth <- function(partition, nth, ns, P, good_rating, binarize, progress, newdata, ...){

    ## prepare data
    train <- .get_fold(partition, nth, type="train")
    ktest <- .get_fold(partition, nth, type="known")
    utest <- .get_fold(partition, nth, type="unknown")
    
    ## train recommender
    time_train <-
        system.time({
            params <- c(list(as.name(P$full_name), data = train), P$params)
            params[["keep_data"]] <- NULL
            ## No do.call here. It is called as anonymous function which we
            ## cannot detect in .Reco.
            reco <- eval(as.call(params))
        })
    
    time_pred <- system.time({
        pre <- predict(reco, newdata = ktest, data = train, ...)
        pre <- removeKnownRatings(pre, ktest)
    })

    ogiven <- mean(partition@given)
    agiven <- mean(rowCounts(ktest))
    
    out <-
        lapply(ns, function(n){
            top <- keepK(pre, n, byrows = T)
            if(binarize){
                top <- as(top, "binaryRatingMatrix")
            }
            ac <- accuracy(top, utest, byrows = TRUE, threshold = good_rating)
            ac <- colMeans(ac, na.rm = T)
            cbind(n, ogiven, agiven, as.data.frame(t(ac)))
        })
        
    if(progress)
        cat(sprintf("[%f/%f]\n", sum(time_train[1:2]), sum(time_pred[1:2])))
    
    do.call(rbind, out)
}

.get_reco_names <- function(reco_list){
    nms <- allNames(reco_list)
    if(any(empty <- !nzchar(nms)))
        nms[empty] <- sapply(reco_list[empty], function(params){
            P <- .separate_name(params)
            params <- params[sapply(params, function(x) is.atomic(x) && length(x) == 1)]
            paste(P$reco_name, paste(names(params), ":", params, sep = "", collapse = " "))
        })
    nms
}

.separate_name <- function(params){

    ## get the 'reco'  
    if(is.null(params$name)) {
        nms <- allNames(params)[[1]]
        if(nzchar(nms) || !is.character(params[[1]]))
            stop("each component in reco_list must have an explicit 'name' component or start with an unnamed string.")
        else{
            reco_name <-  params[[1]]
            params <- params[-1]
        }
    } else {
        reco_name <- params$name
        params$name <- NULL
    }

    reco_name <- gsub("^reco", "", reco_name)
    list(reco_name = reco_name,
         full_name = paste0("reco", reco_name),
         params = params)
}

