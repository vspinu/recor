##' @include commonS4.R

##' @export
setClass("Reco",
         representation(
             reco_type = "character",
             data      = "ANY", 
             data_type = "character",
             data_proc = "listOrNull", 
             doc       = "character", 
             call      = "language", 
             params    = "list",
             predict   = "functionOrCharacter"))

##' @export
setMethod("show", signature(object = "Reco"),
          function(object) {
              str <- capture.output(str(object))[-1]
              cat(object@reco_type, "recommender for", sQuote(object@data_type) , " data:\n")
              cat(str, sep = "\n")
          })


##' @export
.Reco <- function(doc = "<none>", predict, params, keep_data, reco_type, data_type, data, data_proc = NULL){

    if(missing(predict))
        stop("'predict' function must be supplied! ")

    ## get names of all calls in the stack
    ## this doesnt work when constructor is called anonymously as in `do.call` 
    cnames <- lapply(sys.calls(), function(c) {
        call <- c[[1]]
        if(is.name(call)) as.character(call)
        else if (is(call, "standardGeneric")) as.character(call@generic)
        else ""

    })
    reco_names <- grepl("^reco", rev(cnames)[-1])
    if(!any(reco_names))
        stop("Recomender constructor must start with \"reco\".")
    nth <- which(reco_names)[[1]]

    ## get the calling function and its args
    sysfun <- sys.function(-1)
    ## if a generic is called from a do.call, we can still detect it 
    cfun <- if(is(sysfun, "MethodDefinition")) sysfun@.Data else sysfun
    cargs <- formals(cfun)
    cargs[c("data", "...")] <- NULL

    call <- match.call(cfun, sys.call(-nth))

    ## Build Missing Stuff ##
    
    cframe <- parent.frame()
    if(missing(data))
        data <- get("data", cframe)
    
    if(missing(params))
        params <- mget(names(cargs), cframe)

    if(missing(keep_data)) {
        keep_data <- params$keep_data
        if(is.null(keep_data))
            keep_data <- FALSE
    }

    if(missing(data_type))
        data_type <- class(data)

    if(missing(reco_type)){
        reco_type <- gsub("^reco", "", reco_names[[nth]])
    }
    
    if(!keep_data)
        data <- NULL

    new("Reco", reco_type = reco_type, data = data, data_type = data_type,
        doc = doc, call = call, params = params, predict = predict,
        data_proc = data_proc)
}

##' @export
setMethod("predict", signature(object = "Reco"),
          function(object, newdata, data = NULL, ...){
              match.fun(object@predict)(object = object,
                  newdata = newdata,
                  data = data, ...)
          })

