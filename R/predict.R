### various utilities for prediciton

##' @export
pred_get_data <- function(model, data = NULL,  warn = FALSE){
    if(!is.null(data))
        data
    else if(!is.null(model@data))
        model@data
    else {
        if(warn)
            warning("Data was not saved with the model. Retriving from the parent environment.")
        call <- model@call

        data <- call[["data"]]

        ## fixme: this is the real data for standardgeneric calls. What is it
        ## for function calls?
        if(!is.symbol(data))
            return(data)
        
        data_name <- as.character(data)
        if(exists(data_name, parent.frame()))
            return(get(data_name, parent.frame()))
        else
            stop("Data wasn't stored in the model and I cannot find object ", sQuote(data_name), " in GlobalEnv.")
    }
}

##' @export
pred_get_newdata <- function(model, newdata, data){
    if(missing(newdata) || is.null(newdata)){
        if(!is.null(data)) data
        else pred_get_data(model, data)
    } else {
        if(is.vector(newdata) && is.numeric(newdata)) {
            if(is.null(data))
                data <- pred_get_data(model, data)
            data[newdata,, drop = FALSE]
        } else {
            newdata
        }
    }
}

##' @export
## remove known ratings and transport meta information from original data
pred_postproc <- function(ratings, data, newdata){
    ## fixme: we need dense and sparse RatingMatrxes to avoid conversion 
    ratings <- as(ratings, "realRatingMatrix")
    if(.hasSlot(data, "normalize") && !is.null(data@normalize))
        ratings@normalize <- data@normalize
    removeKnownRatings(ratings, newdata)
}


##' @export
pred_get_sim <- function(object, nn, byrows = F, sim_name){
    onn <- object@params$nn
    sim <- object@data_proc[[sim_name]]
    if(nn < Inf &&  nn != onn){
        if(nn > onn){
            warn <- sprintf("For '%s''nn' (%d) prediction parameter is higher than original 'nn' (%d) parameter passed to the constructor. Ignored.",
                            sim_name, nn, onn)
            warning(warn)
        } else {
            sim <- keepK(sim, nn, byrows = byrows)
        }
    }
    sim
}

##' @export
## pred_sim <- function(object, newdata,
##                      nn = Inf, data = NULL,
##                      type = c("ratings"),
##                      sim_name, ...) {

##     newdata <- pred_get_newdata(object, newdata, data)
##     data <- pred_get_data(object, data)
##     sim <- pred_get_sim(object, nn, F, sim_name)

##     out <- 
##         switch(match.arg(type),
##                ratings = {
##                    newdata %*% sim
##                })
    
##     pred_postproc(out, data, newdata)
## }
