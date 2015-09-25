
##' @export
setGeneric("recoUBCF",
           function(data, similarity = "cosine", nn = Inf, weighted = TRUE,
                    sample = NULL, keep_data = FALSE, sim_transformer = NULL, 
                    ...)
    standardGeneric("recoUBCF"),
    signature = "data",
    valueClass = "Reco")

setMethod("recoUBCF", "CsparseMatrix", 
          function(data, similarity = "cosine", nn = 30, weighted = TRUE,
                   sample = NULL, keep_data = FALSE,
                   sim_transformer = NULL, ...){

              ## fixme: if sampled, we need to store the data
              if(!is.null(sample))
                  data <- sample(data, as.integer(sample))
              
              .Reco(doc = "User Based Collaborative Filtering (Matrix implementation)",
                    predict = "predUBCF")
          })

predUBCF <- function(object, newdata,
                     nn = Inf, data = NULL,
                     type = c("ratings", "counts"), ...) {

    newdata <- pred_get_newdata(object, newdata, data)
    data <- pred_get_data(object, data)
    
    sim <- similarity(newdata, data, method = object@params$similarity, byrows = T, ...)

    if(!is.null(sim_trans <- object@params$sim_transformer))
        sim <- sim_trans(sim)
    
    if(nn < nrow(data))
        sim <- keepK(sim, nn, byrows = T)
    
    switch(match.arg(type),
           ratings = {
               out <- sim %*% data
           },
           counts = {
               ## how many times similar users visited the item
               sim <- binarize(sim)
               data <- binarize(data)
               out <- tcrossprod(data, sim)   
           })
    
    pred_postproc(out, data, newdata)
}
