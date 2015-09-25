
##' @export
setGeneric("recoIBCF",
           function(data, similarity = "cosine", nn = Inf, 
                    sample = NULL, keep_data = FALSE,
                    sim_transformer = NULL, ...)
    standardGeneric("recoIBCF"),
    signature = "data",
    valueClass = "Reco")

setMethod("recoIBCF", "CsparseMatrix", 
          function(data, similarity = "cosine", nn = Inf,
                   sample = NULL, keep_data = FALSE,
                   sim_transformer = NULL, ...){
              ## fixme: add sampling

              sim <- similarity(data, method = similarity, byrows = FALSE, ...)
              if(!is.null(sim_transformer))
                  sim <- sim_transformer(sim)
              if(nn < Inf && nn < ncol(data))
                  sim <- keepK(sim, nn, byrows = FALSE)
              
              .Reco(doc = "User Based Collaborative Filtering (Matrix implementation)",
                    predict = "predIBCF",
                    data_proc = list(sim = sim))
          })

predIBCF <- function(object, newdata, nn = Inf, data = NULL, type = c("ratings"), ...) {
    newdata <- pred_get_newdata(object, newdata, data)
    data <- pred_get_data(object, data)
    sim <- pred_get_sim(object, nn, F, "sim")

    out <- 
        switch(match.arg(type),
               ratings = {
                   newdata %*% sim
               })
    
    pred_postproc(out, data, newdata)
}
