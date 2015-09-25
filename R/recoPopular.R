
##' @export
setGeneric("recoPopular",
           function(data, keep_data = FALSE, ...)
    standardGeneric("recoPopular"),
    signature = "data",
    valueClass = "Reco")

setMethod("recoPopular", "nsparseMatrix", 
          function(data, keep_data = FALSE, ...){
              pcounts <- colCounts(data)
              pmeans <- colCounts(data)/nrow(data)
              .Reco(doc = "Most Popular Items", predict = "predPopular",
                    data_proc = list(pcounts = pcounts,
                                     pmeans = pmeans))
          })

setMethod("recoPopular", "dsparseMatrix", 
          function(data, keep_data = FALSE, ...){
              pmeans <- colMeans(data)
              pcounts <- colCounts(data)
              .Reco(doc = "Most Popular Items", predict = "predPopular",
                    data_proc = list(pmeans = pmeans,
                                     pcounts = pcounts))
          })

predPopular <- function(object, N = 30, newdata, data = NULL,
                         type = c("ratings", "counts"), ...) {
    data <- pred_get_data(object, data)
    newdata <- pred_get_newdata(object, newdata, data)
    meas <-
        switch(match.arg(type),
               ratings = object@data_proc$pmeans,
               counts = object@data_proc$pcounts)
    to_sparse <- (N < ncol(data))
    if(to_sparse){
        which <- tail(.qorder(meas), N)
        meas[-which] <- 0
    }
    out <- Matrix(meas, nrow = nrow(newdata), ncol = ncol(newdata),
                  byrow = T, dimnames = dimnames(newdata), sparse = to_sparse)
    pred_postproc(out, data, newdata)
}
