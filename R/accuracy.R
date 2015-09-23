
.accuracy_binary <-
    function(predicted, observed, byrows = FALSE, ...) {

        if(!byrows) 
            return(t(Recall(t(predicted), t(observed), byrows = T, ...)))
        
        TP <- rowSums(predicted * observed)
        TP_FN <- rowCounts(observed)
        TP_FP <- rowCounts(predicted)
        FP <- TP_FP - TP
        FN <- TP_FN - TP
        TN <-  ncol(observed) - TP - FP - FN

        ## calculate some important measures
        precision <- TP / TP_FP 
        TPR <- recall <- TP / TP_FN 
        FPR <- FP / (TN + FP) 
        
        cbind(precision, recall, TP, FP, FN, TN, TPR, FPR)
    }

.accuracy_real <- function(predicted, observed, byrows = FALSE, ...) {
    
    if(!byrows)
        return(t(Recall(predicted, observed, byrows = T, ...)))

    abs <- abs(predicted - observed)
    MAE <- rowMeans(abs, na.rm=TRUE)
    MSE <- rowMeans(abs^2, na.rm=TRUE)
    RMSE <- sqrt(MSE)

    cbind(RMSE, MSE, MAE)
}

.accuracy_real_binary <- function(predicted, observed, byrows = FALSE, threshold = NULL, ...){
    if(is.null(threshold))
        stop("when 'observed' is binary and 'predicted' is not, you need to suply 'threshold'")
    .accuracy_binary(predicted >= threshold, observed, byrows)
}

.accuracy_binary_real <- function(predicted, observed, byrows = FALSE, threshold = NULL, ...){
    if(is.null(threshold))
        stop("when 'predicted' is binary and 'observed' is not, you need to suply 'threshold'")
    .accuracy_binary(predicted, observed >= threshold, byrows)
}


setClassUnion("bsparseMatrix", c("nsparseMatrix", "lsparseMatrix"))

##' @export
setGeneric("accuracy",
           function(predicted, observed, byrows = FALSE, ...)
               standardGeneric("accuracy"))

setMethod("accuracy",
          signature(predicted = "bsparseMatrix", observed = "bsparseMatrix"),
          .accuracy_binary)

setMethod("accuracy",
          signature(predicted = "dsparseMatrix", observed = "dsparseMatrix"), 
          .accuracy_real)

setMethod("accuracy",
          signature(predicted = "dsparseMatrix", observed = "bsparseMatrix"), 
          .accuracy_real_binary)

setMethod("accuracy",
          signature(predicted = "bsparseMatrix", observed = "dsparseMatrix"), 
          .accuracy_binary_real)

setMethod("accuracy",
          signature(predicted = "matrix", observed = "matrix"), 
          function(predicted, observed, byrows = FALSE, threshold, ...){
              if(storage.mode(predicted) == "logical"){
                  if(storage.mode(observed) == "logical") .accuracy_binary(predicted, observed, byrows)
                  else .accuracy_real_binary(predicted, observed, byrows)
              } else {
                  if(storage.mode(observed) == "logical") .accuracy_binary_real(predicted, observed, byrows)
                  else .accuracy_real(predicted, observed, byrows)
              }
          })
