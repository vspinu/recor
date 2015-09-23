
## plot.RecoEvalResult <- function(x, y = NULL, avg = TRUE, add=FALSE, type= "b", annotate = FALSE, ...) {
##           function(x, y,
##                    xlim=NULL, ylim=NULL, col = NULL, pch = NULL, lty = 1, 
##                    avg = TRUE, type="b",
##                    annotate= 0, legend="bottomright", ...) {

##               ## a <- avg(x)
##               ## if(!("TPR" %in% colnames(a[[1]])))
##               ##     return(barplot(do.call(rbind, a), beside=TRUE, legend.text=names(a)))

##               what <- match.arg(y, c("ROC", "prec/rec"))
              
##               take <- if(plot_type == "ROC") c("FPR", "TPR")
##                       else c("recall", "precision")

##               ## find best xlim, ylim
##               max_lim <- apply(sapply(x, FUN = 
##                                              function(y) apply(avg(y)[,take], MARGIN=2, max)), MARGIN=1, max)

##               if(is.null(xlim)) xlim <- c(0, max_lim[1])
##               if(is.null(ylim)) ylim <- c(0, max_lim[2])
              
##               ## fix pch, lty and col
##               if(length(pch)==1) pch <- rep(pch, length(x))
##               if(length(lty)==1) lty <- rep(lty, length(x))
              
##               if(is.null(col)) col <- 1:length(x)
##               if(length(col)==1) col <- rep(col, length(x))
              

##               graphics::plot(NA, xlab=take[1], ylab=take[2], ylim=ylim, xlim=xlim)
##               legend(x=legend, legend=names(x), col=col, 
##                      pch = pch, lty=lty, bty="n")
##               for(i in 1:length(x)) plot(x[[i]], y=plot_type, 
##                                          add=TRUE, col=col[i], type=type, annotate = i %in% annotate, 
##                                          pch = pch[i], lty=lty[i], avg = avg, ...)
##           })

