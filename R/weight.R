
get_weight <- function(x, weight, by_rows){
    if(is.null(weight)) rep.int(1L, if(by_rows) nrow(x) else ncol(x))
    else if(is.numeric(weight)) weight
    else  match.fun(weight)(x, by_rows)
}


##' @export
weightIDF <- function(x, by_rows = T){
    if(by_rows) {
        n <- ncol(x)
        s <- rowSums(abs(x))
    } else {
        n <- nrow(x)
        s <- colSums(abs(x))
    }
    log(n/(1 + s))
}
