
get_norm <- function(x, norm, by_rows){
    if(is.null(norm)) rep.int(1L, if(by_rows) nrow(x) else ncol(x))
    else if(is.numeric(norm)) norm
    else  match.fun(norm)(x, by_rows)
}

norm2 <- function(x, by_rows = T) {
    if(by_rows) {
        sqrt(rowSums(x^2))
    } else {
        sqrt(colSums(x^2))
    }
}
