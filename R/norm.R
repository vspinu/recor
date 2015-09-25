## tothink: soudn't norm and weight return the weighted matrix instead?
## Advantages: it's more general and will remove some repetitive code in proxy4
## Disadvantage: distinction between weight and normalization is lost

## Semantically weight should return the weight. But normalization should return
## the matrix. This is what qlcMatrix does btw.

get_norm <- function(x, norm, by_rows){
    if(is.null(norm)) rep.int(1L, if(by_rows) nrow(x) else ncol(x))
    else if(is.numeric(norm)) norm
    else  match.fun(norm)(x, by_rows)
}

norm1 <- function(x, by_rows = T) {
    if(by_rows) {
        sqrt(rowSums(x^2))
    } else {
        sqrt(colSums(x^2))
    }
}

norm2 <- function(x, by_rows = T) {
    if(by_rows) {
        sqrt(rowSums(x^2))
    } else {
        sqrt(colSums(x^2))
    }
}
