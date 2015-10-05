
### GENERAL API

get_proxy_function <- function(method, prefix = "sim"){
    if(!is.function(method))
        method <- gsub(paste0("^", prefix), "", tolower(method))
    substr(method, 1, 1) <- toupper(substr(method, 1, 1))
    method <- paste0(prefix, method)
    method <-
        if(exists(method, mode = "function"))
    match.fun(method)
        else
            method <- match.fun(paste0(prefix, "_fallback"))
    method
}

sim_fallback <- function(x, y, method, byrows, ...){
    ## fallback on proxy
    proxy::dist(x = x, y = y, method = method, byrows = byrows)
}

disim_fallback <- function(x, y, method, byrows, ...){
    ## fallback on proxy
    proxy::dist(x = x, y = y, method = method, byrows = byrows)
}

dist_fallback <- function(x, y, method, byrows, ...){
    ## fallback on proxy
    proxy::dist(x = x, y = y, method = method, byrows = byrows)
}


##' @export
similarity <- function(x, y = NULL, method = "cosine", byrows = F, ...){
    fun <- get_proxy_function(method, "sim")
    fun(x = x, y = y, byrows = byrows, ...)
}

##' @export
disimilarity <- function(x, y = NULL, method = "cosine", byrows = F, ...){
    fun <- get_proxy_function(method, "disim")
    fun(x = x, y = y, byrows = byrows, ...)
}

##' @export
distance <- function(x, y = NULL, method = "cosine", byrows = F, ...){
    fun <- get_proxy_function(method, "dist")
    fun(x = x, y = y, byrows = byrows, ...)
}



### COSINE
## tothink: similarity of 0 should be 1 or 0?  dist produces 1
.simCosine <-
    function(x, y = NULL, byrows = FALSE, weight = NULL, norm = "norm2", ...){
        if(is.null(x)) {
            if(is.null(y)) return(NULL)
            else return(.simCosine(y, x, byrows, weight, norm2))
        }
        
        if(byrows)
            return(.simCosine(t(x), if(!is.null(y)) t(y), FALSE, weight, norm))

        if (is.null(weight)) {
            if(is.null(norm))
                if(is.null(y)) return (crossprod(x))
                else return(crossprod(y))
        } else {
            w <- Diagonal(x = get_weight(x, weight, !byrows))
            x <- w %*% x
            if (!is.null(y)) {
                w <- Diagonal(x = get_weight(y, weight, !byrows))
                y <- w %*% y
            }
        }
        
        n <- Diagonal(x = 1/get_norm(x, norm, byrows))
        x <- x %*% n
        
        if (!is.null(y)) {
            n <- Diagonal(x = 1/get_norm(y, norm, byrows))
            crossprod(x, y %*% n)
        } else {
            crossprod(x)
        }
    }

##' @export
setGeneric("simCosine",
           function(x, y = NULL, byrows = FALSE, weight = NULL,  norm = "norm2", ...){
               standardGeneric("simCosine")
           },
           signature = c("x", "y"))

setMethod("simCosine", c("Matrix", "ANY"), .simCosine)
setMethod("simCosine", c("Matrix", "Matrix"), .simCosine)
setMethod("simCosine", c("ANY", "Matrix"), .simCosine)

setMethod("simCosine", c("ANY", "ANY"),
          function(x, y = NULL, byrows = FALSE, weight = NULL,  norm = "norm2", ...){
              if(weight == NULL && norm == "norm2")
                  ## fixme: specifically check for data.frames, vectors and matrices 
                  ## fixme: this should return a Matrix object
                  as.matrix(proxy::simil(x, y, method = "cosine", diag = 1))
              else (.simCosine(x, y, byrows, weight, norm))
          })


### CONDITIONAL
## conditional similarity (Karypis 2001)
.conditional <- function(x, dist=TRUE, args=NULL){
    n <- ncol(x)

    ## sim(v,u) = freq(uv) / freq(v)
    uv <-  crossprod(x)
    v <- matrix(colSums(x), nrow = n, ncol = n, byrows = FALSE)

    sim <- uv/v

    ## fix if freq was 0
    sim[is.na(sim)] <- 0

    if(dist) sim <- as.dist(1/(1+sim))
    else attr(sim, "type") <- "simil"
    attr(sim, "method") <- "conditional"		
    sim
}



### KARYPIS
## Karypis similarity
.karypis <- function(x, dist, args=NULL) {
    
    ## get alpha
    args <- .get_parameters(list(alpha = .5), args)
    
    n <- ncol(x)

    ## normalize rows to unit length
    x <- x/rowSums(x)

    ## for users without items
    x[is.na(x)] <- 0

    ## sim(v,u) = 
    ##      sum_{for all i: r_i,v >0} r_i,u / freq(v) / freq(u)^alpha
    uv <-  crossprod(x, x>0)
    v <- matrix(colSums(x), nrow = n, ncol = n, byrows = FALSE)
    u <- t(v) 

    sim <- uv/v/u^args$alpha 

    ##  fix if freq = 0
    sim[is.na(sim)] <- 0
    
    if(dist) sim <- as.dist(1/(1+sim))
    else attr(sim, "type") <- "simil"
    attr(sim, "method") <- "karypis"		
    sim

}

## ## handle karypis and conditional dissimilarities
## if(method == "karypis") {
##     if(!is.null(y)) stop("Kaypis dissimilarities are not implemented between users or as a cross-dissimilarity!")

##     return(.karypis(as(x, "dgCMatrix"), dist=TRUE, args))
## }

## if(method == "conditional") {
##     if(!is.null(y) || which != "items") stop("Conditional dissimilarities are not implemented between users or as a cross-dissimilarity!")

##     return(.conditional(as(x, "dgCMatrix"), dist=TRUE, args))
## }

## ## FIXME: add Weiss dissimilarity
