


### MISC
##' @export
setGeneric("keepK", function(x, keep = 1, byrows = F, highest = T, replace = 0, ...) standardGeneric("keepK"),
           signature = "x")

setMethod("keepK", "matrix", function(x, keep = 1, byrows = F, highest = T, replace = 0, ...){
    keep_fun <- if(highest) utils::tail else utils::head
    if(byrows){
        keep <- rep_len(keep, nrow(x))
        for(i in 1:nrow(x)){
            ixs <- keep_fun(.qorder(x[i, ]), keep[[i]])
            x[i, -ixs] <- replace
        }
    } else {
        keep <- rep_len(keep, ncol(x))
        for(i in 1:ncol(x)){
            ixs <- keep_fun(.qorder(x[, i]), keep[[i]])
            x[-ixs, i] <- replace
        }
    }
    Matrix(x, sparse = T)
})

setMethod("keepK", "CsparseMatrix", function(x, keep = 1, byrows = F, highest = T, replace = 0, ...) {
    if(byrows) {
        t(keepK(t(x), keep = keep, byrows = F, highest = highest, replace = replace, ...))
    } else {
        keep <- rep_len(keep, ncol(x))
        keep_fun <- if(highest) utils::tail else utils::head
        has_x <- .hasSlot(x, "x")
        ixs <- lapply(1:ncol(x), function(i) {
            xi <- x@p[[i]]
            xi1 <- x@p[[i + 1L]]
            if(xi == xi1) {
                integer(0)
            } else {
                range <- (x@p[[i]] + 1L):x@p[[i + 1L]]
                keep_range <-
                    if(has_x)
                        range[keep_fun(.qorder(x@x[range]), keep[[i]])]
                    else
                        range[keep_fun(x@i[range], keep[[i]]) + 1L]
                .qsort(keep_range)
            }
        })

        keepixs <- unlist(ixs)
        newp <- c(0L, cumsum(unlist(lapply(ixs, length))))

        ## out <- x

        if(replace == 0){
            x@i <- x@i[keepixs]
            if(has_x)
                x@x <- x@x[keepixs]
            x@p <- newp
        } else {
            if(!has_x)
                stop("replace argument is not meaningful for Matrixes with no 'x' slot")
            x@x[-keepixs] <- replace
        }
        x
    }
})



## sampleMissing
##' @export
setGeneric("sampleMissing",
           function(x, ratio = 2, byrows = F, unavailable = 0, ...)
               standardGeneric("sampleMissing"),
           signature = "x")

setMethod("sampleMissing", "matrix", function(x, keep = 1, byrows = F, unavailable = 0, ...){
    stop("Not implemented.")
})

setMethod("sampleMissing", "CsparseMatrix", function(x, ratio = 2, byrows = F, unavailable = 0, ...) {

    if(byrows) {
        x <- t(x)
    }

    row_range <- 1:nrow(x)
    xi <- x@i + 1L

    ratings <- lapply(1:ncol(x), function(i) {
        i0 <- x@p[[i]]
        i1 <- x@p[[i + 1L]]
        if(i0 == i1) {
            integer(0)
        } else {
            range <- (i0 + 1L):i1

            xs_avail <- 
                if(.hasSlot(x, "x"))
                    x@x[range] 
                else
                    rep.int(1, length(range))
            rrange_avail <- xi[range]
            range_avail <- 
                if(byrows) i + (rrange_avail - 1L)*ncol(x)
                else rrange_avail + (i - 1L)*nrow(x)
            
            len_unavail <- nrow(x) - length(xs_avail)
            len_unavail <- min(len_unavail, as.integer(ratio * length(xs_avail)))
            xs_unavail <- rep.int(unavailable, len_unavail)
            rrange_unavail <- .qsample((1:nrow(x))[-rrange_avail], len_unavail)
            range_unavail <-
                if(byrows) i + (rrange_unavail - 1L)*ncol(x)
                else rrange_unavail + (i - 1L)*nrow(x)

            ## something is very slow here. Probably rbind :(
            cbind(ratings = c(xs_avail, xs_unavail),
                  row_ix = c(rrange_avail, rrange_unavail), 
                  col_ix = i,
                  ix = c(range_avail, range_unavail))
        }
    })

    out <- do.call(rbind, ratings)
    out <- as.data.frame(out)

    row_names <- rownames(x)
    if(is.null(row_names))
        row_names <- 1:nrow(x)
    col_names <- colnames(x)
    if(is.null(colnames))
        col_names <- 1:ncol(x)

    row_names <- row_names[out$row_ix]
    col_names <- col_names[out$col_ix]
    
    if(byrows){
        names(out)[c(2, 3)] <- c("col_ix", "row_ix")
        out$cols <- row_names
        out$rows <- col_names
    } else {
        out$cols <- col_names
        out$rows <- row_names        
    }
    out
})



### COUNTS
##' @export
setGeneric("colCounts", function(x, ...) standardGeneric("colCounts"))
setMethod("colCounts", signature(x = "CsparseMatrix"), 
          function(x, ...) structure(diff(x@p), names = colnames(x)))

##' @export
setGeneric("rowCounts", function(x, ...) standardGeneric("rowCounts"))
setMethod("rowCounts", signature(x = "CsparseMatrix"), 
          function(x, ...) structure(diff(t(x)@p), names = rownames(x)))



### SDs

## compute standard deviation
.dgC2list <- function(x, row=TRUE) {
    if(row) x <- t(x)   
    lapply(2:length(x@p), FUN = function(i) {
        if(x@p[i-1L]==x@p[i]) numeric(0)
        else x@x[(x@p[i-1L]+1L):x@p[i]]
    })
}

##' @export
setGeneric("rowSds", function(x, ...) standardGeneric("rowSds"))
setMethod("rowSds", signature(x = "CsparseMatrix"),
          function(x, ...) {
              s <- sapply(.dgC2list(x, row=TRUE), sd)
              names(s) <- rownames(x)
              s
          })

##' @export
setGeneric("colSds", function(x, ...) standardGeneric("colSds"))
setMethod("colSds", signature(x = "CsparseMatrix"),
          function(x, ...) {
              s <- sapply(.dgC2list(x@data, row=FALSE), sd)
              names(s) <- colnames(x)
              s
          })
