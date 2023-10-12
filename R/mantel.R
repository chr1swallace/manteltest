##' content for description
##'
##' @title Mantel test
##' @param m1 matrix 1 
##' @param m2 matrix 2, of same dimension as m1
##' @param nperm number of permutations
##' @param graph if true, draw a histogram of permuted values
##' @param ... arguments passed to hist if graph=TRUE
##' @importFrom Rcpp evalCpp
##' @return a list containing the test stat, permuted stats, and p value
##' @export 
##' @author Chris Wallace
my.mantel.test= function (m1, m2, nperm = 999, graph = FALSE, ...) {
  stopifnot(nrow(m1)==nrow(m2) && nrow(m1)==ncol(m1) && nrow(m1)==ncol(m2))
    n <- nrow(m1)
    diag(m1)=diag(m2)=0
    ## realz <- ape:::mant.zstat(m1, m2)
    realz <- ape_zstat(m1, m2)
    nullstats <- replicate(nperm, ape_zstat(m1, ape_perm(m2)))
    ## fit a kde to nullstats
    d <- density(c(nullstats,realz))
    idx <- which(d$x > realz)
    approx.tail.prob <- sum(d$y[idx] * diff(d$x)[1])
    if(graph) {
      hist(nullstats,xlim=c(min(c(nullstats,realz)),max(c(nullstats,realz))),freq=false,...)
      title(sub=format.pval(approx.tail.prob))
      lines(d)
      abline(v=realz,col="firebrick")
    }
    list(z.stat = realz, p=approx.tail.prob, nobs=nrow(m1), null.stat=nullstats)
}

