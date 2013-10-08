#' F-R test for one population comparison
#'
#' Compute a minimal spanning tree using package ade4 (optimized MST computation in C). Following
#' Friedman and Rafsky (1979), the number of runs is standardized by substracting the expected number
#' of runs and dividing the difference by the square root of estimated variance. Each FR statistic is dependent 
#' on the topology of the minimal spanning tree of the given population comparision.
#' 
#' @param xx1 matrix of events (rows) by features (columns)
#' @param xx2 matrix of events (rows) by features (columns)
#' 
#' @return ww FR statistic
#' @return runs total number of within-group subtrees (or equivalently, total number of between-group edges plus 1)
#' @return mu estimated mean of runs
#' @return sigma2 estimated variance of runs
#' @return pNorm p-values of the F-R statitic assuming large sample asymptotic normal assumption
#' @examples
#' ## see vignettes
#'
#' @name getFR
#' 
#' @export
getFR <- function(xx1,xx2)
{
  xx <- rbind(xx1,xx2)
  distmat <- dist(xx, method = "euclidean",diag = T, upper=T)

  mstree <- ade4::neig2mat(ade4::mstree(distmat))
  mstree <- as.matrix(mstree)

  n1 <- dim(xx1)[1]
  n2 <- dim(xx2)[1]

  leftbottom <- mstree[(n1+1):(n1+n2),1:n1]
  rightup <- mstree[1:n1,(n1+1):(n1+n2)]
  runs <- sum(rightup)+1

  xsum <- colSums(mstree)

  C <- sum(xsum*(xsum-1))/2
  m <- n1
  n <- n2
  N <- n2+n1

  mu <- 2*m*n/N + 1
  sigma2 <- (2*m*n/(N*(N-1)))*((2*m*n-N)/N+(C-N+2)*(N*(N-1)-4*m*n+2)/((N-2)*(N-3)))
  ww <- (runs-mu)/sqrt(sigma2)
  pNorm <- pnorm(ww)

  list(ww=ww,runs=runs,mu=mu,sigma2=sigma2,pNorm=pNorm)
}

