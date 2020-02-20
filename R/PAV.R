#' Estimation of the Population Average Value in Randomized Experiments
#'
#' This function estimates the Population Average Value. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param Tr A vector of the unit-level binary treatment receipt variable for each sample.
#' @param That A vector of the unit-level binary treatment that would have been assigned by the
#' individualized treatment rule. If \code{plim} is specified, please ensure
#' that the percentage of treatment units of That is lower than the budget constraint.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{pav}{The estimated
#' Population Average Value.} \item{sd}{The estimated standard deviation
#' of PAV.}
#' @examples
#' Tr = c(1,0,1,0,1,0,1,0)
#' That = c(0,1,1,0,0,1,1,0)
#' Y = c(4,5,0,2,4,1,-4,3)
#' pavlist <- PAV(Tr,That,Y)
#' pavlist$pav
#' pavlist$sd
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mlli@mit.edu}, \url{http://mlli.mit.edu};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAV
PAV <- function (Tr, That, Y, centered = TRUE) {
  if (!(identical(as.numeric(Tr),as.numeric(as.logical(Tr))))) {
    stop("Treatment should be binary.")
  }
  if (!(identical(as.numeric(That),as.numeric(as.logical(That))))) {
    stop("That should be binary.")
  }
  if ((length(Tr)!=length(That)) | (length(That)!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (length(Tr)==0) {
    stop("The data should have positive length.")
  }
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }
  Tr=as.numeric(Tr)
  That=as.numeric(That)
  Y=as.numeric(Y)
  if (centered) {
    Y = Y - mean(Y)
  }
  n=length(Y)
  n1=sum(Tr)
  n0=n-n1
  SAV=1/n1*sum(Tr*That*Y)+1/n0*sum(Y*(1-Tr)*(1-That))
  Sf1=var((That*Y)[Tr==1])
  Sf0=var(((1-That)*Y)[Tr==0])
  varexp=Sf1/n1+Sf0/n0
  return(list(pav=SAV,sd=sqrt(max(varexp,0))))
}
