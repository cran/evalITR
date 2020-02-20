#' Estimation of the Population Average Prescription Difference in Randomized Experiments
#'
#' This function estimates the Population Average Prescription Difference with a budget
#' constraint. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param Tr A vector of the unit-level binary treatment receipt variable for each sample.
#' @param Thatfp A vector of the unit-level binary treatment that would have been assigned by the
#' first individualized treatment rule. Please ensure that the percentage of treatment units of That is lower than the budget constraint.
#' @param Thatgp A vector of the unit-level binary treatment that would have been assigned by the
#' second individualized treatment rule. Please ensure that the percentage of treatment units of That is lower than the budget constraint.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param plim The maximum percentage of population that can be treated under the
#' budget constraint. Should be a decimal between 0 and 1.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{papd}{The estimated
#' Population Average Prescription Difference} \item{sd}{The estimated standard deviation
#' of PAPD.}
#' @examples
#' Tr = c(1,0,1,0,1,0,1,0)
#' That = c(0,1,1,0,0,1,1,0)
#' That2 = c(1,0,0,1,1,0,0,1)
#' Y = c(4,5,0,2,4,1,-4,3)
#' papdlist <- PAPD(Tr,That,That2,Y,plim = 0.5)
#' papdlist$papd
#' papdlist$sd
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mlli@mit.edu}, \url{http://mlli.mit.edu};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAPD
PAPD <- function (Tr, Thatfp,Thatgp , Y, plim, centered = TRUE) {
  if (!(identical(as.numeric(Tr),as.numeric(as.logical(Tr))))) {
    stop("Treatment should be binary.")
  }
  if (!(identical(as.numeric(Thatfp),as.numeric(as.logical(Thatfp))))) {
    stop("Thatfp should be binary.")
  }
  if (!(identical(as.numeric(Thatgp),as.numeric(as.logical(Thatgp))))) {
    stop("Thatgp should be binary.")
  }
  if ((plim<0) | (plim>1)) {
    stop("Budget constraint should be between 0 and 1")
  }
  if ((length(Tr)!=length(Thatfp)) | (length(Thatfp)!=length(Thatgp)) | (length(Thatgp)!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (length(Tr)==0) {
    stop("The data should have positive length.")
  }
  if ((sum(Thatfp)>=floor(length(Tr)*plim)+1) | (sum(Thatgp)>=floor(length(Tr)*plim)+1)) {
    stop("The number of treated units in Thatfp or Thatgp does not match the budget constraint.")
  }
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }
  Tr=as.numeric(Tr)
  Thatfp=as.numeric(Thatfp)
  Thatgp=as.numeric(Thatgp)
  Y=as.numeric(Y)
  if (centered) {
    Y = Y - mean(Y)
  }
  n=length(Y)
  n1=sum(Tr)
  n0=n-n1
  SAPEfp=1/n1*sum(Tr*Thatfp*Y)+1/n0*sum(Y*(1-Tr)*(1-Thatfp))-plim/n1*sum(Y*Tr)-(1-plim)/n0*sum(Y*(1-Tr))
  SAPEgp=1/n1*sum(Tr*Thatgp*Y)+1/n0*sum(Y*(1-Tr)*(1-Thatgp))-plim/n1*sum(Y*Tr)-(1-plim)/n0*sum(Y*(1-Tr))
  Sfp1=var(((Thatfp-plim)*Y)[Tr==1])
  Sfp0=var(((Thatfp-plim)*Y)[Tr==0])
  kf1=mean(Y[Tr==1 & Thatfp==1])-mean(Y[Tr==0 & Thatfp==1])
  kf0=mean(Y[Tr==1 & Thatfp==0])-mean(Y[Tr==0 & Thatfp==0])
  PAPD=SAPEfp-SAPEgp
  Sfgp1=var(((Thatfp-Thatgp)*Y)[Tr==1])
  Sfgp0=var(((Thatfp-Thatgp)*Y)[Tr==0])
  kg1=mean(Y[Tr==1 & Thatgp==1])-mean(Y[Tr==0 & Thatgp==1])
  kg0=mean(Y[Tr==1 & Thatgp==0])-mean(Y[Tr==0 & Thatgp==0])
  varfgp=Sfgp1/n1+Sfgp0/n0-floor(n*plim)*(n-floor(n*plim))/(n^2*(n-1))*(kf1^2+kg1^2)+
    2*floor(n*plim)*max(floor(n*plim),n-floor(n*plim))/(n^2*(n-1))*abs(kf1*kg1)
  return(list(papd=PAPD,sd=max(sqrt(varfgp),0)))
}