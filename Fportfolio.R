
portF <- function(w,mr,mV) {
  PR=t(w)%*% mr;
  PV=t(w)%*% mV %*% w;
  A=c(PR,PV)
  return(A)
}





