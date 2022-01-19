
portF <- function(w,mr,mV) {
  PR=t(w)%*% mr;
  PV=t(w)%*% mV %*% w;
  A=c(PR,PV)
  return(A)
}


#ポートフォリのリターンを計算する関数
Return <- function(weight, return.expected){
  sum(weight * return.expected)
}


#ポートフォリのリスクを計算する関数
Risk <- function(weight, covariance.matrix){
  weight.matrix <- matrix(weight, nrow = length(weight))
  (t(weight.matrix) %*% covariance.matrix %*% weight.matrix)^0.5 %>% 
    as.numeric()
}




