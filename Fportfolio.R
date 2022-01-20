
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


# 等式制約関数
equalityConstraint <- function(weight){
  #eq.value と順番を揃えて返却
  c(sum(weight), Risk(weight, covariance.matrix))
}


# 目的関数
# --- solnp関数は目的関数を最小化する
# --- 最大化するためには-1倍しておく必要がある
objectiveFunction <- function(x){
  -Return(x, return.expected)
}



