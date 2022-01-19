Rw=R_weight;
a_r=round(colMeans(Rw),5)%>% print(); ar=as.matrix(a_r) # 期待リターン
return.expected <- ar;
covM=var(Rw)
ind_risk=sqrt(diag(covM));
ind_rtrn=ar;
Ans12=cbind(ind_risk,ind_rtrn)
rownames(Ans12)<- rabel12

write.csv(Ans12,"xy-heimen12.csv")
# リスク・リターン平面の描画
plot(ind_risk,ind_rtrn)
pointLabel(x=ind_risk, y=ind_rtrn, labels=rabel12 )


#===最小分散ポートフォリオ========
#install.packages("Rsolnp")   #初回のみ
library(Rsolnp)

#Rweightから、似た動きの銘柄を省き、銘柄9個にする
# remove no6,no7 and no.9
Rw=R_weight[,c(1,2,3,4,5,8,10,11,12)];
head(round(Rw,3));

#install.packages("Rsolnp")
library(Rsolnp)
library(maptools)

#ラベル名の指定
r=round(colMeans(Rw),5)
V=var(Rw)
mr=as.matrix(r);mV=as.matrix(V);
#行列の代入
risk=as.matrix(sqrt(diag(mV)));

portF <- function(w,mr,mV) {
  PR=t(w)%*% mr;
  PV=t(w)%*% mV %*% w;
  A=c(PR,PV)
  return(A)
}

w=as.matrix(c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9)) #決定変数を初期化
A=portF(w, mr,mV)
NUMBER.OF.ASSETS=9;N=9
objF <- function(w){A=portF(w, mr,mV); return(A[2])} #目的関数(最適化したい変数だけの関数に仕上げる)
ConstF= function(w) return( sum(w) )     # 制約式の右辺
eq.value = 1                             # 制約式の左辺
ineqConst <- function(w){w}              # 不等式制約関数
ineq.lower <- rep(0, NUMBER.OF.ASSETS);  # 不等式制約値  # --- ウエイトの上下限
ineq.upper <- rep(1, NUMBER.OF.ASSETS);  # 不等式制約値  # --- ウエイトの上下限
#optimize
solution <- solnp(pars=w, fun =objF, eqfun = ConstF, eqB = eq.value,                   
                  ineqfun = ineqConst,
                  ineqLB  = ineq.lower, ineqUB  = ineq.upper)
w_opt=solution$pars
round(w_opt,4)
#A=portF(w_opt, mr,mV); A=as.data.frame(A)
#A
#mvpr

#========最大リターンのポートフォリオ＝＝＝＝＝＝＝＝＝＝
library(tidyverse)


NUMBER.OF.ASSETS <- 9; # 資産数
N=9;
weight <- rep(1/N, N) 
w <- rep(1/N, N) 
names(weight) =rabel9


#平均リターン
a_r=round(colMeans(Rw),5)%>% print(); ar=as.matrix(a_r)
ar
# 期待リターン
return.expected <- ar;


# 分散共分散行列
covariance.matrix <-var(Rw) %>%  print()
covM=var(Rw)
ind_risk=sqrt(diag(covM));
ind_rtrn=ar;




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

Risk(weight, covM)

# 目的関数
# --- solnp関数は目的関数を最小化する
# --- 最大化するためには-1倍しておく必要がある
objectiveFunction <- function(x){
  -Return(x, return.expected)
}

# 等式制約関数
equalityConstraint <- function(weight){
  #eq.value と順番を揃えて返却
  c(sum(weight), Risk(weight, covariance.matrix))
}

# 等式制約値( 投資ウェイトの合計(1), リスク値の指定(0.05)
eq.value <- c(1, 1)

# 不等式制約関数
inequalityConstraint <- function(weight){
  weight
}





# 不等式制約値
ineq.lower <- rep(0, N);  ineq.upper <- rep(1, N)  # --- ウエイトの上下限

# 最適化の記述
solution <- solnp(pars    = weight, 
                  fun     = objectiveFunction,
                  eqfun   = equalityConstraint,
                  eqB     = eq.value, 
                  ineqfun = inequalityConstraint, 
                  ineqLB  = ineq.lower, 
                  ineqUB  = ineq.upper)

# 最適ウエイト
solution$pars
# 最適解
# --- 最適化された目的関数の値
solution$values %>% tail(1)

w_max=solution$pars

portF2 <- function(w,mr,mV) {
  PR=t(w)%*% mr;
  PV=t(w)%*% mV %*% w;
  Vol=sqrt(PV)
  A=c(Vol,PR)
  return(A)
}



Amax=portF2(w_max,r,V);
Aopt=portF2(w_opt,r,V) ;
Aall=rbind(Ans12,Amax,Aopt)
# リスク・リターン平面の描画
plot(Aall[,1],Aall[,2])
rabelA=c("X1_WLD","X2_SDGs","X3_JBear", "X4_JBull","X5_UBear","X6_UBull", "X7_FinS","X8_TchS", "X9_tEngS", "X10_lifeTFS", "X11_EnyS","X12_msfWIDX","MaxP","MinP") 
pointLabel(x=Aall[,1], y=Aall[,2], labels=rabelA )
write.csv(Aall,"xy-heimen12plus2.csv")



