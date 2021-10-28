
graphics.off(); rm(list=ls(all=TRUE));  # 

#define Covariance Matrix


#この結果から、この4銘柄問題における最小分散ポートフォリオは以下の通りです。
#株式、社債、外国債、外国株にそれぞれ、0% 40%、50%、 10%投資するポートフォリオであり、
#この時の、リスクは 1.98　リターンは 0.60である。図で表すと緑の点である。



rabel=c("Stock","CB","FB","Fstock")
r=c(0.7950314, 0.7601103, 0.4350965, 0.7795974);
V=rbind(
  c(25.06244, 10.77078, -0.70339,  5.89855),
  c(10.77078,  7.92962, -0.61954,  2.94978),
  c(-0.70339, -0.61954,  6.75469,  7.52813),
  c( 5.89855,  2.94978,  7.52813, 21.63620))

mr=as.matrix(r);mV=as.matrix(V);
risk=as.matrix(sqrt(diag(V)));

risk

#bit better plot with maptools package
library(maptools)
plot(risk,mr, xlab="Risk", ylab="Return",xlim = c(0, 6),ylim = c(0, 0.9))
#pointLabel(x=risk, y=mr, labels=rabel)
pointLabel(x=risk, y=mr, labels=rabel, )

#(a)とあるポートフォリオのリターンとリスク
w=as.matrix(c(0.25,0.25,0.25,0.25))
PV=t(w)%*% mr
PR=t(w)%*% V %*% w

s=seq(0,1,0.1)
M=length(s)

A=matrix(rep(0,2*M), nrow=M, ncol=2)
A
#(b)2銘柄のポートフォリオのリターンとリスク
for (i in 1:M)
{
  w= as.matrix( c(s[i], (1-s[i]),0,0) );
PR=t(w)%*% mr;
PV=t(w)%*% V %*% w;
A[i,]=c(PV, PR)}

library(maptools)
plot(sqrt(A[,1]),A[,2], xlab="Risk", ylab="Return",xlim = c(0, 6),ylim = c(0, 0.9))
pointLabel(x=risk, y=mr, labels=rabel )





#(c)3銘柄のポートフォリオのリターンとリスク
A=matrix(rep(0,220), nrow=55, ncol=4)
W=matrix(rep(0,220), nrow=55, ncol=4)
t=1
for (i in 1:(M-1))
{
for (j in 1:(11-i))
{
  w1=s[i]; w2=s[j]; w3=(1-w1-w2);
  #print(c(i, j))
  W[t,]=c(w1, w2, w3,0)
  print(c(w1, w2, w3,0))
  w= as.matrix( c(w1, w2, w3,0) );
  PR=t(w)%*% mr;
  PV=t(w)%*% V %*% w;
  A[t,]=c(PV, PR)
  t=t+1;
  }
}

library(maptools)
plot(sqrt(A[,1]),A[,2], xlab="Risk", ylab="Return",xlim = c(0, 6),ylim = c(0, 0.9), pch=4)
par(new=T)  
plot(risk,mr,xlim = c(0, 6),ylim = c(0, 0.9), pch=19,col="red")
pointLabel(x=risk, y=mr, labels=rabel )






#(c)4銘柄のポートフォリオのリターンとリスク
MVPS=6; MVPR=0; MVPW=c(0,0,0,0)

A=matrix(rep(0,440), nrow=220, ncol=2)
W=matrix(rep(0,880), nrow=220, ncol=4)
t=1
for (i in 1:10)
{
for (j in 1:(11-i))
 {
  for (k in 1:(12-(i+j)))
  {
    w1=s[i]; w2=s[j]; w3=s[k]; w4=(1-w1-w2-w3);
    #print(c(i, j))
    W[t,]=c(w1, w2, w3,w4)
    #print(c(w1, w2, w3,w4))
    w= as.matrix( c(w1, w2, w3,w4) );
    PR=t(w)%*% mr;
    PV=t(w)%*% V %*% w;
    A[t,]=c(PV, PR)
    t=t+1;
    if(MVPS>PV){ # Minimum variance protfolio
      MVPS=PV; MVPR=PR;
      MVPW=c(w1, w2, w3,w4)
      }
  }
 }
}

mr1=mr+0.07
risk1=risk+0.4

library(maptools)
plot(sqrt(A[,1]),A[,2], xlab="Risk", ylab="Return",xlim = c(0, 6),ylim = c(0, 0.9), pch=4,col="blue")
par(new=T)  
plot(risk,mr,xlim = c(0, 6),xlab=" ", ylab=" ", ylim = c(0, 0.9), pch=19,col="red")
pointLabel(x=risk1, y=mr1, labels=rabel )
par(new=T)  
plot(sqrt(MVPS),MVPR,xlim = c(0, 6),xlab=" ", ylab=" ", ylim = c(0, 0.9), pch=19,col="green")
pointLabel(x=sqrt(MVPS)-0.1, y=MVPR, labels="MVP" )

#result
round(c(sqrt(MVPS), MVPR),2)
(MVPW)

