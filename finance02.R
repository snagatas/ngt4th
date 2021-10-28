# Financial Data Analysis Lecture  by S.Nagata
# Original article: "K. Inui(2011)"
# Modified by S.Nagata for Stat. Lectures in KGU

graphics.off(); rm(list=ls(all=TRUE));  # 
#windowsFonts(gothic=windowsFont("MS Gothic")); par(family="gothic");  #for windows 
par(family= "HiraKakuProN-W3") #for mac

# read csv file on web 
# https://github.com/snagatas/ngt4th

urlr1="https://raw.githubusercontent.com/snagatas/ngt4th/main/six_results_utf.csv"
urlr2="https://raw.githubusercontent.com/snagatas/ngt4th/main/six_returns_utf.csv"
#urlr1="https://raw.githubusercontent.com/snagatas/ngt4th/main/six_results_jis.csv"
#urlr2="https://raw.githubusercontent.com/snagatas/ngt4th/main/six_returns_jis.csv"
dat_results = read.csv(urlr1, fileEncoding="utf8")
dat_return = read.csv(urlr2, fileEncoding="utf8")

# Excelで計算した結果の確認
a_return = dat_results[2,] ;   #subset avarage return series.
risk = dat_results[1,] ;       #subset risk return series.
Sigma = dat_results[-c(1:2),]; #分散共分散行列の抽出

#Excelでは、196502から199804までの398個データからすいけいしているので注意
d_r= dat_return[1:398,-1] ;  #subset avarage return series.

#(1)平均リターンと分散共分散行列の計算
a_r=round(colMeans(d_r),5)
rm=as.matrix(d_r)
ar=as.matrix(a_r)
rbind(a_r, a_return) #平均リターン　Excelの結果を再現成功!!

CV=(t(rm) %*% rm )/398 -( ar %*%  t(ar))
CV=round(CV,5)
rbind(CV, Sigma) #分散共分散行列　Excelの結果を再現成功!!




#plot risk-return-surface 
re_ri=dat_results[c(1:2),];
class(re_ri);
re_ri2=as.data.frame(t(re_ri));
re_ri2
# it should be noted that dataflame object will be an onject as matrix after transpose  
# https://qiita.com/emihat/items/1130b15152621a5e37f5

#primitive plot
plot(re_ri2)
colnames(re_ri2) <- c("CC","ab")

#bit better plot with maptools package
library(maptools)
plot(re_ri2$CC, re_ri2$ab, xlab="リスク", ylab="リターン")
pointLabel(x=re_ri2$CC, y=re_ri2$ab, labels=rownames(re_ri2))

library(ggplot2)
library("ggrepel")

data=re_ri2
g <- ggplot(data,  aes (x = data[,1],    y = data[,2],  colour=data[,2],  label = rownames(data) ) )

g <- g +  geom_point(
  size = 4
)

g <- g + geom_text_repel( family = "HiraKakuPro-W3")
g <- g + xlab("リスク")
g <- g + ylab("リターン")
g <- g + theme_gray (base_family = "HiraKakuPro-W3")

plot(g)
