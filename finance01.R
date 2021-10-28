# Financial Data Analysis Lecture  by S.Nagata
# Original article: "K. Inui(2011)"
# Modified by S.Nagata for Stat. Lectures in KGU

graphics.off(); rm(list=ls(all=TRUE));  # 既定義の変数とグラフを消去
#windowsFonts(gothic=windowsFont("MS Gothic")); par(family="gothic");  #for windows 
par(family= "HiraKakuProN-W3") #for mac

# read csv file on web 
# url="http://www.e-stat.go.jp/SG1/estat/Csvdl.do?sinfid=000011288561"

urlr1="https://raw.githubusercontent.com/snagatas/ngt4th/main/six_results_utf.csv"
urlr2="https://raw.githubusercontent.com/snagatas/ngt4th/main/six_returns_utf.csv"

dat_results = read.csv(urlr1, fileEncoding="utf8")

dat_return = read.csv(urlr1, fileEncoding="utf8")

a_return = dat_results[2,] ;  #subset avarage return series.
risk = dat_results[1,] ;      #subset risk return series.
Sigma = dat_results[-c(1:2),];#分散共分散行列の抽出


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

