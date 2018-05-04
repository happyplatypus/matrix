library(data.table)
library(plotly)
options(viewer=NULL)
library(googleVis)



d<-fread("/home/puru/data/factors-small-Telugu28438.txt")
d<-d[complete.cases(d)]
head(d)
nrow(d)
p <- plot_ly(d, x=~`ATR-bps`,y = ~`close-open-bps`,color = ~ticker)
p%>%layout(title = "co vs atr")

d[order(-d$`close-open-bps`)]


d[order(-d$`ATR-bps.1`)]



## i want to focus on 1sigma to 3 sigma guys 
two_sigma<-quantile(d$`close-open-bps`,0.95)
one_sigma<-quantile(d$`close-open-bps`,0.68)
three_sigma<-quantile(d$`close-open-bps`,0.99)

d1<-d[ d$`close-open-bps` < three_sigma & d$`close-open-bps` > one_sigma]

p <- plot_ly(d1, x=~`ATR-bps`,y = ~`close-open-bps`,color = ~ticker)
p%>%layout(title = "co vs atr")

p <- plot_ly(d1, x=~`consolidated-trend2`,y = ~`close-open-bps`,color = ~ticker)
p%>%layout(title = "co vs ctrend2")

p <- plot_ly(d1, x=~`close-open-bps.1`,y = ~`close-open-bps`,color = ~ticker)
p%>%layout(title = "co vs co.1")

p <- plot_ly(d1, x=~`volume-change-bps`,y = ~`close-open-bps`,color = ~ticker)
p%>%layout(title = "co vs v change")

head(d1)
cor(d1[,!c('ticker'),with=FALSE])

#d[order(-d$`ATR-bps`)]





d1<-d[ ticker=='MDRX']
head(d1)
d1$idx<-1:nrow(d1)
as.Date(as.character(d1$date[1]),format="%Y%m%d")
convert<-function(x){
as.Date(as.character(x),format="%Y%m%d")
  }
d1[,Date:=convert(date)]

head(d1)
p <- plot_ly(d1, x=~`Date`,y = ~`close-open-bps`,color = ~ticker)
p

p <- plot_ly(d1, x=~`Date`,y = ~`ATR-bps`,color = ~ticker)
p

p <- plot_ly(d1, x=~`Date`,y = ~`volume-change-bps`,color = ~ticker,type="area")
p










### datagen work
d<-fread("/home/puru/data/datagen-specimen299.txt")
head(d)
tics<-unique(d$tic)
tics
d1<-d[tic %in% tics[1] & `agent-name` =='1' ]
Table <- gvisTable(d1)
plot(Table)

d1$`agent-name`<-as.factor(d1$`agent-name`)
p <- plot_ly(d1, x=~time,y = ~`total-pnl`, color = ~`agent-name`)
p%>%layout(title = "total-pnl")



head(d1)
d1<-d1[date==20171115]
d1<-d1[`agent-name`=='desegregates24620']
#table(d1)
library(googleVis)
Table <- gvisTable(d1)
plot(Table)

d1<-d[tic %in% c("BITA") & date==20170825]
#d1<-d[tic %in% c("BLMN", "BIVV" ,"HAIN" ,"CAVM", "CENX" ,"CORT" ,"ANF" ,"SNAP" ,"GPK", "GPOR" ,"SEAS", "CZR")]

tail(d1)
head(d1)
library(plotly)


p <- plot_ly(d1, x=~time,y = ~`current-position`, color = ~`agent-name`)
p%>%layout(title = "position")

p <- plot_ly(d1, x=~time,y = ~`price`, color = ~`agent-name`)
p%>%layout(title = "position")


p <- plot_ly(d1, x=~time,y = ~`ttc`, color = ~`agent-name`,title="ttc")
p%>%layout(title = "ttc")

p <- plot_ly(d1, x=~time,y = ~`pnl`, color = ~tic,type="area")
p%>%layout(title = "pnl")

p <- plot_ly(d1, x=~time,y = ~`m2m-pnl-bps`, color = ~tic)
p%>%layout(title = "m2m")

p <- plot_ly(d1, x=~time,y = ~`max-pnl-this-round`, color = ~tic)
p%>%layout(title = "max-pnl")

p <- plot_ly(d1, x=~time,y = ~`total-pnl`, color = ~`agent-name`)
p%>%layout(title = "total-pnl")


p <- plot_ly(d1, x=~time,y = ~`drawdown`,type="area")
p%>%layout(title = "dd")

p <- plot_ly(d1, x=~time,y = ~dispersion, color = ~tic)
p%>%layout(title = "dispersion")


View(d1)

d1$bidask<-d1$askprice-d1$bidprice
  
p <- plot_ly(d1, x=~time,y = ~`returns-from-open`, color = ~`agent-name`)
p

p <- plot_ly(d1, x=~time,y = ~`price-ema-5`)
p

p <- plot_ly(d1, x=~time,y = ~`price-ema-26`)
p

p <- plot_ly(d1, x=~time,y = ~`price-ema-12`)
p


p <- plot_ly(d1, x=~time,y = ~`bidask`, color = ~tic,type="area")
p


d1<-d[tic %in% c("XON")]
head(d1)
tail(d1)
## try price forecasting
p<-d1$price
N<-length(p)
N
p
## 15 minutes forward
d1$fp<-(c(p[15:N],rep(NA,14)))
d1<-d1[complete.cases(d1),]
d1$dy<-d1$fp-d1$price


p <- plot_ly(d1, x=~time,y = ~`dy`, color = ~tic)
p


## try price forecasting
d1<-d[tic %in% c("VALE") ]
head(d1)
#d1<-d1[date==20171114]

head(d1)
d2<-d1[,c('time','pseudo-price-ema-5','pseudo-price-ema-26','returns-quality'),with=FALSE]
#d3<-d1[,c('time','returns-quality'),with=FALSE]
#head(d3)
d2$`ema-diff`<-d2$'pseudo-price-ema-5'-d2$'pseudo-price-ema-26'
head(d2)
d3<-melt(d2,id.vars = 'time')
p <- plot_ly(d3, x=~time,y = ~value, color = ~variable)
p











#head(scale(prices[,-1,with=FALSE]))


head(d1,200)
prices<-d1[,mean(V3),by=V2]

head(prices)
prices[order(prices$V1)]



d2<-(d[V2=='GE'&V1==20171117])
head(d2)
N<-nrow(d2)
N
d2<-d2[2:N]
matplot((d2[,c(4),with=FALSE]),type='l')

head(d1)
dcast(d1,V2~V3,id.vars=c('V3'))

library(tidyr)
set.seed(14)
stocks <- data.frame(time = as.Date('2009-01-01') + 0:9,
                     X = rnorm(10, 0, 1),
                     Y = rnorm(10, 0, 2),
                     Z = rnorm(10, 0, 4))
stocksm <- gather(stocks, stock, price, -time)
head(stocksm)
spread.stock <- spread(stocksm, stock, price)
head(spread.stock)


matplot(scale(d2[,c(3,4,5,6),with=FALSE]),type='l')

plot(d[V2=='ABBV'&V1==20171117]$V6)
plot(d[V2=='ABBV'&V1==20171117]$V3)
plot(d[V2=='ABBV'&V1==20171117]$V5)



plot(d[V2=='BAC'&V1==20171117]$V6)
plot(d[V2=='BAC'&V1==20171117]$V3)
plot(d[V2=='BAC'&V1==20171117]$V5)

#plot(d$V3)
