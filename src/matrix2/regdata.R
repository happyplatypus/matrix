library(data.table)
library(plotly)
library(googleVis)
options(viewer=NULL)


### datagen work
file<-"/home/puru/data/datagen-levy299.txt"
d<-getregdata(file)
getregdata<-function(file){
d<-fread(file)
head(d)
d<-d[,!c('last-entry'),with=FALSE]
## convert to regdata
tics<-unique(d$tic)
tics
dall<-NULL
ii=2
for (ii in 1:length(tics)){
d1<-d[tic %in% tics[ii] ]
p<-d1$price
N<-length(p)

p1 <- plot_ly(d1, y = ~`price`,type="area")
p1%>%layout(title = "dprice")
print(p1)

## 15 minutes forward
d1$fp<-(c(p[15:N],rep(NA,14)))
d1<-d1[complete.cases(d1),]
d1$dy<-d1$fp-d1$price
d1<-d1[,c("dy","returns-from-open",   "recent-returns" ,"stdev-ema-20"     ,   "volume-ema-20"     ,  "dispersion","pseudo-price-ema-5"  ,"pseudo-price-ema-26", "returns-quality","bidprice" ,"askprice"),with=FALSE]
d1$bidask<-d1$askprice-d1$bidprice
d1$emadiff<-d1$`pseudo-price-ema-5`-d1$`pseudo-price-ema-26`
d1<-d1[,!c("askprice" ,"bidprice", "pseudo-price-ema-5", "pseudo-price-ema-26"),with=FALSE]
head(d1)
dall<-rbind(dall,d1)
}
return(dall)
}

head(d1)
Table <- gvisTable(d)
plot(Table)
p <- plot_ly(d, y = ~`dy`,type="area")
p%>%layout(title = "price")


files<-tail(fread("~/data/files"),10)[[1]]
dd<-Map(getregdata,files)
data<-do.call("rbind",dd)
p <- plot_ly(data, y = ~`dy`,type="area")
p%>%layout(title = "dprice")

head(data)
take<-floor(1*nrow(data))
data1<-data[order(-abs(data$`returns-from-open`))][1:take]
p <- plot_ly(data1, y = ~`dy`,type="area")
p%>%layout(title = "dprice")

nrow(data1)
head(data1)
tail(data1)
cor(data1)
colnames(data1)
m<-mean(data1$dy)
sd<-sqrt(var(data1$dy))
data1$dy<-data1$dy/sd
sqrt(var(data1$dy))

summary(m<-lm(dy ~ -1+`returns-from-open`+bidask+emadiff,data=data1))
data1$signal<-as.matrix(data1[,c('returns-from-open'          ,    'bidask'     ,        'emadiff') ,with=FALSE])%*%coef(m)

coef(m)

### for training NN
data1$dy<-sign(data1$dy)
head(data1)
write.table(data1,"/home/puru/Dropbox/data/regdata.csv",row.names=F,col.names=FALSE,quote=F)

#sd<-sqrt(var(data1$`returns-quality`))

#data1$signal<-data1$`returns-quality`/sd
cor(data1$signal,data1$dy)
sqrt(var(data1$signal))

p <- plot_ly(data1, y = ~`signal`,type="area")
p%>%layout(title = "all")

p <- plot_ly(data1,y=~dy, x = ~`signal`)
p%>%layout(title = "all")



data1$idx<-1:nrow(data1)
data_melt<-melt(data1,id.vars = 'idx')
p <- plot_ly(data_melt, y = ~`value`,color=~variable,type="area")
p%>%layout(title = "all")


## fit model

require(glmnet)
# Data = considering that we have a data frame named dataF, with its first column being the class
dataF<-data1
x <- as.matrix(dataF[,-1,with=FALSE]) # Removes class
y <- as.double(as.matrix(dataF[, 1,with=FALSE])) # Only class

# Fitting the model (Lasso: Alpha = 1)
set.seed(999)
#cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')
cv.lasso <- cv.glmnet(x, y,  alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc',intercept=FALSE)
#?cv.glmnet

# Results
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, s=cv.lasso$lambda.min)
str(cv.lasso)
predict(cv.lasso,newx=x)
v<-data.table(y,'predicted'=)
v_melt<-melt(v)
p <- plot_ly(v_melt, y = ~`value`,color=~variable,type="area")
p%>%layout(title = "all")

p <- plot_ly(v, y = ~`predicted.1`,type="area")
p%>%layout(title = "all")














d1<-d[tic %in% c("YRD") ]
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
