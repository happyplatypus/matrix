rm(list=ls(all=TRUE))
library(data.table)
library(plotly)
library(googleVis)
options(viewer=NULL)


### datagen work
tic<-"BPMC"
tickers<-fread("/home/puru/Dropbox/data/corr_tickers.data",header=F)$V1

file<-paste0("/home/puru/Downloads/stooq/",tolower("bpmc"),".us.txt")
d<-fread(file)
head(d)

dates<-unique(d$Date)
date<-dates[1]
#tic<-"TSRO"
for (date in dates){
dall<-NULL
for (tic in tickers) {
file<-paste0("/home/puru/Downloads/stooq/",tolower(tic),".us.txt")
d<-fread(file)
head(d)
tail(d)
d1<-d[Date==date]
d1$ticker<-tic
d1$Close<-log(d1$Close)-log(d1$Open[1])
d1$Volume<-cumsum((d1$Volume))
d1$time<-1:nrow(d1)
dall<-rbind(dall,d1)
}
p1 <- plot_ly(dall, y = ~`Close`,color=~ticker, x = ~`time`)

p2<-p1%>%layout(title = date)
print(add_paths(p2, linetype = ~ticker))
#print(p2)

#Table <- gvisTable(dall[ticker=='TSRO'])
#plot(Table)



}


dall[ticker=='TSRO']



##test
p <- plot_ly(iris, x = ~Sepal.Width, y = ~Sepal.Length) 
add_paths(p, linetype = ~Species)
##test



#getdata('BPMC')


#p1 <- plot_ly(d1, y = ~`Volume`,type="area",color=~ticker)
#p1%>%layout(title = "price")




  dall<-NULL
  check<-'ZGNX'
  ii=1
  date="2017-12-19"
 dall<-NULL
   for (tic in tickers) {
    file<-paste0("/home/puru/Downloads/stooq/",tolower(tic),".us.txt")
    d<-fread(file)
    head(d)
    tail(d)
    d1<-d
    colnames(d1)
    normal<-c(0,diff(log(d1$Close)))
    N<-length(normal)
    if (tic==check)
    {#print(ii);
      normal<-c(normal[2:N],0)}
    d1$Close<-normal
    colnames(d1)<-paste0(colnames(d1),".",tic)
    dall<-cbind(dall,d1$Close)
    #ii=ii+1
  }

  dim(dall)
  dim(cor(dall))
order(cor(dall)[1,])
cor(dall)[1,][order(cor(dall)[1,])]
tickers[27]
tickers[1]



### cross section analysis
dates
date="2017-12-05"
#dall<-NULL
tic<-tickers[1]
print(tic)
file<-paste0("/home/puru/Downloads/stooq/",tolower(tic),".us.txt")
d<-fread(file)
head(d)
tail(d)
d1<-d[Date==date]
colnames(d1)
normal<-cumsum(c(0,diff(log(d1$Close))))
N<-length(normal)
d1$Close<-normal
colnames(d1)<-paste0(colnames(d1),".",tic)
#grep(colnames(d1),"Close")
dall<-d1[,grep("Close",colnames(d1)),with=FALSE] 


for (tic in tickers[2:length(tickers)]) {
  print(tic)
  file<-paste0("/home/puru/Downloads/stooq/",tolower(tic),".us.txt")
  d<-fread(file)
  head(d)
  tail(d)
  d1<-d[Date==date]
  colnames(d1)
  normal<-cumsum(c(0,diff(log(d1$Close))))
  N<-length(normal)
  d1$Close<-normal
  colnames(d1)<-paste0(colnames(d1),".",tic)
  #grep(colnames(d1),"Close")
  dall<-cbind(dall, d1[,grep("Close",colnames(d1)),with=FALSE] )
  #ii=ii+1
}


dall
order(dall[78])

dall[78][,order(dall[78]),with=FALSE]
dall$idx<-1:nrow(dall)
dallm<-melt(dall,id.vars = 'idx')

dallm

p1 <- plot_ly(dallm, y = ~`value`,type="area",color=~variable)
p1%>%layout(title = "price")

dall<-dall[,!c('idx'),with=FALSE]

n=23
dall[n]

m<-mean(as.numeric(dall[n]))
s<-sqrt(var(as.numeric(dall[n])))

zscores<-(dall[n]-m)/s
order(as.numeric(zscores))

zscores[,order(as.numeric(zscores)),with=FALSE]
zscores[zscores>2]


### needs to be written in clj

## when zscore> 2 enter exit on drawdown, ttc



d<-fread('/home/puru/Dropbox/data/results-NKTRbig.data')
d$idx<-1:nrow(d)
p1 <- plot_ly(d, y = ~`V21`,color=~V3, x = ~`V19`)
p1

p1 <- plot_ly(d, y = ~`V21`,color=~V3, x = ~`idx`)
p1

p1 <- plot_ly(d, y = ~`V19`,color=~V3, x = ~`idx`)
p1


d1<-d[,V21:=pmax(V21,-2000)]
p1 <- plot_ly(d1, y = ~`V21`,color=~V3, x = ~`idx`)
p1
mean(d1$V21)
