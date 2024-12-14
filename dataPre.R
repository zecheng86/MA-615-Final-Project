library(data.table)
library(ggplot2)
library(plotly)
library(glue)
readLines('data.txt')->info
grep('^#',info)->tou
cbind(tou,c(tail(tou,-1)-2,length(info)))->ww


lapply(1:nrow(ww),\(ii) 
{
ww[ii,]->x
info[x[1]:x[2]]->inter
inter[0-1:2]->inter
strsplit(inter,"\\s+")->inter
do.call('rbind',inter)->inter
as.data.table(inter)->inter
names(inter)<-c('year','value')
inter[,year:=as.integer(year)]
inter[,value:=as.numeric(gsub(",","",value))]
n2<-'value'
if(ii %% 3==1) n2<-'population'
if(ii %% 3==2) n2<-'GDP'
if(ii %% 3==0) n2<-'Tourist'
inter[,what:=n2]
inter[,country:=fcase(ii %in% 1:3,'Cook',ii %in% 4:6,'Fiji',ii %in% 7:9,'Tonga',default='')]
inter
})->dat

rbindlist(dat)->resu


###timeseries and forcast
library(forecast)

resu[,.N,.(what,country)]
split(resu,resu[,.(what,country)])->fen

lapply(fen,function(x)
{
x[order(year)]->x
x$value->xv
inter.ts<-ts(xv, start = x$year[1], frequency = 1)
inter.fit <- auto.arima(inter.ts)
inter.pred<-forecast(inter.fit, h = 2)
as.data.table(as.data.frame(inter.pred),keep='year')->inter.ok
inter.ok[,type:='Forecast']
setnames(inter.ok,'Point Forecast','value')
inter.ok[,what:=unique(x$what)]
inter.ok[,country:=unique(x$country)]
inter.ok
})->preds

rbindlist(preds)->preds


save(resu,preds,file='dataUsed.RData')








