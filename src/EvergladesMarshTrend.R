## 
## Everglades Marsh trends
##
## Code was compiled by Paul Julian
## contact info: paul.julian@floridadep.gov

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
# Data Wrangling
library(AnalystHelper);
library(plyr);
library(reshape)

library(zoo)
# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(tmap)

# Data Vis
library(flextable)
library(magrittr)

# Paths
wd="C:/Julian_LaCie/_Github/EvergladesMarshTrends"
paths=paste0(wd,c("/Exports","/Plots","/Data","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
export.path=paths[1]
plot.path=paths[2]
data.path=paths[3]

GIS.path="C:/Julian_LaCie/_GISData"

# Helper variables
nad83.pro=CRS(SRS_string = "EPSG:4269")
utm17=CRS(SRS_string ="EPSG:26917")

tmap_mode("view")
# -------------------------------------------------------------------------
# Update for updated analysis
CurWY=2020


# GIS Data ----------------------------------------------------------------
## GIS Data 
# personal geodatabase 
db.path=paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb")
ogrListLayers(db.path)

evpa=spTransform(readOGR(db.path,"EPA_Boundary"),wkt(utm17))
rs.feat=spTransform(readOGR(db.path,"RestorationStrategies_Features"),wkt(utm17))
sta=spTransform(readOGR(db.path,"EvergladesSTAs"),wkt(utm17))
bcnp=spTransform(readOGR(db.path,"BCNP"),wkt(utm17))
canal=spTransform(readOGR(db.path,"SFWMD_Canals"),wkt(utm17))
shore=spTransform(readOGR(db.path,"SFWMD_Shoreline"),wkt(utm17))
eaa=spTransform(readOGR(db.path,"EvergladesAgriculturalArea"),wkt(utm17))
wma=spTransform(readOGR(db.path,"HoleyLand_Rotenberger"),wkt(utm17))
wca=spTransform(readOGR(db.path,"WCAs"),wkt(utm17))
enp.shore=spTransform(readOGR(db.path,"Shoreline_ENPClip"),wkt(utm17))
c139=spTransform(readOGR(db.path,"C139Annex"),wkt(utm17))
enp=spTransform(readOGR(db.path,"ENP"),wkt(utm17))

# https://geo-sfwmd.hub.arcgis.com/datasets/environmental-monitoring-stations
wmd.mon=spTransform(readOGR(paste0(GIS.path,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),wkt(utm17))
wmd.mon=subset(wmd.mon,ACTIVITY_S=="Surface Water Grab")

## Station List
station.list.LNWR=data.frame(Station.ID=c(paste0("LOXA",c(101,130,124,135:140,104:108,"104.5","107U","Z1","Z2","Z3","Z4")),
                                     paste0("LOX",3:16),"X1","X4","Z1"),
                             Region="LNWR")
station.list.LNWR$alias=station.list.LNWR$Station.ID
station.list.LNWR$alias=with(station.list.LNWR,ifelse(alias=="Z1","LOXAZ1",as.character(alias)))

station.list.wca2=data.frame(Station.ID=c(paste0("2AN",c(".25",1,2,4,5,6)),
                                          paste0("2AC",c(".25",2,4,5)),
                                          "2AFS.25","FS1","FS3","404Z1",
                                          paste0("CA2",c(6,8,9,17,22:24)),
                                          paste0("WCA2",c("F1","F2","F4")),"F3","F5","U3","E5","U1"),
                             Region="WCA2")
station.list.wca2$alias=station.list.wca2$Station.ID

station.list.wca3=data.frame(Station.ID=c(paste0("CA3",c(2:6,8:9,11,14:19,24:25)),
                                          "3ASMESO","CA3B1","S345B6","CA3B2"),
                             Region="WCA3")
station.list.wca3$alias=station.list.wca3$Station.ID

station.list.ENP=data.frame(Station.ID=c(paste0("SRS",c("1B","1C",2)),
                                         "NP201","N01","NE0","NE1","G-3273","RG1","CR2","TSB","EP",
                                         paste0("P",c(33,34,36,37))),
                            Region="ENP")
station.list.ENP$alias=station.list.ENP$Station.ID

station.list=rbind(station.list.LNWR,station.list.wca2,station.list.wca3,station.list.ENP)

plot(evpa,border="red")
plot(wca,add=T,col="ivory")
plot(gSimplify(enp.shore,500),add=T,col="ivory")
# plot(subset(wmd.mon,STATION%in%station.list$Station.ID),add=T)
plot(subset(wmd.mon,STATION%in%station.list.LNWR$Station.ID),add=T,pch=21)
plot(subset(wmd.mon,STATION%in%station.list.wca2$Station.ID),add=T,pch=21,bg="red",col="red")
plot(subset(wmd.mon,STATION%in%station.list.wca3$Station.ID),add=T,pch=21,bg="green",col="green")
plot(subset(wmd.mon,STATION%in%station.list.ENP$Station.ID),add=T,pch=21,bg="blue",col="blue")

# tm <- tm_basemap(leaflet::providers$Esri.WorldImagery,alpha=0.9)+
#   tm_shape(evpa,name="Everglades Protection Area")+tm_borders(col="red",alpha=0.5,lwd=2)+
#   tm_shape(subset(wmd.mon,STATION%in%station.list$Station.ID),name="Monitoring Sites")+
#   tm_dots(col="dodgerblue1",size=0.1)+tm_text("STATION",col="white",fontface=3,ymod=0.1)
# 

# Water Quality Data ------------------------------------------------------
dates=date.fun(c("1978-05-01",paste0(CurWY,"-04-30")))
parameters=data.frame(Test.Number=c(23,25),param=c("OPO4","TP"))

pb=txtProgressBar(min=1,max=nrow(station.list),style=3)
wq.dat=data.frame()
for(i in 1:nrow(station.list)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],station.list$Station.ID[i],parameters$Test.Number)
  wq.dat=rbind(wq.dat,tmp)
  setTxtProgressBar(pb,i)
}

wq.dat=merge(wq.dat,station.list,"Station.ID")
wq.dat=merge(wq.dat,parameters,"Test.Number")
wq.dat$TPWeek=biweek.period(wq.dat$Date.EST)
wq.dat$WY=WY(wq.dat$Date.EST)

P.dat.xtab=cast(wq.dat,alias+Region+Date.EST+WY+TPWeek~param,value="HalfMDL",mean)
P.dat.xtab$OPFlag=with(P.dat.xtab,ifelse(is.na(OPO4)|OPO4==0,1,0));
P.dat.xtab$TPFlag=with(P.dat.xtab,ifelse(is.na(TP),1,0));
P.dat.xtab$Reversal=with(P.dat.xtab,ifelse(P.dat.xtab$OPFlag==1,0,ifelse(OPO4>(TP*1.3),1,0)));
plot(TP~OPO4,P.dat.xtab,ylab="TP (mg L\u207B\u00b9)",xlab="SRP (mg L\u207B\u00b9)",pch=21,bg=ifelse(Reversal==1,"red",NA),col=adjustcolor("grey",0.8),log="xy");abline(0,1,col="red")

sum(P.dat.xtab$Reversal,na.rm=T)

P.dat.xtab$TP=with(P.dat.xtab,ifelse(Reversal==1,NA,TP));# Removes Reversal 
P.dat.xtab$TP_ugL=P.dat.xtab$TP*1000
P.dat.xtab$HydroSeason=FL.Hydroseason(P.dat.xtab$Date.EST)

## Biweekly mean TP data + Data Screening
TP.dat=ddply(P.dat.xtab,
                 c("Region","alias","WY","TPWeek","HydroSeason"),summarise,
                 AMean=mean(TP_ugL,na.rm=T),
                 N=N.obs(TP_ugL),
             min.date=min(Date.EST));
TP.dat.screen=ddply(TP.dat,c("alias","WY","HydroSeason"),summarise,N=N.obs(AMean));
TP.dat.screen=cast(TP.dat,alias+WY~HydroSeason,value="N",sum);

TP.dat.screen$NTotal=with(TP.dat.screen,A_Wet+B_Dry);
TP.dat.screen$SeasonScreen=with(TP.dat.screen,ifelse(B_Dry>=1&A_Wet>=1,1,0))
TP.dat.screen$NScreen=with(TP.dat.screen,ifelse(NTotal>=6,1,0));
TP.dat.screen$UseData=with(TP.dat.screen,ifelse((SeasonScreen+NScreen)==2,"Yes","No"));

TP.dat=merge(TP.dat,TP.dat.screen[,c("alias","WY","UseData")],by=c("alias","WY"),all.x=T)
range(TP.dat$WY)

TP.dat$month=as.numeric(format(TP.dat$min.date,"%m"))
TP.dat$CY=as.numeric(format(TP.dat$min.date,"%Y"))
TP.dat$month.f=factor(TP.dat$month,levels=c(5:12,1:4))

plot(AMean~month,subset(TP.dat,alias=='LOX11'))
plot(AMean~min.date,subset(TP.dat,alias=='LOX11'))



# seasonal trend ----------------------------------------------------------
# https://fawda123.github.io/swmp_workshop_2016/training_modules/module4_kendall/kendall.pdf

library(EnvStats)
sea.kendall=data.frame()
sites.val=unique(TP.dat$alias)
pb=txtProgressBar(min=1,max=length(sites.val),style=3)
for(i in 1:length(unique(TP.dat$alias))){
tmp.dat=subset(TP.dat,alias==sites.val[i]&UseData=="Yes"&is.na(AMean)==F)
test=kendallSeasonalTrendTest(AMean~month+CY,data=tmp.dat)

rslt=data.frame(alias=sites.val[i],
           ChiSq=as.numeric(test$statistic[1]),pval.Chi=as.numeric(test$p.value[1]),
           z=as.numeric(test$statistic[2]),pval.z=as.numeric(test$p.value[2]), df=as.numeric(test$parameters),N.samp=nrow(tmp.dat),
           tau=as.numeric(test$estimate[1]),slope=as.numeric(test$estimate[2]),
           slope.LCL=as.numeric(test$interval$limits[1]),slope.HCL=as.numeric(test$interval$limits[2]))
sea.kendall=rbind(sea.kendall,rslt)
setTxtProgressBar(pb,i)
}

subset(sea.kendall,tau<0&pval.z<0.05)

# annual trend ------------------------------------------------------------
library(zyp)

# ddply(station.list,c("alias","Region"),summarise,N.val=N.obs(Station.ID))
station.list2=subset(station.list,Station.ID!="Z1")
station.list2$id.val=1:nrow(station.list2)

TP.AGM.dat=ddply(subset(TP.dat,UseData=="Yes"),c("alias","WY"),summarise,AGM=exp(mean(log(AMean),na.rm=T)))
ann.kendall=ddply(TP.AGM.dat,"alias",summarise,
                  tau=cor.test(AGM,WY,method="kendall")$estimate,pval=cor.test(AGM,WY,method="kendall")$p.val,
                  slope=as.numeric(coef(zyp.sen(AGM~WY))[2]),
                  LCL=as.numeric(confint(zyp.sen(AGM~WY))[2,1]),
                  UCL=as.numeric(confint(zyp.sen(AGM~WY))[2,2]),
                  N.samp=N.obs(AGM))
# write.csv(ann.kendall,paste0(export.path,"/ann_kendall.csv"),row.names=F)
ann.kendall=ann.kendall2
subset(ann.kendall,tau<0&pval<0.05)
nrow(subset(ann.kendall,tau<0&pval<0.05))
nrow(ann.kendall)
range(subset(ann.kendall,tau<0&pval<0.05)$slope)

subset(ann.kendall,tau>0&pval<0.05)
subset(ann.kendall,tau>0)

ann.kendall$pval.txt=with(ann.kendall,ifelse(pval<0.01,"<0.01",format(round(pval,2),nsmall=2)))
ann.kendall$tau=round(ann.kendall$tau,2)
ann.kendall$slope=round(ann.kendall$slope,2)
ann.kendall$LCL=round(ann.kendall$LCL,2)
ann.kendall$UCL=round(ann.kendall$UCL,2)

ann.kendall=merge(ann.kendall,station.list2,"alias")
ann.kendall=ann.kendall[,c("Region","alias","tau","pval.txt","slope","LCL","UCL","N.samp")]
ann.kendall=ann.kendall[order(match(ann.kendall$Region,c("LNWR","WCA2","WCA3","ENP")),ann.kendall$alias),]

# library(DT)
# datatable(ann.kendall,filter="top",rownames = NA,escape=F,
#           colnames=c("Region","Station","Kendall's<br>\u03C4","\u03C1-value","Thiel-Sen<br>Slope","Lower<br>CI","Upper<br>CI","N<br>(WYs)"),
#           extensions="Buttons",
#           options=list(
#   dom="Bfrtip",
#   buttons=c("copy","csv","excel"),
#   columnDefs=list(list(className='dt-center',targets=2:7))
# ))


TP.AGM.dat=merge(TP.AGM.dat,station.list2[,c("alias","Region","id.val")],"alias")
# png(filename=paste(plot.path,"/WY",CurWY,"_AnnualSamples.png",sep=""),width=7,height=4,units="in",res=200,type="windows",bg="white")

region.val=c("LNWR","WCA2","WCA3","ENP")
par(family="serif",mar=c(2,2,2,1.75),oma=c(2.5,3,0.1,1))#,mfrow=c(3,1))
layout(matrix(1:4,1,4))

xlim.val=c(1979,CurWY);by.x=20;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
for(i in 1:4){
plot(id.val~WY,subset(TP.AGM.dat,Region==region.val[i]),xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=subset(station.list2,Region==region.val[i])$id.val,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(TP.AGM.dat,Region==region.val[i]),points(WY,id.val,pch=21,col=adjustcolor("indianred1",0.75),bg=adjustcolor("indianred1",0.5)))
with(subset(station.list2,Region==region.val[i]),axis_fun(2,id.val,id.val,alias,cex=0.75))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
mtext(side=3,adj=0,region.val[i],cex=0.8)
  }
mtext(side=1,outer=T,"Water Year")
dev.off()

##
# library(fields)
# library(spdep)
# epa.sites=subset(wmd.mon,STATION%in%station.list$Station.ID)
# 
# ann.kendall.shp=merge(epa.sites,ann.kendall,by.x="STATION",by.y="alias",all.y=T)
# ann.kendall.shp=subset(ann.kendall.shp,is.na(slope)==F)
# 
# evpa.r=raster(evpa)
# res(evpa.r)=1000
# 
# subset(ann.kendall.shp,is.na(slope)==T)@data
# 
# cover.design(coordinates(ann.kendall.shp),50)$design
# 
# m=Tps(coordinates(ann.kendall.shp),ann.kendall.shp$slope)
# tps=interpolate(evpa.r,m)
# tps.TP.trend=mask(tps,evpa)
# plot(tps.TP.trend)

col=c("indianred1","lawngreen","grey")
col.vals=col[TP.wmd.mon$Status]
pch.vals=c(22,21,21)[TP.wmd.mon$Status]
bbox=raster::extent(473714,587635,2748300,2960854)
cols=c(rgb(255/255,235/255,175/255,1,"esri.topaz.sand"),
       rgb(190/255,232/255,255/255,1,"esri.light.blue"),
       rgb(151/255,219/255,242/255,1,"esri.light.blue1"),
       rgb(247/255,255/255,231/255,1,"KM.BCNP.col"),
       rgb(109/255,187/255,67/255,1,"esri.green1"))

# tiff(filename=paste(plot.path,"/WY",CurWY,"_TPTrendMap.tiff",sep=""),width=5.5,height=6,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste(plot.path,"/WY",CurWY,"_TPTrendMap.png",sep=""),width=5.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.1,0.1,0.1,0.1),mar=c(0.1,0.1,0.1,0.1))
layout(matrix(c(1,2),1,2,byrow=T),widths=c(1,0.5))

plot(shore,ylim=bbox[3:4],xlim=bbox[1:2],bg="lightblue",border="grey",col="cornsilk",lwd=0.5)
plot(eaa,col="olivedrab1",border="grey",add=T)
plot(c139,col="grey",border=NA,add=T)
plot(sta,col="skyblue",border="grey",lwd=0.1,add=T)
plot(rs.feat,col="steelblue3",border="grey",lwd=0.1,add=T)
plot(wma,col=cols[3],border=NA,add=T)
plot(wca,col="white",add=T)
plot(bcnp,col=cols[4],border=NA,add=T)
plot(enp.shore,col="white",border="dodgerblue3",add=T,lwd=0.5)
plot(canal,lwd=2,col="dodgerblue3",add=T)
plot(canal,lwd=1,col=cols[2],add=T)
plot(evpa,border="red",col=NA,lwd=1.5,add=T)
plot(subset(ann.kendall.shp,pval>0.05),pch=21,bg=adjustcolor('yellow',0.5),col=adjustcolor("black",0.5),cex=0.75,add=T)
plot(subset(ann.kendall.shp,slope<0&pval<0.05),pch=25,bg=adjustcolor('green',0.5),col=adjustcolor("black",0.5),add=T)
plot(subset(ann.kendall.shp,slope>0&pval<0.05),pch=24,bg=adjustcolor('red',0.5),col=adjustcolor("black",0.5),add=T)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4)
box(lwd=1)

plot(0:1,0:1,axes=F,ylab=NA,xlab=NA,type="n")
legend(0.5,0.9,
       legend=c("Significantly\nIncreasing","Not statistically\nsignificant","Significantly\nDecreasing"),
       pch=c(24,21,25),pt.bg=adjustcolor(c("red","yellow","green"),0.5),col="black",lty=0,lwd=0.1,
       pt.cex=1,ncol=1,cex=0.8,bty="n",xpd=NA,xjust=0.5,y.intersp=1.75,x.intersp=0.75,
       title.adj=0,title="Annual Trend Results")
dev.off()

# Data Viz ----------------------------------------------------------------
TP.AGM.dat=merge(TP.AGM.dat,
                 ddply(TP.AGM.dat,"alias",summarise,LTmeanGM=mean(AGM,na.rm=T)),
                 "alias")
TP.AGM.dat$anom=with(TP.AGM.dat,AGM-LTmeanGM)
# write.csv(TP.AGM.dat,paste0(export.path,"/TPAGM.csv"),row.names=F)
# write.csv(TP.dat,paste0(export.path,"/TPdat.csv"),row.names=F)

wca$name3=with(wca@data,ifelse(name2=="WCA1","LNWR",as.character(name2)))
##
unique(station.list2$alias)
for(i in 1:length(unique(station.list2$alias))){
  site.val.input=unique(station.list2$alias)[i]
  
  tmp.dat=subset(TP.dat,alias==site.val.input&UseData=="Yes")
  tmp.dat=merge(tmp.dat,data.frame(month=c(5:12,1:4),month.plt=1:12),"month")
  mo_agg=ddply(tmp.dat,"month.f",summarise,mean.val=mean(AMean,na.rm=T),SE.val=SE(AMean),med.val=median(AMean,na.rm=T),N.val=N.obs(AMean))
  
  tmp.dat.AGM=subset(TP.AGM.dat,alias==unique(tmp.dat$alias))
  
  
# tiff(filename=paste(plot.path,"/WY",CurWY,"_example.tiff",sep=""),width=7,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
png(filename=paste(plot.path,"/site_trend/",unique(station.list2$alias)[i],".png",sep=""),width=8.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3.5,1,1),oma=c(1,1,0.5,1),xpd=F)
layout(matrix(c(1:6),2,3,byrow=F))

cols <- colorRampPalette(c('lightblue', 'lightgreen'))(12)
max.val=ceiling(max(tmp.dat$AMean,na.rm=T)+max(tmp.dat$AMean,na.rm=T)*0.2)
#ylim.val=c(0,max.val);by.y=if(max.val>100){20}else{5};ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.val=c(1,max.val);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(1,12);by.x=1;xmaj=1:12;xmaj.lab=c(5:12,1:4)#seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(AMean~month.plt,tmp.dat,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F,log="y")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp.dat,points(jitter(month.plt,0.75),AMean,pch=21,col=adjustcolor("grey",0.75),bg=adjustcolor("grey",0.5)))
with(mo_agg,points(month.f,mean.val,pch=21,bg=cols,cex=2,lwd=0.5))
axis_fun(1,xmaj,xmaj,xmaj.lab,line=-0.70,cex=0.75)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Monthly distribution",cex=0.8)
mtext(side=2,line=2.5,"TP (\u03BCg L\u207B\u00B9)",cex=0.8)
# mtext(side=3,adj=0,paste0("Station: ",unique(tmp.dat$alias)),cex=0.8)
legend("topleft",legend=c("Monthly POR Mean","Individual Sample"),
       pch=c(21,21),pt.bg=c(cols[1],adjustcolor("grey",0.5)),col=c("black",adjustcolor("grey",0.75)),
       lwd=0.05,lty=NA,
       pt.cex=1,ncol=1,cex=0.75,bty="n",xpd=NA,xjust=0.5,y.intersp=1,x.intersp=0.75)


boxplot(AMean~month.f,tmp.dat,ylim=ylim.val,col=cols,pch=21,bg="grey",ann=F,axes=F,log="y",
        boxlwd=0.8,whisklwd=0.8,staplelwd=0.8,outlwd=0.8,medlwd=1,lwd=0.1,cex=0.8)
axis_fun(1,xmaj,xmaj,xmaj.lab,line=-0.70,cex=0.70)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Monthly distribution",cex=0.8)
mtext(side=2,line=2.5,"TP (\u03BCg L\u207B\u00B9)",cex=0.8)
#mtext(side=3,adj=0,paste0("Station: ",unique(tmp.dat$alias)))

# par(family="serif",mar=c(0.5,0.5,0.5,1),oma=c(3.5,1,0.1,1))
# layout(matrix(1:12,12,1))
# ylim.val=c(0,max.val);by.y=if(max.val>500){100}else{20};ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# for(i in xmaj.lab){
#   hist(subset(tmp.dat,month.f==i)$AMean,ann=F,axes=F,yaxs="i",freq=T,
#        breaks=seq(min(tmp.dat$AMean,na.rm=T),max(tmp.dat$AMean,na.rm=T),diff(range(tmp.dat$AMean, na.rm = T))/30),
#        xlim=ylim.val,col="black",border="lightblue")
#   box(lwd=1)
#   mtext(side=4,las=3,month.abb[i],cex=0.75)
#   #if(i==5){mtext(side=3,adj=0,"Station: LOX11")}
# }
# axis_fun(1,ymaj,ymin,ymaj,line=-0.5);box(lwd=1)
# mtext(side=1,line=2.5,"TP (\u03BCg L\u207B\u00B9)")

tmp.dat.AGM2=merge(tmp.dat.AGM,expand.grid(alias=unique(tmp.dat$alias),WY=seq(1979,2020,1)),all.y=T)

min.val=if(min(tmp.dat.AGM$AGM)<10){0}else{10}# ceiling(min(tmp.dat.AGM$AGM,na.rm=T)-min(tmp.dat.AGM$AGM,na.rm=T)*0.1)
max.val=ceiling(max(tmp.dat.AGM$AGM,na.rm=T)+max(tmp.dat.AGM$AGM,na.rm=T)*0.10)
by.y=if(min.val==0&max.val<=10){2}else if(max.val<=15){5}else if(min.val==0&max.val<=20|max.val<=20){10}else if(min.val==0&max.val<30){20}else if(min.val==10&max.val<40){10}else{50}
ylim.val=c(min.val,max.val);ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)# ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")#by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1979,2020);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(AGM~WY,tmp.dat.AGM,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp.dat.AGM2,pt_line(WY,AGM,2,"grey",1,21,"lightgreen",cex=1.25))
mod=mblm::mblm(AGM~WY,tmp.dat.AGM)
mod.fit=predict(mod,tmp.dat.AGM,interval="confidence")
# if(min(mod.fit[,2])>0){shaded.range(tmp.dat.AGM$WY,mod.fit[,2],mod.fit[,3],"grey",lty=0)}
shaded.range(tmp.dat.AGM$WY,mod.fit[,2],mod.fit[,3],"grey",lty=0)
lines(tmp.dat.AGM$WY,mod.fit[,1],lty=2,col="black")
lines(range(tmp.dat.AGM$WY),rep(min(tmp.dat.AGM$LTmeanGM,na.rm=T),2),col=adjustcolor("indianred1",0.5),lwd=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Water Year",cex=0.8)
mtext(side=2,line=2.5,"TP AGM (\u03BCg L\u207B\u00B9)",cex=0.8)
# mtext(side=3,adj=0,paste0("Station: ",unique(tmp.dat$alias)))
legend("topleft",legend=c("Thiel-Sen \u00B1 95% CI","Average Geomean"),
       lty=c(2,1),col=c("black",adjustcolor("indianred1",0.5)),lwd=c(1,2),
       pt.cex=1,ncol=1,cex=0.75,bty="n",xpd=NA,xjust=0.5,y.intersp=1,x.intersp=0.75)


rng=signif(max(abs(range(tmp.dat.AGM$anom,na.rm=T)))+max(abs(range(tmp.dat.AGM$anom,na.rm=T)))*0.1,2)
ylim.val=c(-1*rng,rng);by.y=rng/2;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1979,2020);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

# Trend in anomaly data
mod=mblm::mblm(anom~WY,subset(tmp.dat.AGM,is.na(anom)==F),repeated = F)
mod.fit2=data.frame(WY=seq(min(tmp.dat.AGM$WY,na.rm=T),max(tmp.dat.AGM$WY,na.rm=T)))
mod.fit2=cbind(mod.fit2,predict(mod,mod.fit2,interval="confidence"))
mod.fit2=merge(mod.fit2,expand.grid(alias=unique(tmp.dat$alias),WY=seq(1979,2020,1)),"WY",all.y=T)

# Color ramp for all the (filled) data.
vec.val=seq(-1*rng,rng,1)
cols=colorRampPalette(c('lightblue', 'lightgreen','tomato'))(length(vec.val))
cols.val=cols[findInterval(tmp.dat.AGM2$anom,vec.val)]
par(lwd=0.8)
x=barplot(tmp.dat.AGM2$anom,ylim=ylim.val,ann=F,axes=F,col=NA,border=NA,space=0)
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=3,col="grey",lwd=1)
x=barplot(tmp.dat.AGM2$anom,ylim=ylim.val,ann=F,axes=F,col=cols.val,space=0,add=T)
abline(h=0,lwd=1)
lines(x,mod.fit2$fit,lty=1,col="black",lwd=2)
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"Water Year",cex=0.8)
mtext(side=2,line=2.5,"Annual TP Aomalies (\u03BCg L\u207B\u00B9)",cex=0.75)

par(mar=c(2.5,0.5,1,0.5))
tmp.site=subset(epa.sites,STATION==site.val.input)
if(subset(station.list2,alias==site.val.input)$Region!="ENP"){bbox.lims=bbox(subset(wca,name3==as.character(subset(station.list2,alias==site.val.input)$Region)))}else{bbox.lims=bbox(enp.shore)}
plot(wca,col="white",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=1)
plot(gSimplify(enp.shore,500),col="white",border="dodgerblue3",add=T,lwd=0.25)
plot(evpa,border="red",col=NA,lwd=1.75,add=T)
plot(epa.sites,pch=21,bg=adjustcolor("grey",0.5),col="grey",lwd=0.1,add=T)
plot(tmp.site,pch=21,bg="indianred1",lwd=0.1,cex=1.25,add=T)
raster::text(tmp.site,"STATION",halo=T,cex=0.75,pos=4,font=2)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4)
box(lwd=1)

plot(0:1,0:1,ann=F,axes=F,type="n")
dev.off()
print(i)
}

### 
