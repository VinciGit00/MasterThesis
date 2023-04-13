library(ncdf4)
# library(ecmwfr)

# PREMI control + shift + o PER VEDERE LA STRUTTURA

# A - Aggiustare precipitation ====
# A1 _ Estrarre i valori dal netcdf ####
oldwd<-getwd()
setwd("we/era5land")
nc <- nc_open("ERA5Land Lombardia 2021.nc")
lat <- ncvar_get(nc,"latitude")
long <- ncvar_get(nc,"longitude")
t <-ncvar_get(nc,"time")
nvar <- nc$nvars
varname <- names(nc$var)
matr<-matrix(nrow = length(t)*length(long)*length(lat),
               ncol = 2)
matr[,1]<-rep(rep(long,length(lat)),length(t))
matr[,2]<-rep(rep(lat,each=length(long)),length(t))
colnames(matr)<-c("Lon","Lat")
df<-data.frame(matr)
matr<-matrix(nrow = length(t)*length(long)*length(lat),
               ncol = nvar)
start<-paste0("2021","-01-01 00:00:00")
end<-paste0("2021","-12-31 23:00:00" )
daytime<-seq.POSIXt(as.POSIXct(start,tz="UTC"),
                      as.POSIXct(end,tz="UTC"),
                      by="hour",tz="UTC")
df$time<-rep(daytime,each=length(long)*length(lat))
for (k in 1:nvar) {
    var<-ncvar_get(nc,varname[k])
    matr[,k]<-c(var)}
colnames(matr)<-c(varname)
df<-cbind(df,as.data.frame(matr))
prec <- cbind(df[,c(1,2,3,11)])
rm(list = ls())

# A2 _ Da orari a giornalieri ####
load("prec_hourly.Rdata")
head(prec)
prec<-prec[order(prec$Lat,prec$Lon,prec$time),]
v<-seq(0,nrow(prec),24)
v<-v[-1]
prec <- prec[v,]
prec$time<-as.Date(prec$time)
save(prec,file="prec_daily.Rdata")
rm(list = all())
#A3 _ Sostituire ai dati sbagliati
load("Daily Land 2021 Lombardy.Rdata")
df_daily <- df_daily[order(df_daily$days,df_daily$Lon,df_daily$Lat),]
prec <- prec[order(prec$time,prec$Lon,prec$Lat),]
df_daily$sum_total_precipitation<-prec$tp
save(df_daily,file="era5land_fixed_ready_for_IDW.Rdata")
setwd(oldwd)
#check results ####
df_daily[which.max(df_daily$sum_total_precipitation),]
#4 ottobre 2021 a Lon 8.35 e lat 45.95 -> ha fatto tempesta
# https://www.ilmeteo.it/portale/archivio-meteo/Omegna/2021/Ottobre/4

