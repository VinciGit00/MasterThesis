#setwd("we")
load("era5land/era5land_fixed_ready_for_IDW.rdata")
Land <- df_daily
load("era5singlelevel/Daily Single Levels 2021 Lombardy.rdata")
SingleLevels <- df_daily
rm(df_daily)
Land <- Land[order(Land[,3],Land[,2],Land[,1]),]
SingleLevels <- SingleLevels[order(SingleLevels[,3],SingleLevels[,2],SingleLevels[,1]),]

coord_land <-unique(Land[,c(1,2)])
coord_sl <-unique(SingleLevels[,c(1,2)])

df1 <- Land
df2 <- SingleLevels

c_lon <- 1 # indicate the column of longitude
c_lat <- 2 # indicate the column of latitude
knn <- 4 # indicate the number of neighbours

# assign a unique value for each localisations
df1$id <- rep(1:nrow(unique(df1[,c(c_lon,c_lat)])),length(unique(df1$days)))
df2$id <- rep(1:nrow(unique(df2[,c(c_lon,c_lat)])),length(unique(df2$days)))

# 
df1_s <- cbind(coord_land,unique(df1$id))
library(sp)
coordinates(df1_s)<-c("Lon","Lat")

#write csv for other files
df_df1_s <- as.data.frame(df1_s)
df_df1_s$Lon<-round(df_df1_s$Lon,2)
df_df1_s$Lat<-round(df_df1_s$Lat,2)

write.csv(df_df1_s,file = "Grid_ERA5Land.csv",row.names = F)
###

df2_s <- cbind(coord_sl,unique(df2$id))
coordinates(df2_s)<-c("Lon","Lat")

#check
plot(df2_s,col="blue",)
text(df2_s,pos=4,col="blue",cex=0.5)
plot(df1_s,add=T,col="red")
text(df1_s,pos=1,col="red",cex=0.5)
#ok

dist <- spDists(df1_s,df2_s)
k_list <- list()
for (i in 1:nrow(dist)) {
  k <- order(dist[i,],decreasing = F)[1:knn]
  d <- dist[i,k]
  if(min(d)==0){
    k <- rep(k[1],4)
    d <- c(1,1,1,1)}
  d <- 1/d
  D <- sum(d)
  dp <- d/D
  k_list[[i]]<-cbind(as.data.frame(k),as.data.frame(dp))
}

# columns we don't need to repeat
no_col<-which(colnames(df2) %in% c("Lon","Lat","days","id"))

#numerical variables
"%notin%"<-Negate("%in%")
num_var <- which(lapply(df2, class)=="numeric")
cat_var <- which(lapply(df2, class)=="factor")

# prepare df1 for enetring values
df1<-cbind(df1,as.data.frame(
  matrix(NA,nrow=nrow(df1),ncol=ncol(df2[,-no_col]),
         dimnames = list(NULL,names(df2)[-no_col]))))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

save.image("idw_era5land.RData")

# -- VVVVVV ---
source("idw_parallel.r") # <<---- run on HPC
# -- /\/\/\ ---
