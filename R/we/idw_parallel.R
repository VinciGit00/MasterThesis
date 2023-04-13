library(doParallel)
registerDoParallel()
load("idw_era5land.RData")
df1<-df1[,-c(17:22)]
df12 <- foreach (day = unique(df1$days), .combine = rbind) %dopar% {
  df12 <- foreach (id1 = unique(df1$id), .combine = rbind) %dopar% {
    final_value<-c()
    for (nvar in 1:ncol(df2)) {
      "%notin%"<-Negate("%in%")
      if (nvar %notin% no_col){
        if (nvar %in% num_var){value<-c()
        for (kn in 1:knn) {
          value[kn] <- k_list[[id1]]$dp[kn]*df2[df2$days==day & df2$id == k_list[[id1]]$k[kn],nvar]
        }
        final_value[(nvar-3)] <- sum(value)
        }
        if (nvar %in% cat_var){value<-c()
        value <- getmode(df2[df2$days==day&df2$id %in% k_list[[id1]]$k,nvar])
        final_value[(nvar-3)] <- levels(value)[value]}
      }
    }
    df12 <- cbind(df1[df1$days==day&df1$id==id1,],t(as.data.frame(final_value)))
  }
}
save(df12,file="AGC_ERA5_2021.Rdata")