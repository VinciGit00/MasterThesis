#Libraries
library(rgdal)
library(sf)
library(dplyr)
library(sp)
library(raster)

#Reading the files
##Shapefile
shapefile_path <- "/Users/marcovinciguerra/Desktop/bdn/Com01012021_g_WGS84.shp"

my_shapefile <- st_read(dsn = shapefile_path)
my_shapefile$field_1 <- NULL

##CSV
read_csv <- read.csv('/Users/marcovinciguerra/Desktop/bdn/Agrimonia_Dataset_v_2_0_1.csv')

cord <- unique(read_csv[, 1:3])

coordinates(cord) <- c("Longitude", "Latitude")

cord_sf <- st_as_sf(cord)

##Select the interested columns 
subset <- read_csv[c("IDStations", "Longitude", "Latitude", "Time", "LI_pigs", "LI_bovine")]
colnames(subset)[2] = "Lon"
colnames(subset)[3] = "Lat"

#Round the columns
my_shapefile <- my_shapefile %>%
  mutate(across(1:2, round, 2))

subset <- subset %>%
  mutate(across(2:3, round, 2))


#Join the dataframe 
#join <- merge(x = my_shapefile, y = subset, by = c("Lat", "Lon"))

st_buffer <- st_buffer(cord_sf, nQuadSegs = 4,  endCapStyle = 'SQUARE', dist = 0.05)
st_crs(st_buffer) <- 4326

my_shapefile <- st_transform(my_shapefile, crs = st_crs(st_buffer))

my_shapefile <- st_make_valid(my_shapefile)
#temp <- subset(join, select = c("Lon", "Lat", "Time", "LI_pigs", "LI_bovine", "geometry", "buffer.geometry"))

#join <- merge(x= join, y=st_buffer, by="IDStations")


# Get the unique longitude and latitude values
unique_locs <- unique(join[, c("Lon", "Lat")])

#Calcolo di tutte le intersezioni che rimane fisso
#Loop every single square
for (i in 1:nrow(st_buffer)) {
  # Get the values in the current row
  row_values <- st_buffer[i, ]
  
  area_buffer <- st_area(row_values)/1000000
  
  #Calculate the intersection
  intersection <- st_intersection(row_values, my_shapefile)
  
  area <- st_area(intersection)/1000000
  
  weigth <- area/area_buffer
  
  df <- data.frame(cbind(intersection[,c(1,7)]))
  
  df$weigth <- round(weigth, 5 )
  
  df <- df[, -3]
  
  if(i==1) {
    all_df <- df
  } else {
    all_df<-rbind(all_df,df)
  }
  
}

weights_bdn <-all_df
names(weights_bdn)[3] <- "weight"
write.csv(weights_bdn, file="weights_bdn.csv", row.names = FALSE)
save(weights_bdn, file="weights_bdn.Rdata" )