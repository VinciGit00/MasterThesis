library(sf)

# create a square polygon
square <- st_polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0))))
square <- st_sfc(square)

# create a polygon of interest
poly <- st_polygon(list(rbind(c(0.5, 0.5), c(0.5, 1), c(1, 1), c(1, 0.5), c(0.5, 0.5))))
poly <- st_sfc(poly)

# intersect the square with the polygon
intersection <- st_intersection(square, poly)

intersection

# calculate the area of the intersection polygon
area <- st_area(intersection)

# print the area
area