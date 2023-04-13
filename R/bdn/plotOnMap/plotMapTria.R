library(rgdal)
library(sp)

# Create a multipolygon object
mp <- readWKT2("MULTIPOLYGON (((0 0, 0 1, 1 1, 1 0, 0 0)), ((2 2, 2 3, 3 3, 3 2, 2 2)))")

# Plot the multipolygon
plot(mp)
