# Load Libraries  ----
library(ggplot2)
library(ggmap)
library("ggsci")   # special color tables
library("cowplot")
library(grid)
library(OpenStreetMap)


COL_EXP=pal_uchicago("default")(7)
cols <- c("Yearly" = COL_EXP[2], "Decadal" = COL_EXP[3],"Centennial" = COL_EXP[4])


# Read Data Sets  ----


## https://www.openstreetmap.org/relation/46182#map=14/45.81265/10.07502
## https://maps.stamen.com/terrain/#15/45.8101/10.0665

pianico_map = get_stadiamap(
  bbox = c(left = 45.8, right = 47.0, top = 11, bottom = 10),
  zoom = 14, 
  maptype = "stamen_terrain") 
