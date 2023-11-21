### Get Landcover data from ESA
library(geodata)
library(leaflet)

### Download geodata for visualizing
 geodata::landcover(var = "built", path = "./raw_data/")
# can <- osm("CAN", "places", "./output/")
# plot(can)
class(urb_chkls)
###american census geography, check tidycensus package and 'urban areas' in get_acs()

### check to see if the files downloaded into the correct directory
list.files("./raw_data/", pattern = ".tif") #RASTER FILE IN .tif format

### Import raster into R
lc_b <- rast("./raw_data/WorldCover_built_30s.tif")
lc_b
#### quick plot of urban areas
plot(lc_b)

### Plot interactively with leaflet
leaflet() %>% addTiles() %>% 
  addRasterImage(lc_b)
