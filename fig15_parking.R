library(plyr) # ddply
library(readr)  # read_csv
library(leaflet)
library(KernSmooth) #bkde2D
library(sp) #polygons
library(rgdal) #readOGR
library(rgeos) #gcontains
library(lubridate) #gcontains

rm(list =ls()) # clear history

source("obu_functions.R") # common OBU functions, run before setwd'ing somewhere

dir_shape<-c("/home/sheida/Documents/obu/shapefile/",
             "/home/tias/Dropbox-vub/OBU")

dir_data<-c("/home/sheida/Documents/obu/p2016/",
            "/home/tias/people/sheida/OBU/data/oct2016/Summaries/")


file_ids<-c(20161001:20161008,20161010:20161025)
summary_names<-paste0("summary_",file_ids) #adjust file name accordingly

# 1 get day id and truck ids to be removed
# condition at this moment is only obs_dist_driven < 1km
removable_ids <- get_removable_ids(dir_data, summary_names)
removable_ids_idSwitch <- get_ids_idSwitch(dir_data, summary_names)


# 2 bxl shape file
setwd_exists(dir_shape)
bxl <- readOGR(dsn = "source", layer = "bxl_region_handcrafted")


# 3 get lon/lat of each entry (except excludes)
# -- no condition on distance from previous point... (jumps) because separate file
setwd_exists(paste0(dir_data))



long_park <- ldply(file_ids, function(fid) {
  parked<-read_csv(paste('bxl_parked_',fid,sep=""),
                   col_types = cols_only( id = col_double(),  X = col_double(), Y = col_double())) # faster: only read needed cols
  remove_ids<-removable_ids[removable_ids$fid==paste0("summary_",fid),"ids"]
  removable_ids_idSwitch<-removable_ids_idSwitch[removable_ids_idSwitch$fid==fid,"ids"]
  parked <- parked[!(parked$id %in% remove_ids),]
  parked <- parked[!(parked$id %in% removable_ids_idSwitch),]
  data.frame(lon=parked$X,lat=parked$Y)
})


spdf <- SpatialPointsDataFrame(data=long_park, coords=long_park[,c("lon","lat")],proj4string=CRS(proj4string(bxl)))

# 4 make the contour lines
kde <- bkde2D(spdf@coords, bandwidth=c(.001, .001), gridsize = c(1000,1000))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat, nlevels=10)
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons, proj4string=CRS(proj4string(bxl)))
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))
p<-leaflet(spgons) %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])

# 5 get levels of the CL, make polygon data frame
levels <- sapply(CL, `[[`, "level")
polydf <- SpatialPolygonsDataFrame(spgons, data.frame(id=1:length(levels),level=levels))
# add centroids
centr <- gCentroid(polydf, byid=T)
polydf$center_X <- centr@coords[,1]
polydf$center_Y <- centr@coords[,2]
# only outer polygons
polydf <- polydf[polydf$level == min(polydf$level),]
# show numbered
leaflet(polydf) %>% addProviderTiles(providers$CartoDB) %>% addPolygons() %>%
  addLabelOnlyMarkers(polydf$center_X, polydf$center_Y, label=paste(polydf$id), labelOptions=labelOptions(noHide=T))

# 6 do counting of all polygons (incl overlapping ones)
polydf$count <- colSums(gContains(polydf, spdf, byid = TRUE))
polydf$perc <- polydf$count / sum(polydf$count)
labels <- paste0(round(1000*polydf$perc)/10,"%")
# try colors with: previewColors(colorBin(heat.colors(2), domain = NULL, bins = 11), sort(polydf$perc))
colors <- colorBin(heat.colors(2), domain = NULL, bins = 11)
# pretty print percentages on a gray-ish map
leaflet(polydf) %>% addProviderTiles(providers$CartoDB) %>%
  addPolygons(color="black",fillColor = colors(polydf$perc),fillOpacity = 0.5) %>%
  addLabelOnlyMarkers(polydf$center_X, polydf$center_Y, label=labels, labelOptions=labelOptions(noHide=T))

# Show only top 10 entrie points
polydf$perc <- colSums(gContains(polydf, spdf, byid = TRUE))
polydf<-polydf[order(polydf$count,decreasing = TRUE),]
top10_polydf<-polydf[(polydf$count> polydf$count[11]),]
top10_polydf$id<-c(1:10)
addPolygons(map = p, data = top10_polydf) %>%
  addPolygons(color="black",fillColor = colors(polydf$perc),fillOpacity = 0.5) %>%
  addLabelOnlyMarkers(top10_polydf$center_X, top10_polydf$center_Y, label=paste(top10_polydf$id), labelOptions=labelOptions(noHide=T, style = list("font-size" = "28px")))
