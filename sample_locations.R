library(raster)
library(ggmap)

landclass <- raster("/alphadata01/bstocker/data/landcover/modis_igbp.tif")
kgclass <- raster("/alphadata01/bstocker/data/koeppengeiger/koeppen-geiger.tif")

## reduce region
bb <- extent(c(108, 155, -45, -10))  # xmin, xmax, ymin, ymax
landclass <- crop( landclass, bb )
kgclass <- crop( kgclass, bb )

## resample
kgclass <- resample( kgclass, landclass, method="ngb" )

## mask out all non-grass pixels
kgclass_gra <- mask( kgclass, landclass, inverse=TRUE, maskvalue=10, updatevalue=NA )
kgclass_gra <- mask( kgclass_gra, kgclass_gra, maskvalue=32, updatevalue=NA )

## sample from grass pixels
sample <- sampleStratified( kgclass_gra, 30, na.rm=TRUE, xy=TRUE )
print(table(sample[,"koeppen.geiger"]))

## create polygons for sampled sites
cells = 1:ncell(kgclass_gra)
values = rep(NA,length(cells))
values[sample[,"cell"]] = 1
sample_raster = kgclass_gra
sample_raster[] = NA
sample_raster = setValues(sample_raster, values)
site_poly = rasterToPolygons(sample_raster, na.rm = TRUE)

# download google maps pictures for all sites
for (idx in 1:nrow(sample)){
  keeptrying <- TRUE
  while(keeptrying){
    myMap2 <- try( get_googlemap(center = c(lon=sample[[idx,"x"]], lat=sample[[idx,"y"]]), zoom =15, maptype = "satellite"))
    if (class(myMap2)[1]!="try-error"){
      keeptrying <- FALSE
    }
  }
  ggmap(myMap2) + geom_polygon(aes(long, lat), data=fortify(site_poly), col="red", fill=NA )
  ggsave( paste0("fig/picture_site_",as.character(idx),".pdf"), device = "pdf" )
}

# mymap = try(ggmap::get_map(location = c(lon = sample[[idx,"x"]],
#                                         lat = sample[[idx,"y"]]),
#                            color = "color",
#                            source = "google",
#                            maptype = "satellite",
#                            zoom = 15), silent = TRUE)
# 
# map = ggmap(mymap) + geom_polygon(data = fortify(site_poly),
#                            aes( long, lat, group=group),
#                            colour = "red",
#                            fill = NA)
# plot(map)
