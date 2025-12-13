
library(httr)
library(lubridate)


lat <- 43.03
lon <- -78.95
date <- Sys.Date() - 5
zoom <- 8  # use 8 or 9 for land-level detail

lat_rad <- lat * pi / 180
n <- 2^zoom
xtile <- floor((lon + 180) / 360 * n)
ytile <- floor((1 - log(tan(lat_rad) + 1 / cos(lat_rad)) / pi) / 2 * n)

url <- sprintf("https://gibs.earthdata.nasa.gov/wmts/epsg4326/best/MODIS_Terra_CorrectedReflectance_TrueColor/default/%s/250m/%d/%d/%d.jpg",
               date, zoom, ytile, xtile)

r <- GET(url)
writeBin(content(r, "raw"), "grand_island_satellite_zoom8.jpg")
