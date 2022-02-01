library(ggplot2); library(sf); library(data.table)


midatl <- st_read('data and imports/mid-atlantic/matl_states_land.shp')
bathy <- st_read('data and imports/chesapeake_bay_bathymetry',
                 query = 'select CONTOUR from "Maryland_Bathymetry_-_Chesapeake_Bay_Contours" where CONTOUR IN (-5,-10,-15,-20,-25,-30,-35,-40,-45,-50)')
bathy <- bathy %>% 
  st_transform(4326)

stations <- readxl::read_excel('p:/obrien/biotelemetry/mainstem backbone/MidBay Backbone_instrument_metadata.xlsx',
                               sheet = 2, skip = 3)
setnames(setDT(stations), function(.) tolower(gsub(' .*', '', .)))

stations <- unique(stations, by = 'station_no')

stations <- stations %>% 
  st_as_sf(coords = c('deploy_long', 'deploy_lat'),
           crs = 4326)


ggplot() +
  geom_sf(data = bathy, color = 'lightgray') +
  geom_sf(data = midatl) +
  geom_sf(data = stations, size = 5) + 
  coord_sf(xlim = c(-76.5, -76.15), ylim = c(38.25, 38.45)) +
  theme_minimal() +
  theme(plot.margin = margin(0, 2, 0, 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 60))


ggplot() +
  # geom_sf(data = bathy, color = 'lightgray') +
  geom_sf(data = midatl) +
  # geom_sf(data = stations, size = 5) + 
  coord_sf(xlim = c(-77.4, -75), ylim = c(36.9, 39.7)) +
  theme_minimal() +
  theme(plot.margin = margin(0, 2, 0, 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 60))
