library(sf); library(dplyr)

bathy_main <- st_read('data and imports/chesapeake_bay_bathymetry')

bathy <- bathy_main %>% 
  st_transform(4326) %>% 
  st_crop(c(xmin = -76.5, xmax = -76.15, ymin = 38.25, ymax = 38.45))


stations <- readxl::read_excel('p:/obrien/biotelemetry/mainstem backbone/MidBay Backbone_instrument_metadata.xlsx',
                               sheet = 2, skip = 3)  %>% 
  distinct(STATION_NO, .keep_all = T) %>% 
  st_as_sf(coords = c('DEPLOY_LONG', 'DEPLOY_LAT'),
           remove = F,
           crs = 4326)



plot(st_geometry(filter(bathy, CONTOUR == 0)))


shore_pts <- st_nearest_points(filter(stations, grepl('[16]', STATION_NO)),
                       st_combine(filter(bathy, CONTOUR == 0))) %>% 
  st_cast('POINT') %>% 
  .[c(2,4)] %>% 
  st_as_text()


station_line <- stations %>% 
  st_combine() %>% 
  st_cast('POINT') %>% 
  st_as_text() %>% 
  c(shore_pts[1], ., shore_pts[2]) %>% 
  st_as_sfc(crs = 4326) %>% 
  st_combine() %>% 
  st_cast('LINESTRING')



k <- st_intersection(bathy, station_line)

k <- k %>% 
  filter(st_geometry_type(.) == 'MULTIPOINT') %>% 
  st_cast('POINT') %>% 
  rbind(k %>% 
          filter(st_geometry_type(.) == 'POINT'))

k <- k %>% 
  st_transform(3857) %>% 
  mutate(X = st_coordinates(.)[, 1],
         Y = st_coordinates(.)[, 2]) %>% 
  bind_rows(shore_pts[2] %>% 
              st_as_sfc(crs = 4326) %>% 
              st_as_sf() %>% 
              st_transform(3857) %>% 
              mutate(CONTOUR = 0,
                     X = st_coordinates(.)[, 1],
                     Y = st_coordinates(.)[, 2])) %>% 
  mutate(X_scale = abs(X-min(X))/1000) %>% 
  arrange(X) 

stations_km <- stations %>% 
  st_transform(3857) %>% 
  mutate(X = st_coordinates(.)[,1],
         X_scale = abs(X - min(k$X)) / 1000)

library(ggplot2)
ggplot() +
  geom_polygon(data = k, aes(x = X_scale, y = CONTOUR), fill = 'white',) + 
# geom_point(data = k, aes(x = X, y = CONTOUR)) +
  geom_vline(data = stations_km,
             aes(xintercept =  X_scale)) +
  labs(x = 'Kilometers from W Shore', y = 'Depth')+
  scale_x_continuous(limits = range(k$X_scale), expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'burlywood4'),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

st_intersects(st_buffer(stations, 1), k)


j <- data.frame(X = seq(min(k$X), max(k$X), by = 0.00001))

k <- k %>% 
  arrange(X) %>% 
  mutate(X = round(X, 5))

j <- left_join(j, k)
