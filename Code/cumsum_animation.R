library(data.table)

act <- readxl::read_excel('c:/users/darpa2/downloads/Active transmitters_20210810.xlsx')

dets <- list.files('p:/obrien/biotelemetry/mainstem backbone/detection files',
                   pattern = '1214.*.csv',
                   full.names = T)



dets <- lapply(dets, fread, fill = T, col.names = function(.) tolower(gsub('[) ()]', '',.)))


dets <- rbindlist(dets)



#General summaries
act_match <- dets[act, on = c(transmitter = 'Tag ID Code Standard'), nomatch = 0]
act_match[, commonname := tolower(`Common Name`)]
act_match[, date := as.Date(dateandtimeutc)]


setorder(act_match, dateandtimeutc)

n_det <- act_match[, .N, by = .(commonname, date, transmitter)][, csum := cumsum(N), by = commonname]
n_indiv <- unique(act_match, by = 'transmitter')[, .N, by = .(commonname, date)][, csum := cumsum(N), by = commonname]


library(ggplot2); library(gganimate)
det_plot <- animate(
  ggplot(data = n_det) +
    geom_step(aes(x = date, y = csum), direction = 'vh') +
    labs(y = '# Detections', x = NULL) +
    facet_wrap(~commonname, scales = 'free_y', ncol = 1) +
    theme_minimal() +
    theme(plot.margin = margin(0, 0, 0, 0)) +
    
    transition_reveal(date, range = as.Date(c('2021-05-20', '2021-12-13'))),
  nframes = 208,
  fps = 5,
  res = 72,
  width = 315,
  height = 630
)


indiv_plot <- animate(
  ggplot(data = n_indiv) +
    geom_step(aes(x = date, y = csum), direction = 'vh') +
    labs(y = '# Individuals', x = NULL) +
    facet_wrap(~commonname, scales = 'free_y', ncol = 1) +
    expand_limits(y = 0) +
    theme_minimal() +
    theme(plot.margin = margin(0,0,0,0)) +
    
    transition_reveal(date, range = as.Date(c('2021-05-20', '2021-12-13'))),
  nframes = 208,
  fps = 5,
  res = 72,
  width = 315,
  height = 630
)


library(sf)
midatl <- st_read('data and imports/mid-atlantic/matl_states_land.shp')
bathy <- st_read('data and imports/chesapeake_bay_bathymetry',
                 query = 'select CONTOUR from "Maryland_Bathymetry_-_Chesapeake_Bay_Contours" where CONTOUR IN (-5,-10,-15,-20,-25,-30,-35,-40,-45,-50)')
bathy <- bathy %>% 
  st_transform(4326)

stations <- readxl::read_excel('p:/obrien/biotelemetry/mainstem backbone/MidBay Backbone_instrument_metadata.xlsx',
                               sheet = 2, skip = 3)
setnames(setDT(stations), function(.) tolower(gsub(' .*', '', .)))

stations <- unique(stations, by = c('station_no', 'deploy_date_time'))

stations[, ':='(starttime = as.POSIXct(deploy_date_time, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'),
                endtime = as.POSIXct(recover_date_time, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'),
                receiver = paste(ins_model_no, ins_serial_no, sep = '-'))]
stations <- stations[!is.na(endtime),
                     .(station_no, starttime, endtime, deploy_lat, deploy_long, receiver)]

setkey(stations, receiver, starttime, endtime)

map_det <- act_match[, .(dateandtimeutc, receiver, transmitter, commonname, date)]
map_det[, fakeend := dateandtimeutc]

map_det <- foverlaps(map_det, stations[!is.na(endtime)], by.x = c('receiver', 'dateandtimeutc', 'fakeend'),
               nomatch = 0)

map_det[, fakeend := NULL]

map_det <- map_det[, .N, by = .(receiver, station_no, date, commonname, deploy_long, deploy_lat)]

map_det <- st_as_sf(map_det,
              coords = c('deploy_long', 'deploy_lat'),
              crs = 4326)

map <- animate(
  ggplot() +
    geom_sf(data = bathy, color = 'lightgray') +
    geom_sf(data = midatl) +
    geom_sf(data = map_det, aes(size = N, group = station_no)) +
    scale_size_continuous(range = c(5, 20),
                          breaks = round(quantile(
                            map_det$N,
                            c(0, 0.25, .50, .75, .95),
                            names = F))) +
    coord_sf(xlim = c(-76.5, -76.15), ylim = c(38.25, 38.45)) +
    labs(size = 'Detections') +
    theme_minimal() +
    theme(legend.position = c(0.05, 0.2)) +
    theme(plot.margin = margin(0, 2, 0, 0),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12, angle = 60)) +
    
    transition_time(date, range = as.Date(c('2021-05-20', '2021-12-13')))+
    labs(title = '{frame_time}'),
  nframes = 208,
  fps = 5,
  res = 72,
  width = 880,
  height = 630
)


library(magick)
map_mgif <- image_read(map)
det_mgif <- image_read(det_plot)
indiv_mgif <- image_read(indiv_plot)

new_gif <- image_append(c(map_mgif[1], det_mgif[1], indiv_mgif[1]))
for(i in 2:208){
  combined <- image_append(c(map_mgif[i], det_mgif[i],
                             # Can't get indiv_plot to match the correct numer of frames
                             # Repeating last frame as a workaround
                             indiv_mgif[ifelse(i > 205, 205, i)]))
  new_gif <- c(new_gif, combined)
}

image_write(new_gif, 'test.gif')
