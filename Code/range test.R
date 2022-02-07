library(ggplot2); library(data.table)

# Import individual detection logs and join them together
# dets <- list.files('p:/obrien/biotelemetry/mainstem backbone/detection files',
#                    pattern = '1214.*.csv',
#                    full.names = T)
# 
# dets <- lapply(dets, fread, fill = T, col.names = function(.) tolower(gsub('[) ()]', '',.)))
# 
# 
# dets <- rbindlist(dets)
# fwrite(dets, 'embargo/raw/cruise1221_dets.csv')

dets <- fread('embargo/raw/cruise1221_dets.csv')


# Match transmitter detections to their lat/long (columns are currently NA)
## Import station metadata (OTN format)
# stations <- readxl::read_excel('p:/obrien/biotelemetry/mainstem backbone/MidBay Backbone_instrument_metadata.xlsx',
#                                sheet = 2, skip = 3)
# 
# ## Rename columns
# station_key <- data.table(stations)[, .('stationname' = STATION_NO,
#                                         'transmitter' = TRANSMITTER,
#                                         'starttime' = `DEPLOY_DATE_TIME   (yyyy-mm-ddThh:mm:ss)`,
#                                         'latitude' = DEPLOY_LAT,
#                                         'longitude' = DEPLOY_LONG,
#                                         'receiver' = INS_SERIAL_NO)]
## Store locally
# fwrite(station_key, 'embargo/raw/station_key.csv')

station_key <- fread('embargo/raw/station_key.csv')

## Remove receivers with no internal transmitters
station_key <- station_key[transmitter != '']

## Convert time column to time, use full receiver serial
station_key[, ':='(starttime = as.POSIXct(starttime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'),
                   receiver = paste0('VR2AR-', receiver))]

## There are 6 receivers, repeated for each cruise
##  so just shifting the deployment time column up 6 rows to create the end time
station_key[, endtime := shift(starttime, -6)]

## Label maintenance cruises
station_key[, cruise := rep(c('202105', '202108', '202112'), each = 6)]

## Remove receivers that are currently deployed (no data yet)
station_key <- station_key[!is.na(endtime)]

## Set key (start and end time by reciever) for data.table::foverlaps
setkey(station_key, receiver, starttime, endtime)


# Join station location key and transmitter detections
## there need to be two time columns in each data.table for foverlaps
##  Creating a dummy column
dets[, dummy_end := dateandtimeutc]

## Conduct overlap join
dets <- foverlaps(dets,
                  station_key[, -'transmitter'],
                  by.x = c('receiver', 'dateandtimeutc', 'dummy_end'),
                  nomatch = 0)

## Drop unused columns
dets <- dets[, -c('transmittername', 'transmitterserial', 'sensorvalue', 'sensorunit',
                  'i.stationname', 'i.latitude', 'i.longitude', 'transmittertype',
                  'sensorprecision', 'dummy_end', 'starttime', 'endtime')]

## Join locations of transmitters that were detected
dets <- dets[station_key[, -c('receiver', 'starttime', 'endtime')], ,
    on = c('transmitter', 'cruise'), nomatch = 0]

## Rename columns for clairty
dets <- dets[, .('station_to' = stationname,
           'lat_to' = latitude,
           'lon_to' = longitude,
           'station_from' = i.stationname,
           'lat_from' = i.latitude,
           'lon_from' = i.longitude,
           'datetime' = dateandtimeutc,
           cruise)]
dets[, day := lubridate::floor_date(datetime, 'day')]


# Calculate distances between receivers
library(sf)
spatial_key <- st_as_sf(setorder(station_key, cruise, stationname),
               coords = c('longitude', 'latitude'),
               crs = 4326)

dists <- data.table(spatial_key)[, st_distance(geometry), by = 'cruise']
dists[, ':='(station_from = rep(station_key$stationname, times = 6),
                   station_to = rep(station_key$stationname, each = 6))]
setnames(dists, 'V1', 'dist')


# Calculate number of detections heard from each transmitter x receiver pair ("successes")
successes <- dets[, data.table(xtabs(~ station_to + station_from)), by = c('cruise', 'day')]


# Calculate total number transmissions for each receiver (times a receiver heard its own transmitter)
trials <- dets[, data.table(trials = diag(xtabs(~ station_to + station_from)),
                            station_from = names(diag(xtabs(~ station_to + station_from)))),
               by = c('cruise', 'day')]



# Build out model data
model_data <- successes[trials, on = c('cruise', 'day', 'station_from')]
rm(successes, trials)

model_data <- model_data[dists, on = c('cruise', 'station_from', 'station_to'), nomatch = 0]
model_data <- unique(model_data, by = c('cruise', 'station_from', 'station_to', 'day'))
setorder(model_data, day, station_from)



library(lme4)
model_data[, ':='(dist_km = as.numeric(dist)/1000, 
                  day_f = as.factor(day),
                  site_f = as.factor(station_to))]
model_data <- model_data[!day %in% as.POSIXct(c('2021-05-19', '2021-08-20', '2021-12-14'), tz = 'UTC')]

# job::job({
#   r_test <- glmer(cbind(N, trials - N) ~ dist_km +  (0+dist_km|day_f:site_f) + (1|site_f),
#                       data = model_data, family = 'binomial')
# })
# job::job({
#   r_test_no0 <- glmer(cbind(N, trials - N) ~ dist_km +  (0+dist_km|day_f:site_f) + (1|site_f),
#                    data = model_data[dist_km != 0], family = 'binomial')
# })
# 
# job::job({
# r_test2 <- glmer(cbind(N, trials - N) ~ 0 + dist_km + site_f + (0+dist_km|day_f:site_f),
#                 data = model_data, family = 'binomial')
# r_test2_no0 <- glmer(cbind(N, trials - N) ~ 0 + dist_km + site_f + (0+dist_km|day_f:site_f),
#                      data = model_data[dist_km != 0], family = 'binomial')
# })
# 
# job::job({
#   r_test3 <- glmer(cbind(N, trials - N) ~ 0 + dist_km + (0+dist_km|day_f:site_f) + (1|site_f) + (1|day_f),
#                    data = model_data, family = 'binomial')
# })
# job::job({
#   r_test3_no0 <- glmer(cbind(N, trials - N) ~ 0 + dist_km + (0+dist_km|day_f:site_f) + (1|site_f) + (1|day_f),
#                    data = model_data[dist_km != 0], family = 'binomial')
# })


# This is what I'm selecting
job::job({
  r_test4 <- glmer(cbind(N, trials - N) ~ 0 + dist_km + site_f + (0 + dist_km|day_f:site_f) + (1|day_f),
                   data = model_data, family = 'binomial')
}, import = c(model_data, data.table), packages = c('lme4'))
# job::job({
#   r_test4_no0 <- glmer(cbind(N, trials - N) ~ 0 + dist_km + site_f + (0+dist_km|day_f:site_f) + (1|day_f),
#                    data = model_data[dist_km != 0], family = 'binomial')
# })


plot(r_test4, type = c('p', 'smooth'))
#heteroscedasticity
plot(r_test4, sqrt(abs(resid(.))) ~ fitted(.), type = c('p', 'smooth'))


# d50s <- data.table(coef(r_test_no0)$`day_f:site_f`, keep.rownames = T)[, -'(Intercept)']
# d50s[, c('day', 'site') := tstrsplit(rn, ':')]
# d50s <- d50s[data.table(coef(r_test_no0)$site_f, keep.rownames = T)[, -'dist_km'],
#              on = c('site == rn')]
# d50s[, d50 := 1000 * -`(Intercept)` / dist_km]
# d50s[, day := as.Date(day)]
# 
# 
# ggplot(data = d50s) +
#   geom_line(aes(x = day, y = d50, color = site)) +
#   scale_color_viridis_d()
# 
# 
# d50s <- data.table(coef(r_test2_no0)$`day_f:site_f`, keep.rownames = T)
# d50s[, c('day', 'site') := tstrsplit(rn, ':')]
# setnames(d50s, function(.) gsub('site_f', '', .))
# d50s <- melt(d50s[, -'rn'], id.vars = c('day', 'site', 'dist_km'))
# d50s <- d50s[site == variable]
# d50s[, ':='(d50 = 1000 * (-value / dist_km),
#             day = as.Date(day))]
# 
# ggplot(data = d50s) +
#   geom_line(aes(x = day, y = d50, color = site)) +
#   scale_color_viridis_d()
# 
# 
# d50s <- data.table(coef(r_test3)$`day_f:site_f`, keep.rownames = T)
# d50s[, c('day', 'site') := tstrsplit(rn, ':')]
# d50s <- d50s[, -c('(Intercept)', 'rn')]
# d50s <- d50s[data.table(ranef(r_test3)$day_f, keep.rownames = T), on = c('day == rn')]
# d50s <- d50s[data.table(ranef(r_test3)$site_f, keep.rownames = T), on = c('site == rn')]
# d50s[, ':='(day = as.Date(day),
#             d50 = 1000 * -(`(Intercept)` + `i.(Intercept)`) / dist_km)]
# 
# ggplot(data = d50s) +
#   geom_line(aes(x = day, y = d50, color = site)) +
#   ylim(c(-100, 2000)) +
#   scale_color_viridis_d() +
#   facet_wrap(~site)

# Picking r_test4 at this point...
d50s <- data.table(coef(r_test4)$`day_f:site_f`, keep.rownames = T)
d50s[, c('day', 'site') := tstrsplit(rn, ':')]
setnames(d50s, function(.) gsub('site_f', '', .))
d50s <- melt(d50s[, -'rn'], id.vars = c('day', 'site', 'dist_km'))
d50s <- d50s[site == variable]
d50s <- d50s[data.table(ranef(r_test4)$day_f, keep.rownames = T), on = c('day == rn')]
d50s[, ':='(d50 = 1000 * (-(value + `(Intercept)`)  / dist_km),
            day = as.Date(day))]

ggplot(data = d50s) +
  lims(y = c(0, 2000), x = c(as.Date('2021-05-20'), as.Date('2021-12-13'))) +
  geom_line(aes(x = day, y = d50)) +
  labs(y = 'Distance at 50% detection probability (m)', x = NULL) +
  facet_wrap(~site) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 15))




(-fixef(r_test)[1] / fixef(r_test)[2]) * 1000
# d50s <- data.table(coef(r_test)$`day_f:site_f)[,
#                   .(cp1 = 1000 * (-`(Intercept)` / `dist_km`),
#                     cp2 = 1000 * (-(`(Intercept)` + `site_fCedar Point 2`) /
#                                     (`dist_km:site_fCedar Point 2` + dist_km)),
#                     cp3 = 1000 * (-(`(Intercept)` + `site_fCedar Point 3`) /
#                                     (`dist_km:site_fCedar Point 3` + dist_km)),
#                     cp4 = 1000 * (-(`(Intercept)` + `site_fCedar Point 4`) /
#                                     (`dist_km:site_fCedar Point 4` + dist_km)),
#                     cp5 = 1000 * (-(`(Intercept)` + `site_fCedar Point 5`) /
#                                     (`dist_km:site_fCedar Point 5` + dist_km)),
#                     cp6 = 1000 * (-(`(Intercept)` + `site_fCedar Point 6`) /
#                                     (`dist_km:site_fCedar Point 6` + dist_km)))]
# d50s <- melt(d50s, id = 'day')


# d50s <- data.table(coef(r_test)$`day_f:site_f`, keep.rownames = T)
# # [, .(d50 = 1000 * -`(Intercept)` / dist_km)]
# d50s[, c('day', 'site') := tstrsplit(rn, ':')]
# d50s <- d50s[data.table(ranef(r_test)$site_f, keep.rownames = T),
#      on = c('site == rn')]
# d50s <- d50s[, day := as.Date(day)]
# d50s[, d50 := (-(`(Intercept)` + `i.(Intercept)`) / dist_km) * 1000]



d50s <- data.table(coef(r_test)$`day_f:site_f`, keep.rownames = T)[, .(rn, dist_km)]
d50s[, c('day', 'site') := tstrsplit(rn, ':')]
d50s <- d50s[data.table(as.data.frame(fixef(r_test)), keep.rownames = T)[, site := gsub('site_f', '', rn)],
     on = 'site']
d50s[, d50 := (-`fixef(r_test2)` / dist_km) * 1000]
d50s <- d50s[, day := as.Date(day)]


library(ggplot2)
ggplot(data = d50s[day != '2021-05-19']) +
  geom_line(aes(x = day, y = d50, color = site)) +
  scale_x_date(date_breaks = '2 week') +
  geom_vline(xintercept = as.Date('2021-08-20', tz = 'utc'), color = 'red') +
  labs(y = 'D50 (m)', x = NULL) +
  scale_color_viridis_d()

d50s <- rbind(
  d50s[day <= '2021-08-20'][spatial_key[1:6, c('stationname', 'geometry')],
                            on = c(site = 'stationname')],
  d50s[day > '2021-08-20'][spatial_key[1:6, c('stationname', 'geometry')],
                           on = c(site = 'stationname')]
)

d50s[, geometry := st_transform(geometry, 32618)]
d50s[, range := st_buffer(geometry, d50)]
d50s[, range := st_transform(range, 4326)]

midatl <- st_read('data and imports/mid-atlantic/matl_states_land.shp')





library(gganimate)

ts_plot <- animate(
  ggplot(data = d50s) +
    lims(y = c(0, 2000), x = c(as.Date('2021-05-20'), as.Date('2021-12-13'))) +
    geom_line(aes(x = day, y = d50)) +
    labs(y = 'Distance at 50% detection probability (m)', x = NULL) +
    facet_wrap(~site) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          axis.title = element_text(size = 12)) +
    
    transition_reveal(day, range = as.Date(c('2021-05-20', '2021-12-13'))),
  
  nframes = 207,
  fps = 5,
  device = 'ragg_png',
  res = 72,
  width = 650,
  height = 475,
  scaling = 1.7
)


# 
# animate(
#   ggplot() +
#     geom_sf(data = midatl) +
#     geom_sf(data = d50s, aes(geometry = range),
#             fill = 'green', color = 'green3', alpha = 0.6) +
#     coord_sf(xlim = c(-76.5, -76.1), ylim = c(38.2, 38.4)) +
#     theme_minimal() +
#     
#     transition_time(day, range = as.Date(c('2021-05-20', '2021-12-13'))), 
#   # +
#     # labs(title = '{frame_time}'),
#   renderer = ffmpeg_renderer()
#   
# )


# job::job({
 # anim_save('figures and animations/d50.gif',
map <- animate(
  ggplot() +
    geom_sf(data = midatl) +
    geom_sf(data = d50s[day != '2021-05-19' & d50 > 0],
            aes(geometry = range),
            fill = 'green', color = 'green3', alpha = 0.6) +
    coord_sf(xlim = c(-76.48, -76.15), ylim = c(38.24, 38.4), expand = F) +
    theme_minimal() + 
    theme(axis.text.y = element_text(angle = 45),
          plot.margin = margin(1, 1, 1, 1),
          panel.grid.minor = element_blank()) +
    
    transition_time(day, range = as.Date(c('2021-05-20', '2021-12-13'))) +
    labs(title = '{frame_time}'),
  nframes = 207,
  fps = 5, 
  # renderer = ffmpeg_renderer(),
  device = 'ragg_png',
  res = 72,
  width = 650,
  height = 397,
  scaling = 1.7
)
# )
# })


 library(magick)
 ts_mgif <- image_read(ts_plot)
 map_mgif <- image_read(map)
 
 new_gif <- image_append(c(map_mgif[1], ts_mgif[1]), stack = T)
 for(i in 2:207){
   combined <- image_append(c(map_mgif[i], ts_mgif[i]), stack = T)
   new_gif <- c(new_gif, combined)
 }
 
 image_write(new_gif, 'figures and animations/d50_3.gif')
 