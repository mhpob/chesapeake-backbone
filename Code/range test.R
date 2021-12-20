library(data.table)

dets <- list.files('p:/obrien/biotelemetry/mainstem backbone/detection files',
                   pattern = '1214.*.csv',
                   full.names = T)

dets <- lapply(dets, fread, fill = T, col.names = function(.) tolower(gsub('[) ()]', '',.)))


dets <- rbindlist(dets)




stations <- readxl::read_excel('p:/obrien/biotelemetry/mainstem backbone/MidBay Backbone_instrument_metadata.xlsx',
                               sheet = 2, skip = 3)


station_key <- data.table(stations)[, .('stationname' = STATION_NO,
                                        'transmitter' = TRANSMITTER,
                                        'starttime' = `DEPLOY_DATE_TIME   (yyyy-mm-ddThh:mm:ss)`,
                                        'latitude' = DEPLOY_LAT,
                                        'longitude' = DEPLOY_LONG,
                                        'receiver' = INS_SERIAL_NO)]
station_key <- station_key[!is.na(transmitter)]

station_key[, ':='(starttime = as.POSIXct(starttime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'),
                   receiver = paste0('VR2AR-', receiver))]


station_key[, endtime := shift(starttime, -6)]
station_key[, cruise := rep(c('202105', '202108', '202112'), each = 6)]

setkey(station_key, receiver, starttime, endtime)



dets[, dummy_end := dateandtimeutc]

dets <- foverlaps(dets,
                  station_key[!is.na(endtime), -'transmitter'],
                  by.x = c('receiver', 'dateandtimeutc', 'dummy_end'),
                  nomatch = 0)

dets <- dets[, -c('transmittername', 'transmitterserial', 'sensorvalue', 'sensorunit',
                  'i.stationname', 'i.latitude', 'i.longitude', 'transmittertype',
                  'sensorprecision', 'dummy_end', 'starttime', 'endtime')]

dets <- dets[station_key[!is.na(endtime), -c('receiver', 'starttime', 'endtime')], ,
    on = c('transmitter', 'cruise'), nomatch = 0]

dets <- dets[, .('station_to' = stationname,
           'lat_to' = latitude,
           'lon_to' = longitude,
           'station_from' = i.stationname,
           'lat_from' = i.latitude,
           'lon_from' = i.longitude,
           'datetime' = dateandtimeutc,
           cruise)]
dets[, day := lubridate::floor_date(datetime, 'day')]


library(sf)
spatial_key <- st_as_sf(setorder(station_key, cruise, stationname),
               coords = c('longitude', 'latitude'),
               crs = 4326)

dists <- data.table(spatial_key)[, st_distance(geometry), by = 'cruise']
dists[, ':='(station_from = rep(station_key$stationname, times = 6),
                   station_to = rep(station_key$stationname, each = 6))]
setnames(dists, 'V1', 'dist')


successes <- dets[, data.table(xtabs(~ station_to + station_from)), by = c('cruise', 'day')]
trials <- dets[, data.table(trials = diag(xtabs(~ station_to + station_from)),
                            station_from = names(diag(xtabs(~ station_to + station_from)))),
               by = c('cruise', 'day')]


model_data <- successes[trials, on = c('cruise', 'day', 'station_from')]
rm(successes, trials)

model_data <- model_data[dists, on = c('cruise', 'station_from', 'station_to'), nomatch = 0]
setorder(model_data, day, station_from)



library(lme4)
model_data[, ':='(dist_km = as.numeric(dist)/1000, 
                  day_f = as.factor(day),
                  site_f = as.factor(station_to))]


r_test <- glmer(cbind(N, trials) ~ dist_km + (dist_km||day_f:site_f),
                data = model_data[dist_km != 0], family = 'binomial')

plot(r_test, type = c('p', 'smooth'))
#heteroscedasticity
plot(r_test, sqrt(abs(resid(.))) ~ fitted(.), type = c('p', 'smooth'))



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


d50s <- data.table(coef(r_test)$`day_f:site_f`)[, .(d50 = 1000 * -`(Intercept)` / dist_km)]
d50s[, c('day', 'site') := tstrsplit(row.names(coef(r_test)$`day_f:site_f`), ':')]
d50s[, day := as.Date(day)]



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

animate(
  ggplot() +
    geom_sf(data = midatl) +
    geom_sf(data = d50s, aes(geometry = range),
            fill = 'green', color = 'green3', alpha = 0.6) +
    coord_sf(xlim = c(-76.5, -76.1), ylim = c(38.2, 38.4)) +
    theme_minimal() +
    
    transition_time(day, range = as.Date(c('2021-05-20', '2021-12-13'))), 
  # +
    # labs(title = '{frame_time}'),
  renderer = ffmpeg_renderer()
  
)


# job::job({
#  anim_save('test.mp4',
    animate(
      ggplot() +
        geom_sf(data = midatl) +
        geom_sf(data = d50s[day != '2021-05-19' & d50 > 0],
                aes(geometry = range),
                fill = 'green', color = 'green3', alpha = 0.6) +
        coord_sf(xlim = c(-76.5, -76.1), ylim = c(38.2, 38.4)) +
        theme_minimal() + 
        
        transition_time(day) +
        labs(title = '{frame_time}'),
      nframes = 209,
      fps = 5, 
      renderer = ffmpeg_renderer(),
      device = 'ragg_png',
      res = 72,
      width = 997,
      height = 630,
      scaling = 1.7
    )
#  )
# })
