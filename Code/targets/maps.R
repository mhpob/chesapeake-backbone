create_map_bathy <- function(md_bathy, midatl, station_key){
  # library(ggplot2); library(sf); library(geoarrow);
  # library(arrow); library(dplyr); library(targets)
  # tar_load(midatl); tar_load(md_bathy); tar_load(station_key)
  
  midatl <- read_geoparquet_sf(midatl)
  bathy <- md_bathy |> 
    open_dataset() |> 
    filter(CONTOUR %in% seq(-50, -5, 5)) |> 
    geoarrow_collect_sf() |>  
    st_transform(4326)
  
  
  stations <- station_key |> 
    unique(by = 'stationname') |>  
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
}



create_ches_map <- function(midatl, station_key){
  # library(ggplot2); library(sf); library(geoarrow);
  # library(arrow); library(dplyr); library(targets)
  # tar_load(midatl); tar_load(md_bathy); tar_load(station_key)
  
  midatl <- read_geoparquet_sf(midatl)
  
  stations <- station_key |> 
    unique(by = 'stationname') |> 
    st_as_sf(coords = c('deploy_long', 'deploy_lat'),
             crs = 4326)
  
  
  ggplot() +
    geom_sf(data = midatl) +
    annotate('rect', xmin = -76.5, xmax = -76.15, ymin = 38.25, ymax = 38.45,
             fill = NA, color = 'black') +
    coord_sf(xlim = c(-77.4, -75), ylim = c(36.9, 39.7)) +
    theme_minimal() +
    theme(plot.margin = margin(0, 2, 0, 0),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12, angle = 60))
}
  
