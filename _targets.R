# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c('dplyr', 'data.table', 'readxl', 'ggplot2', 'ragg', 'lubridate', 'gt', 
              'sf', 'geoarrow', 'arrow'), 
  # default storage format
  format = "feather" 
  # Set other options as needed.
)

# tar_make_future() configuration:
plan(multisession,
     workers = availableCores(logical = F))


# Load the R scripts with your custom functions:
for (file in list.files("code/targets", full.names = TRUE)) source(file)
rm(file)

# Replace the target list below with your own:
list(
  # Follow/import detection CSVs
  tar_files_input(csv_dirs,
                  list.dirs('p:/obrien/biotelemetry/mainstem backbone/detection files',
                            full.names = T)),
  tar_target(csv_data,
             csv_read_in(csv_dirs),
             pattern = map(csv_dirs)
  ),
  
  
  # Follow/import receiver locations
  tar_file(receiver_deployment,
             'embargo/act/midbay backbone_instrument_metadata.xlsx'),
  tar_target(station_key, make_station_key(receiver_deployment)),
  
  
  # Select detections that occurred when receiver was deployed, add station
  #   location, and code whether logged detection was a self-sync
  tar_target(detections, clean_detections(csv_data, station_key)),
  
  
  
  tar_file(act_dropbox, 'embargo/act/active transmitters.xlsx'),
  tar_target(act_clean, act_fix(act_dropbox)),
  
  tar_target(act_matched, match_act(detections, act_clean)),
  
  tar_files_input(matos_matched,
                  list.files('embargo/act', full.names = T,
                             pattern = 'qualified.*\\.csv')),


  # Spatial objects
  tar_file(natural_earth, 'data and imports/ne_10m_coastline.parquet'),
  tar_file(md_bathy, 'data and imports/maryland_bathymetry.parquet'),
  tar_file(midatl, 'data and imports/matl_states.parquet'),
  
  # Maps
  tar_qs(map_bathy, create_map_bathy(md_bathy, midatl, station_key)),
  tar_qs(ches_map, create_ches_map(midatl, station_key)),
  
  # ACT/MATOS push summaries
  #   Last was on 2022-04-15; 2022-07-13
  
  tar_target(act_push_dates, data.frame(previous = as.Date('2022-04-15'),
                                        current = as.Date('2022-07-13'))),
  tar_quarto(act_push_summary, 'reports/ACT-push-summary.qmd')
  
  # 
  # tar_target(matched_unmatched, tag_matching(detections, act, tag_lists),
  #            format = 'qs'),
  # 
  # tar_target(matched_detections, matched_unmatched$matched),
  # tar_target(unmatched_detections, matched_unmatched$unmatched),
  

)
