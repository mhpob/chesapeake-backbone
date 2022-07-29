match_act <- function(detections, act_clean){
  # library(data.table)
  # library(targets)
  # tar_load(detections)
  # tar_load(act_clean)
  
  dets <- copy(detections)
  act <- copy(act_clean)
  
  
  act_match <- rbind(
    dets[act, on = 'transmitter', nomatch = 0],
    # This winds up with mismatched colnames. figure that out.
    dets[act[, !'transmitter'],
         on = c(transmitter = 'tagidcodesensori'),
         nomatch = 0] |> 
      DT(, tagidcodesensori := 'TAG_VAL'),
    dets[act[, !'transmitter'],
         on = c(transmitter = 'tagidcodesensorii'),
         nomatch = 0] |> 
      DT(, tagidcodesensorii := 'TAG_VAL')
  )
}
