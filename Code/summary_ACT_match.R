library(data.table)

act <- readxl::read_excel('c:/users/darpa2/downloads/Active transmitters_20210810.xlsx')

dets <- list.files('p:/obrien/biotelemetry/mainstem backbone/detection files',
                   pattern = '1214.*.csv',
                   full.names = T)



dets <- lapply(dets, fread, fill = T, col.names = function(.) tolower(gsub('[) ()]', '',.)))


dets <- rbindlist(dets)



#General summaries
act_match <- dets[act, on = c(transmitter = 'Tag ID Code Standard'), nomatch = 0]
act_match[, mon := month(dateandtimeutc)]
act_match[, commonname := tolower(`Common Name`)]

xtabs(~ commonname + mon, data = act_match)
xtabs(~ commonname + mon, data = unique(act_match, by = 'transmitter'))
unique(act_match$`Primary Researcher`)
