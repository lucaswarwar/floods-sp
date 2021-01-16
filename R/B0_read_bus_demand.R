# B0: BUS DEMAND ----------------------------- 

# Load libraries
source('setup.R')

# list all files
files <- list.files(path = paste0(path_sptrans,'2019/'))

# Function to read excel files (1 per day)

read_files_sptrans <- function(x){
  
  path <- paste0(path_sptrans, '2019/',x)
  df <- readxl::read_xls(path, skip = 2)
  return(df)
  
}

sptrans <- purrr::map(.x = files, .f = read_files_sptrans) %>% data.table::rbindlist(fill = TRUE)

sptrans <- sptrans[,.(Data,Linha, `Passageiros Pagantes`,`Passageiros Int Ônibus->Ônibus`,
                      `Passageiros Com Gratuidade`,`Tot Passageiros Transportados`)]

data.table::setnames(sptrans, old = names(sptrans),
                     new = c('date', 'route','passenger_pgt','passenger_int','passenger_free','passenger_tot'))

sptrans <- sptrans[, cd_linha := stringr::str_sub(route,1,6)]
sptrans <- sptrans[, date := gsub('2018','2019',date)]

readr::write_rds(sptrans,here::here('data','bus-demand','bus_demand_2019.rds'))

### 1. SPTrans GTFS: bus, subway and train for SP city ###

# Read and filter original GTFS 

gtfs <- gtfs2gps::read_gtfs(paste0(path_gtfs,'spo.zip'))

gtfs_routes <- data.table::setDT(gtfs$routes)[route_type == 3,.(route_id,route_long_name)]
gtfs_trips <- data.table::setDT(gtfs$trips)[route_id %in% gtfs_routes$route_id,.(route_id,trip_id,trip_headsign,direction_id)]
gtfs_stoptimes <- data.table::setDT(gtfs$stop_times)[trip_id %in% gtfs_trips$trip_id,.(trip_id,stop_id,stop_sequence)] %>% unique()
gtfs_stops <- data.table::setDT(gtfs$stops)[stop_id %in% gtfs_stoptimes$stop_id,.(stop_id, stop_name,stop_lat,stop_lon)]

gtfs_final <- 
  gtfs_routes %>% 
  dplyr::right_join(gtfs_trips, by = 'route_id') %>% 
  dplyr::right_join(gtfs_stoptimes, by = 'trip_id') %>% 
  dplyr::left_join(gtfs_stops, by = 'stop_id')

gtfs_final <- setDT(gtfs_final)[ ,cd_linha := gsub('-','',route_id)]

gtfs_sf<-gtfs2gps::gtfs_stops_as_sf(gtfs)

gtfs_sf <- gtfs_sf %>% 
  dplyr::mutate(id_hex=h3jsr::point_to_h3(gtfs_sf, res= 9))

gtfs_final <- gtfs_final %>% 
  dplyr::left_join(select(gtfs_sf,stop_id,id_hex), by = 'stop_id')

readr::write_rds(gtfs_final,here::here('data','bus-demand','bus_routes.rds'))