

source('setup.R')

# Load street network and select columns
st_network <- readr::read_rds(here::here('data-raw','street-network','st_network.rds')) %>% 
  dplyr::select(osm_id,name,highway,oneway,surface,geometry) %>% 
  data.table::setDT(key = 'osm_id')

# Load dodgr graph and calculate total travel time and lenght by way_id
graph_sp <- readr::read_rds(here::here('data-raw','street-network','graph_sp.rds')) %>% 
  as_tibble() %>% 
  dplyr::select(osm_id = way_id,from_id,to_id,d,d_wghtd=d_weighted,t=time,t_wghtd=time_weighted) 

graph <- data.table::setDT(graph,key = 'osm_id')[, lapply(.SD,sum, na.rm=T),by = .(osm_id)]

# Merge
st_graph <- data.table::merge.data.table(st_network,graph,
                                         all.x = TRUE, by = 'osm_id')

# Fix ways names' accents

st_graph <- st_graph[, name := gsub('Ã©','e',name)]
st_graph <- st_graph[, name := gsub('Ã´','o',name)]
st_graph <- st_graph[, name := gsub('Ã£','a',name)]
st_graph <- st_graph[, name := gsub('Ã¡','a',name)]
st_graph <- st_graph[, name := gsub('Ã§','c',name)]
st_graph <- st_graph[, name := gsub('Ãª','e',name)]
st_graph <- st_graph[, name := gsub('Ã¢','a',name)]
st_graph <- st_graph[, name := gsub('Ãº','u',name)]
st_graph <- st_graph[, name := gsub('Ã³','o',name)]
st_graph <- st_graph[, name := gsub('Ã‚','a',name)]
st_graph <- st_graph[, name := gsub('Ã','i',name)]
st_graph <- st_graph[, name := gsub('i¼','u',name)]
st_graph <- st_graph[, name := gsub('iš','u',name)]

st_graph <- st_graph[, name := stringr::str_to_upper(name)]

unique(st_graph$highway)
st_graph <- st_graph[highway %nin% c("pedestrian","footway","steps",'track','path','cycleway')]

readr::write_rds(st_graph, here::here('data-raw','street-network','st_graph.rds'))

speeds_jan2019 <- data.table::fread(paste0(path_uber,'2019/movement-speeds-hourly-sao-paulo-2019-1.csv'),
                                    select = c('year','month','day','hour',
                                               'osm_way_id',
                                               'speed_kph_mean', 'speed_kph_stddev')) %>% 
  dplyr::rename(osm_id=osm_way_id,speed_kph=speed_kph_mean,speed_sd=speed_kph_stddev) %>%  
  data.table::setDT(key = 'osm_id')


speeds_jan2019 <- speeds_jan2019[, lapply(.SD,mean, na.rm=T),by = .(osm_id,year,month,day,hour)]

speeds_jan2019$osm_id <- as.character(speeds_jan2019$osm_id)
speeds_jan2019 <- data.table::merge.data.table(speeds_jan2019,st_graph,
                                               all.x = TRUE, by = 'osm_id')

speeds_jan2019 %>% 
  dplyr::select(osm_id,name,highway,oneway,surface,
                year,month,day,hour,speed_kph,d,d_wghtd,t,t_wghtd) %>% 
  data.table::data.table(key = 'osm_id') %>% 
  readr::write_rds(here::here('data','speeds_jan_2019.rds'),compress = 'gz')