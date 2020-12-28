# A0: build street network and Dodgr graph for SP

# Setup -------
source('setup.R')

### 1. Download OSM data for SP metro ###

sao <- osmdata::opq('são paulo') %>% 
       osmdata::add_osm_feature(key = 'highway') %>% 
       osmdata::osmdata_sf(quiet = F) %>% 
       osmdata::osm_poly2line()


# Extract online lines
sao_linhas <- sao$osm_lines %>% 
  dplyr::select(osm_id, name, bicycle, covered,foot,highway,incline,
                motorcar,motorcycle,motor_vehicle,oneway,surface,tracktype,tunnel,
                width,geometry)

# Save
readr::write_rds(sao_linhas, here::here('data-raw','street_network','st_network_sp.rds'))

### 2. Create Dodgr Graph ###

sp_graph <- dodgr::weight_streetnet(sao_linhas, wt_profile = 'motorcar')

# Select rows and save
readr::write_rds(sp_graph, here::here('data-raw','street_network','graph_sp.rds'))

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

graph_hex <- st_graph %>% 
  sf::st_as_sf() %>% 
  sf::st_centroid() 

graph_hex <- graph_hex %>% 
  dplyr::mutate(id_hex = h3jsr::point_to_h3(graph_hex, res = 9)) %>% 
  dplyr::select(osm_id, id_hex)

st_graph <- st_graph %>% 
  dplyr::left_join(graph_hex, by = 'osm_id') %>% 
  data.table::setDT(key = 'osm_id')

readr::write_rds(st_graph, here::here('data','street-network','st_graph.rds'))


