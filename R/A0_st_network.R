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