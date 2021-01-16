

source('setup.R')

st_graph <- readr::read_rds(here::here('data-raw','street-network','st_graph.rds'))

meses <- data.table::data.table(
  mo = as.character(seq(1,12,1)),
  ab = c('jan','feb','mar','apr',
         'may','jun','jul','aug',
         'sep','oct','nov','dec'))

read_speeds <- function(year, months = 'all'){
  
  message('Working on', year)
  
  read_speeds_mo <- function(x){
    
    message('Working on', x)
    
    meses_x <- meses %>% dplyr::filter(mo==x)
    ab <- meses_x$ab
    
    speeds <- data.table::fread(paste0(path_uber,year, '/movement-speeds-hourly-sao-paulo-',year,'-',x,'.csv'),
                                   select = c('year','month','day','hour',
                                              'osm_way_id',
                                              'speed_kph_mean', 'speed_kph_stddev')) %>% 
          dplyr::rename(osm_id=osm_way_id,speed_kph=speed_kph_mean,speed_sd=speed_kph_stddev) %>%  
          data.table::setDT(key = 'osm_id')
    
    
    speeds <- speeds[, lapply(.SD,mean, na.rm=T),by = .(osm_id,year,month,day,hour)]
    
    speeds$osm_id <- as.character(speeds$osm_id)
    speeds <- data.table::merge.data.table(speeds,st_graph,
                                       all.x = TRUE, by = 'osm_id')
    
    speeds %>% 
      dplyr::select(osm_id,name,highway,oneway,surface,
                    year,month,day,hour,speed_kph,d,d_wghtd,t,t_wghtd) %>% 
      data.table::data.table(key = 'osm_id') %>% 
      readr::write_rds(here::here('data','speeds',year,paste0('speeds_',ab,'_',year,'.rds')),compress = 'gz')
    
  }
  
  if (months == 'all') {
    
    x <- meses$mo
    
  } else (x = months)
  
  purrr::walk(.x = x,.f = read_speeds_mo)
  
}

read_speeds(year = '2019',months = '7')

