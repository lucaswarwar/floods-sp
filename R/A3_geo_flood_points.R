# A3: geocodes flood points from last 5 years 

# Load libraries
source('setup.R')


# Loads clean data
floods <- readr::read_rds(here::here('data','flood_points_clean.rds')) %>% data.table::setDT()


library(ggmap)

my_api <- 'AIzaSyBcgzW8RhJ6xbGPZZslf_CaGLodGvRLgzU'
register_google(key = my_api) # registra a key do Google API

# locations
enderecos <- unique(floods$location) # 4351
enderecos <- paste0(enderecos, ' - São Paulo, SP')

# Run Google API
coords_enderecos <- lapply(enderecos,geocode) %>% data.table::rbindlist()

coords_enderecos <- coords_enderecos[, location := enderecos]

floods$location <- paste0(floods$location, ' - São Paulo, SP')

floods <- data.table::merge.data.table(floods,
                                       coords_enderecos,
                                       all.x = TRUE,
                                       by = 'location')



floods <- flood_points_geo[!is.na(lat)]

floods_sf <- sf::st_as_sf(floods, coords = c('lon','lat'), crs = 4326)

floods_sf <- floods_sf %>% mutate(id_hex = h3jsr::point_to_h3(floods_sf,res=9))

floods <- floods[, id_hex := floods_sf$id_hex]

floods <- data.table::setDT(floods)[lat<0,.(date,flood_point,reference,start,end,lon,lat,id_hex)]

floods <- floods[, sp_hex := h3jsr::h3_to_polygon(id_hex)]

floods <- floods[, year := stringr::str_sub(date,7,10)]
floods <- floods[, month := stringr::str_sub(date,4,5)]
floods <- floods[, month := data.table::fcase(
  month == '01','jan',month == '02','feb',
  month == '03','mar',month == '04','apr',
  month == '05','may',month == '06','jun',
  month == '07','jul',month == '08','aug',
  month == '09','sep',month == '10','oct',
  month == '11','nov',month == '12','dec')]

floods <- floods[, start := paste0(start,':00')]
floods <- floods[, end := paste0(end,':00')]

floods <- floods[, start := hms::as_hms(start)]
floods <- floods[, end := hms::as_hms(end)]
floods<- floods[,duration := lubridate::as.duration(end - start)]

df <- stringr::str_split_fixed(floods$duration,'s',n=2) %>% as.data.frame()
floods<- floods[,duration := as.numeric(df$V1)]
floods <- floods[, duration := ifelse(duration<0,24*3600+duration,duration)]
floods <- floods[, rush := ifelse(lubridate::hour(start) %in% c(6,7,17,18) |lubridate::hour(end) %in% c(6,7,17,18),1,0)]

hex_date <- floods[,.(total_duration = sum(duration,na.rm = TRUE),
                      avg_duration = mean(duration,na.rm = TRUE),
                      rush = sum(rush,na.rm = TRUE),
                      points=.N),
                   by = .(date,id_hex)]

hex_month <- floods[,.(total_duration = sum(duration,na.rm = TRUE),
                      avg_duration = mean(duration,na.rm = TRUE),
                      rush = sum(rush,na.rm = TRUE),
                      points=.N),
                   by = .(month, year,id_hex)]

hex_year <- floods[,.(total_duration = sum(duration,na.rm = TRUE),
                       avg_duration = mean(duration,na.rm = TRUE),
                       rush = sum(rush,na.rm = TRUE),
                       points=.N),
                    by = .(year,id_hex)]

hex_2019 <- hex_year[year == '2019']

mapview::mapview(hex_2019 %>% sf::st_as_sf(),zcol='avg_duration')

floods <- floods[,.(date,month,year,flood_point,reference,start,end,duration,rush,lon,lat,id_hex,sp_hex)]

readr::write_rds(floods, here::here('data','floods','flood_points_total.rds'))
readr::write_rds(hex_date, here::here('data','floods','flood_points_date.rds'))
readr::write_rds(hex_month, here::here('data','floods','flood_points_month.rds'))
readr::write_rds(hex_year, here::here('data','floods','flood_points_year.rds'))
