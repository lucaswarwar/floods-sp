# A1: scrap CGESP website for flood points in Sao Paulo from 2010 to 2020

# Load libraries
source('setup.R')

# Useful dataset with dates from 2010 to 2020
days <- data.table::fread(here::here('data-raw','days.csv'))

scrap_flood_points <- function(date){
  
  message('Working on ',date)
  
  day <- stringr::str_sub(date,1,2)
  month <- stringr::str_sub(date,4,5)
  year <- stringr::str_sub(date,7,10)
    
  link <- paste0('https://www.cgesp.org/v3/alagamentos.jsp?dataBusca=',day,'%2F',month,'%2F',year,'&enviaBusca=Buscar')
  
  df <- ralger::tidy_scrap(link, nodes = '.ponto-de-alagamento', colnames = 'flood_point') %>% data.table::setDT()
  
  df <- df[, date := paste(day,month,year,sep = '/')]
  
  df <- df[, flood_point := stringr::str_trim(flood_point)]
  df <- df[, flood_point := stringr::str_squish(flood_point)]
  
  df <- df[, start := stringr::str_sub(flood_point,4,8)]
  df <- df[, end := stringr::str_sub(flood_point,12,16)]
  
  df <- df[, flood_point := stringr::str_sub(flood_point,17,-1)]
  
  df2 <- stringr::str_split_fixed(df$flood_point, pattern = " Sentido: ",n=2) %>% as.data.frame()
  df3 <- stringr::str_split_fixed(df2$V2, pattern = "Referência: ",n=2) %>% as.data.frame()
  
  df <- df[, flood_point := stringr::str_trim(df2$V1)]
  df <- df[, direction := stringr::str_trim(df3$V1)]
  df <- df[, reference := stringr::str_trim(df3$V2)]
  
  return(df)
  
}
  
flood_points <- purrr::map(.x = days$days,.f = scrap_flood_points) %>% data.table::rbindlist()

readr::write_rds(flood_points, here::here('data','flood_points.rds'))

