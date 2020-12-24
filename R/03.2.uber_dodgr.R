###################################################################
##### 03.2. Read Uber speed data and combine with Dodgr graph #####
###################################################################

# Setup -------
source('setup.R')

# Set path where raw files are saved
path_uber <- 'D:/data/uber/spo/speeds/'

### 1. Function to download, clean and save uber data by month ###

read_uber <- function(x){
  
  year <- '2018'
  
  # Year 2019
  if (x == '12' & year == '2019') {
    month <- 'dec'
    weekend <- c(1,7,8,14,15,21,22,25,28,29,31)
  } 
  
  if (x == '11' & year == '2019') {
    month <- 'nov'
    weekend <- c(2,3,9,10,15,16,17,23,24,30)
  } 
  
  if (x == '10' & year == '2019') {
    month <- 'oct'
    weekend <- c(5,6,12,13,19,20,26,27)
  } 
  
  if (x == '9' & year == '2019') {
    month <- 'sep'
    weekend <- c(1,7,8,14,15,21,22,28,29)
  } 
  
  if (x == '8' & year == '2019') {
    month <- 'aug'
    weekend <- c(3,4,10,11,17,18,24,25,31)
  }
  
  if (x == '7' & year == '2019') {
    month <- 'jul'
    weekend <- c(6,7,13,14,20,21,27,28)
  } 
  
  if (x == '6' & year == '2019') {
    month <- 'jun'
    weekend <- c(1,2,8,9,15,16,20,22,23,29,30)
  }
  
  if (x == '5' & year == '2019') {
    month <- 'may'
    weekend <- c(1,4,5,11,12,18,19,25,26)
  } 
  
  if (x == '4' & year == '2019') {
    month <- 'apr'
    weekend <- c(6,7,13,14,19,20,21,27,28)
  }
  
  if (x == '3' & year == '2019') {
   month <- 'mar' 
   weekend <- c(2,3,5,9,10,16,17,23,24,30,31)
  } 
  
  if (x == '2' & year == '2019') {
    month <- 'feb'
    weekend <- c(2,3,9,10,16,17,23,24)
  } 
  
  if (x == '1' & year == '2019') {
    month <- 'jan'
    weekend <- c(1,5,6,12,13,19,20,26,27)
  }
  
  # Year 2018
  if (x == '12' & year == '2018') {
    month <- 'dec'
    weekend <- c(1,2,8,9,15,16,22,23,25,31)
  } 
  
  if (x == '11' & year == '2018') {
    month <- 'nov'
    weekend <- c(2,3,4,10,11,15,17,18,24,25)
  } 
  
  if (x == '10' & year == '2018') {
    month <- 'oct'
    weekend <- c(6,7,12,13,14,20,21,27,28)
  } 
  
  if (x == '9' & year == '2018') {
    month <- 'sep'
    weekend <- c(1,2,7,8,9,15,16,22,23,29,30)
  } 
  
  if (x == '8' & year == '2018') {
    month <- 'aug'
    weekend <- c(4,5,11,12,18,19,25,26)
  }
  
  if (x == '7' & year == '2018') {
    month <- 'jul'
    weekend <- c(1,7,8,9,14,15,21,22,28,29)
  } 
  
  if (x == '6' & year == '2018') {
    month <- 'jun'
    weekend <- c(2,3,9,10,16,17,23,24,30)
  }
  
  if (x == '5' & year == '2018') {
    month <- 'may'
    weekend <- c(1,5,6,12,13,19,20,26,27,31)
  } 
  
  if (x == '4' & year == '2018') {
    month <- 'apr'
    weekend <- c(1,7,8,14,15,21,22,28,29)
  }
  
  if (x == '3' & year == '2018') {
    month <- 'mar' 
    weekend <- c(3,4,10,11,17,18,24,25,30,31)
  } 
  
  if (x == '2' & year == '2018') {
    month <- 'feb'
    weekend <- c(3,4,10,11,17,18,24,25)
  } 
  
  if (x == '1' & year == '2018') {
    month <- 'jan'
    weekend <- c(1,6,7,13,14,20,21,27,28)
  }
  
  message('Working on',x,year)
  
  # Load raw data
  speeds_spo <- data.table::fread(paste0(path_uber,year, '/movement-speeds-hourly-sao-paulo-',year,'-',x,'.csv'),
                                  select = c('year','month','day', 'hour', 'osm_way_id',
                                             'osm_start_node_id','osm_end_node_id', 'speed_kph_mean'),
                                  colClasses = 'character') 
  
  # Filter weekdays
  speeds_spo <- data.table::setDT(speeds_spo)[day %nin% weekend]
  
  # Create time buckets
  speeds_spo <- data.table::setDT(speeds_spo)[hour %in% c(6:19)]
  speeds_spo <- speeds_spo[, hour := data.table::fcase(hour %in% c(6,7), "6h-8h",
                                                       hour %in% c(8:11), "8h-12h",
                                                       hour %in% c(12,13), "12h-14h",
                                                       hour %in% c(14:16), "14h-17h",
                                                       hour %in% c(17:19), "17h-20h")]
  
  # Aggregate by route and hod
  speeds_spo$speed_kph_mean <- as.numeric(speeds_spo$speed_kph_mean)
  speeds_spo <- speeds_spo[,.(avg_speed = mean(speed_kph_mean, na.rm = T)), 
                           by = .(month, hour,osm_way_id,osm_start_node_id,osm_end_node_id)]
  
  # Save it
  readr::write_rds(speeds_spo, here::here('data','uber',year, paste0('03.2.uber_speed_',month,'.rds'))) 
  
}

# Apply function
x <- c('1','2','3','4','5','6','7','8','9','10','11','12')
purrr::walk(.x = x, .f = read_uber)

### 2. Combine Uber data with Dodgr Graph ###

# Load Dodgr graph
graph <- readr::read_rds(here::here('data','street_network', '03.1.dodgr_graph_sp.rds'))

# Function to aggregate merged Uber and Dodger data accross months

merge_uber_dodgr <- function(){
  
  year <- '2018'
  x <- c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')
  
  # Function to merge data at each month
  
  merge_month <- function(x){  
    
    graph <- graph
    year <- year
    
    message('Working on ',x)
    
    # Load Uber data
    speeds <- readr::read_rds(here::here('data','uber',year,paste0('03.2.uber_speed_',x,'.rds')))
  
    data.table::setDT(graph)
    data.table::setDT(speeds)
    
    # Merge by segment (road, start, end)
    merge <- data.table::merge.data.table(speeds[,!c('month')],
                                          graph[,.(way_id, edge_id, from_id,from_lon,from_lat, to_id,to_lon,to_lat,d,time)],
                                          by.x = c('osm_start_node_id','osm_end_node_id','osm_way_id'),
                                          by.y = c('from_id', 'to_id','way_id'))
  
    rm(speeds)
    return(merge)
  
  }
  
  # Apply merge function
  merge_final <- purrr::map(.x = x, .f = merge_month) %>% data.table::rbindlist()
  
  # Aggregate months
  merge_final <- data.table::setDT(merge_final)[, .(avg_speed = mean(avg_speed, na.rm = T)),
                                                  by = .(osm_way_id,osm_start_node_id,osm_end_node_id,
                                                         edge_id,from_lon,from_lat,to_lon,to_lat,hour,d,time)]
  
  # Save it
  readr::write_rds(merge_final, here::here('data','uber',year,paste0('03.2.uber_dodgr_',year,'.rds')))

}

merge_uber_dodgr()

### 3. Get travel time for segments not in Uber from Dodgr ###

# Load merged data

read_uber_dodgr <- function(x){
  uber_dodgr<- readr::read_rds(here::here('data','uber',x,paste0('03.2.uber_dodgr_',x,'.rds')))
  return(uber_dodgr)
} 

x <- c('2018','2019')
uber_dodgr <- purrr::map(.x = x,.f = read_uber_dodgr) %>% data.table::rbindlist()

# Aggregate years
uber_dodgr <- data.table::setDT(uber_dodgr)[, .(avg_speed = mean(avg_speed, na.rm = T)),
                                                by = .(osm_way_id,osm_start_node_id,osm_end_node_id,
                                                 edge_id,from_lon,from_lat,to_lon,to_lat,hour,d,time)]

n_distinct(uber_dodgr$edge_id) # 104,028 unique segments
n_distinct(graph$edge_id) # 2,092,815 unique segments

# Rename columns and arrange travel times
uber_dodgr <- data.table::setnames(uber_dodgr,
                                   old = c('osm_way_id','osm_start_node_id','osm_end_node_id','d','time','avg_speed'),
                                   new = c('way_id','from_id','to_id','dist','time_dodgr','speed_uber'))

# Travel time for Uber data
uber_dodgr <- uber_dodgr[, time_uber := (dist/(speed_uber/3.6))]
# Speed for dodgr data
uber_dodgr <- uber_dodgr[, speed_dodgr := ((dist/time_dodgr)*3.6)] # Dodgr assumes max. speed for the street
# Speed and Time diff
uber_dodgr <- uber_dodgr[, pct_speed := speed_uber/speed_dodgr]
uber_dodgr <- uber_dodgr[, pct_time := time_uber/time_dodgr]
# Get info on highway type
uber_dodgr <- data.table::merge.data.table(uber_dodgr,
                                           graph[,.(edge_id,highway)],
                                           by = 'edge_id')

# Save
readr::write_rds(uber_dodgr,here::here('data','uber','03.2.uber_dodgr.rds'))

# Discover how speed/time is different between Uber and Dodgr according to street type and max. speed
uber_dodgr[,.(mean(pct_speed)),by = .(highway)] %>% dplyr::arrange(-V1)
uber_dodgr[,.(mean(pct_speed)),by = .(as.character(speed_dodgr))] %>% dplyr::arrange(-V1)

#    highway        pct_speed 
#  living_street 2.3506310
#        service 1.7252346
#   unclassified 1.2417292
#  tertiary_link 1.1529199
#  motorway_link 1.0589137
# secondary_link 0.9558304
#    residential 0.9513850
#   primary_link 0.8555225
#       motorway 0.8423851
#     trunk_link 0.7417267
#       tertiary 0.6814507
#      secondary 0.5187505
#        primary 0.4878073
#          trunk 0.4694326

#  dodgr_speed    pct_speed
#           10 2.3506310
#           15 1.7252346
#           20 1.1529199
#           45 1.0589137
#           25 0.9522117
#           30 0.8559667
#           90 0.8423851
#           40 0.6822921
#           55 0.5184827
#           65 0.4878073
#           85 0.4694326

# Dodgr speed and highway type is the same thing
# Discover how speed/time is different according to speed limit and hour of day

h <- uber_dodgr[,.(mean(pct_speed)),by = .(hour,as.character(speed_dodgr))] %>% dplyr::arrange(-V1)

#    speed max.    hour     pct_speed

#1:            10  8h-12h 2.5735737
#2:            10 14h-17h 2.4010952
#3:            10   6h-8h 2.3877112
#4:            10 12h-14h 2.3719726
#5:            10 17h-20h 2.0190966
#6:            15 17h-20h 1.7655990
#7:            15 14h-17h 1.7220712
#8:            15  8h-12h 1.7216763
#9:            15 12h-14h 1.7055320
#10:           15   6h-8h 1.7021825
#11:           20   6h-8h 1.1980059
#12:           20  8h-12h 1.1798462
#13:           20 12h-14h 1.1711126
#14:           20 14h-17h 1.1678867
#15:           45 12h-14h 1.1034634
#16:           45 14h-17h 1.0842979
#17:           45  8h-12h 1.0676390
#18:           20 17h-20h 1.0594371
#19:           45 17h-20h 1.0228170
#20:           45   6h-8h 1.0164072
#21:           25   6h-8h 0.9967812
#22:           25  8h-12h 0.9701057
#23:           25 12h-14h 0.9547933
#24:           25 14h-17h 0.9480543
#25:           25 17h-20h 0.9031192
#26:           30   6h-8h 0.8690148
#27:           90 12h-14h 0.8676166
#28:           30 12h-14h 0.8641877
#29:           30 14h-17h 0.8622542
#30:           30  8h-12h 0.8560178
#31:           90 14h-17h 0.8554611
#32:           90  8h-12h 0.8396840
#33:           90   6h-8h 0.8348607
#34:           30 17h-20h 0.8291279
#35:           90 17h-20h 0.8141868
#36:           40   6h-8h 0.7149267
#37:           40  8h-12h 0.6933609
#38:           40 12h-14h 0.6779667
#39:           40 14h-17h 0.6773300
#40:           40 17h-20h 0.6493252
#41:           55   6h-8h 0.5458955
#42:           55  8h-12h 0.5279143
#43:           55 12h-14h 0.5145975
#44:           55 14h-17h 0.5128412
#45:           65   6h-8h 0.5086469
#46:           65  8h-12h 0.4929458
#47:           55 17h-20h 0.4914410
#48:           65 12h-14h 0.4900569
#49:           85   6h-8h 0.4868812
#50:           65 14h-17h 0.4848484
#51:           85 12h-14h 0.4796544
#52:           85 14h-17h 0.4684880
#53:           85  8h-12h 0.4682157
#54:           65 17h-20h 0.4627277
#55:           85 17h-20h 0.4439237

uber_dodgr_wide <- uber_dodgr[,!c('highway','time_uber','speed_uber', 'pct_time')]
uber_dodgr_wide <- uber_dodgr_wide %>%  tidyr::pivot_wider(names_from = hour,
                                                           names_prefix = 'pct_speed_',
                                                           values_from = pct_speed)

# Get segments not in Uber data 
graph1 <- data.table::setDT(graph)[edge_id %nin% uber_dodgr$edge_id]

# Make columns the same as uber_dodgr df
graph1 <- graph1[,.(edge_id,way_id,from_id,from_lon,from_lat,to_id,to_lon,to_lat,d,time)]
data.table::setnames(graph1,
                     old = c('d','time'),
                     new = c('dist','time_dodgr'))

graph1 <- graph1[, speed_dodgr := ((dist/time_dodgr)*3.6)] # Dodgr assumes max. speed for the street

# Adjust speed pct according to hour of day and speed limit

graph1 <- graph1[, `pct_speed_6h-8h` := data.table::fcase(as.character(speed_dodgr) == '10', 2.3877112,
                                                          as.character(speed_dodgr) == '15', 1.7021825,
                                                          as.character(speed_dodgr) == '20', 1.1980059,
                                                          as.character(speed_dodgr) == '25', 0.9967812,
                                                          as.character(speed_dodgr) == '30', 0.8690148,
                                                          as.character(speed_dodgr) == '40', 0.7149267,
                                                          as.character(speed_dodgr) == '45', 1.0164072,
                                                          as.character(speed_dodgr) == '55', 0.5458955,
                                                          as.character(speed_dodgr) == '65', 0.5086469,
                                                          as.character(speed_dodgr) == '85', 0.4868812,
                                                          as.character(speed_dodgr) == '90', 0.8348607)]

graph1 <- graph1[, `pct_speed_8h-12h` := data.table::fcase(as.character(speed_dodgr) == '10', 2.5735737,
                                                           as.character(speed_dodgr) == '15', 1.7216763,
                                                           as.character(speed_dodgr) == '20', 1.1798462,
                                                           as.character(speed_dodgr) == '25', 0.9701057,
                                                           as.character(speed_dodgr) == '30', 0.8560178,
                                                           as.character(speed_dodgr) == '40', 0.6933609,
                                                           as.character(speed_dodgr) == '45', 1.0676390,
                                                           as.character(speed_dodgr) == '55', 0.5279143,
                                                           as.character(speed_dodgr) == '65', 0.4929458,
                                                           as.character(speed_dodgr) == '85', 0.4682157,
                                                           as.character(speed_dodgr) == '90', 0.8396840)]

graph1 <- graph1[, `pct_speed_12h-14h` := data.table::fcase(as.character(speed_dodgr) == '10', 2.3719726,
                                                            as.character(speed_dodgr) == '15', 1.7055320,
                                                            as.character(speed_dodgr) == '20', 1.1711126,
                                                            as.character(speed_dodgr) == '25', 0.9547933,
                                                            as.character(speed_dodgr) == '30', 0.8641877,
                                                            as.character(speed_dodgr) == '40', 0.6779667,
                                                            as.character(speed_dodgr) == '45', 1.1034634,
                                                            as.character(speed_dodgr) == '55', 0.5145975,
                                                            as.character(speed_dodgr) == '65', 0.4900569,
                                                            as.character(speed_dodgr) == '85', 0.4796544,
                                                            as.character(speed_dodgr) == '90', 0.8676166)]

graph1 <- graph1[, `pct_speed_14h-17h` := data.table::fcase(as.character(speed_dodgr) == '10', 2.4010952,
                                                            as.character(speed_dodgr) == '15', 1.7220712,
                                                            as.character(speed_dodgr) == '20', 1.1678867,
                                                            as.character(speed_dodgr) == '25', 0.9480543,
                                                            as.character(speed_dodgr) == '30', 0.8622542,
                                                            as.character(speed_dodgr) == '40', 0.6773300,
                                                            as.character(speed_dodgr) == '45', 1.0842979,
                                                            as.character(speed_dodgr) == '55', 0.5128412,
                                                            as.character(speed_dodgr) == '65', 0.4848484,
                                                            as.character(speed_dodgr) == '85', 0.4684880,
                                                            as.character(speed_dodgr) == '90', 0.8554611)]

graph1 <- graph1[, `pct_speed_17h-20h` := data.table::fcase(as.character(speed_dodgr) == '10', 2.0190966,
                                                            as.character(speed_dodgr) == '15', 1.7655990,
                                                            as.character(speed_dodgr) == '20', 1.0594371,
                                                            as.character(speed_dodgr) == '25', 0.9031192,
                                                            as.character(speed_dodgr) == '30', 0.8291279,
                                                            as.character(speed_dodgr) == '40', 0.6493252,
                                                            as.character(speed_dodgr) == '45', 1.0228170,
                                                            as.character(speed_dodgr) == '55', 0.4914410,
                                                            as.character(speed_dodgr) == '65', 0.4627277,
                                                            as.character(speed_dodgr) == '85', 0.4439237,
                                                            as.character(speed_dodgr) == '90', 0.8141868)]

# Combine both
uber_dodgr <- rbind(uber_dodgr_wide, graph1)
readr::write_rds(uber_dodgr,here::here('data','uber','03.2.uber_dodgr_wide.rds'))
