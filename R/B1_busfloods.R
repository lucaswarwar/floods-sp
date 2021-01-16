# B2 : bus demand and flood points
# 

# Setu up
source('setup.R')

# Load files
floods <- readr::read_rds(here::here('data','floods','flood_points_date.rds')) %>% data.table::setDT()
bus_demand <- readr::read_rds(here::here('data','bus-demand','bus_demand_2019.rds')) %>% data.table::setDT()
routes <- readr::read_rds(here::here('data','bus-demand','bus_routes.rds')) %>% data.table::setDT()

# Some adjustments
bus_demand <- bus_demand[, date := lubridate::ymd(date)]
floods <- floods[, date := lubridate::dmy(date)]

routes <- routes[, cd_linha := gsub('-','',route_id)]

match <- routes[,.(cd_linha,id_hex)] %>% unique()

dates <- data.frame(date = bus_demand$date %>% unique())

match <- purrr::map_dfr(seq_len(365),~match) # replicate dataset for 365 days

dates <- purrr::map_dfr(seq_len(50404),~dates)

match <- dplyr::bind_cols(match, dates)

match <- merge.data.table(match,floods,all.x = TRUE,by = c('date','id_hex'))
match <- match[, total_duration := ifelse(is.na(total_duration),0,total_duration)]
match <- match[, avg_duration := ifelse(is.na(avg_duration),0,avg_duration)]
match <- match[, rush := ifelse(is.na(rush),0,rush)]
match <- match[, points := ifelse(is.na(points),0,points)]

match <- match[,.(total_duration = sum(total_duration),
                  avg_duration = mean(avg_duration),
                  rush = sum(rush),
                  points = sum(points)),
               by = .(date,cd_linha)]

bus_demand <- merge.data.table(bus_demand,match,all.x = TRUE,by = c('date','cd_linha'))

bus_demand <- bus_demand[, total_duration := ifelse(is.na(total_duration),0,total_duration)]
bus_demand <- bus_demand[, avg_duration := ifelse(is.na(avg_duration),0,avg_duration)]
bus_demand <- bus_demand[, rush := ifelse(is.na(rush),0,rush)]
bus_demand <- bus_demand[, points := ifelse(is.na(points),0,points)]

bus_demand <- bus_demand[, flood := ifelse(points==0,0,1)]

bus <- bus_demand[, .(tot=mean(passenger_tot)),by = .(cd_linha,flood)]
bus <- bus %>% group_by(cd_linha) %>% mutate(n=n()) %>% filter(n==2)
bus <- setDT(bus)[flood==0]

bus_demand <- bus_demand[cd_linha %in% bus$cd_linha]

bus_demand <- merge.data.table(bus_demand,
                               bus[,.(cd_linha,tot)],
                               by = 'cd_linha')

bus_demand[, op_rate := passenger_tot/tot]

ggplot(bus_demand %>% filter(op_rate <3)) +
  geom_point(aes(total_duration,op_rate))

model = fixest::feols(op_rate ~ points | date, data = bus_demand)
summary(model)
