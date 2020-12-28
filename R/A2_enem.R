source('setup.R')

enem <- data.table::fread(paste0(path_inep,'enem/DADOS/MICRODADOS_ENEM_2019.csv'),
                          select = c("NU_INSCRICAO","NU_ANO","CO_MUNICIPIO_RESIDENCIA",    
                                     "NU_IDADE","TP_SEXO","TP_COR_RACA","TP_ST_CONCLUSAO",
                                     "TP_ANO_CONCLUIU","TP_ESCOLA","TP_ENSINO","IN_TREINEIRO",
                                     "CO_ESCOLA","CO_MUNICIPIO_ESC",
                                     "TP_PRESENCA_CN","TP_PRESENCA_CH","TP_PRESENCA_LC","TP_PRESENCA_MT",             
                                     "NU_NOTA_CN","NU_NOTA_CH","NU_NOTA_LC","NU_NOTA_MT",
                                     "TP_STATUS_REDACAO","NU_NOTA_REDACAO",
                                     "Q005", "Q006","Q010","Q011"),
                          colClasses = 'character')

educacao_inep_final_2019 <- readr::read_rds("C:/Users/lucas/Desktop/R/floods-sp/educacao_inep_final_2019.rds") %>% 
  data.table::setDT(key = 'co_entidade')

enem <- data.table::setDT(enem,key = 'CO_ESCOLA')[CO_ESCOLA %in% educacao_inep_final_2019$co_entidade]

enem <- merge.data.table(enem,
                         educacao_inep_final_2019[,.(co_entidade,no_entidade,endereco,PrecisionDepth,geocode_engine,lon,lat)],
                         all.x = TRUE,
                         by.x = 'CO_ESCOLA',by.y = 'co_entidade')

enem <- enem[TP_ST_CONCLUSAO == '2' & IN_TREINEIRO == '0']

readr::write_rds(enem, here::here('data','schools','enem_2019.rds'))
