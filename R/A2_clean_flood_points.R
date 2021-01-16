# A2: cleans and padtonize addresses from webscrao

# Load libraries
source('setup.R')

# Loads scrap
floods <- readr::read_rds(here::here('data','flood_points.rds')) %>% data.table::setDT()


# Fix some rows with wrong scrap
floods <- floods[, flood_point := ifelse(is.na(as.numeric(stringr::str_sub(end,1,1))), 
                                         paste0(end,flood_point), flood_point)]

# Function to padronize and fix common mistakes

clean_addres <- function(string) {
  
  x <- string
  
  # Remove points, commas etc
  x <- gsub('[*]','',x)
  x <- gsub('[.]','',x)
  x <- gsub('[,]','',x)
  x <- gsub('[º]','',x)
  
  # Uppercase
  x <- stringr::str_to_upper(x)
  
  # Remove accents
  x <- gsub("Ç","C",x) 
  x <- gsub("Ã","A",x) 
  x <- gsub("Á","A",x) 
  x <- gsub("À","A",x) 
  x <- gsub("Â","A",x) 
  x <- gsub("Ä","A",x) 
  x <- gsub("É","E",x) 
  x <- gsub("Ê","E",x) 
  x <- gsub("È","E",x) 
  x <- gsub("Ë","E",x) 
  x <- gsub("Í","I",x) 
  x <- gsub("Ì","I",x) 
  x <- gsub("Î","I",x) 
  x <- gsub("Ï","I",x) 
  x <- gsub("Ó","O",x) 
  x <- gsub("Ò","O",x) 
  x <- gsub("Ô","O",x) 
  x <- gsub("Ö","O",x) 
  x <- gsub("Õ","O",x) 
  x <- gsub("Ú","U",x) 
  x <- gsub("Û","U",x) 
  x <- gsub("Ù","U",x) 
  x <- gsub("Ü","U",x) 
  
  # Padronize way type
  x <- gsub('^R ','RUA ',x)
  x <- gsub('^AV ','AVENIDA ',x)
  x <- gsub('^ES ','ESTRADA ',x)
  x <- gsub('^EST ','ESTRADA ',x)
  x <- gsub('^ESTR ','ESTRADA ',x)
  x <- gsub('^PCA ','PRACA ',x)
  x <- gsub('^PC ','PRACA ',x)
  x <- gsub('^TN ','TUNEL ',x)
  x <- gsub('^AC ','ACESSO ',x)
  x <- gsub('^AL ','ALAMEDA ',x)
  x <- gsub('^ROD ','RODOVIA ',x)
  x <- gsub('^PTE ','PONTE ',x)
  x <- gsub('^PT ','PONTE ',x)
  x <- gsub('^VD ','VIADUTO ',x)
  x <- gsub('^ELEV ','ELEVADO ',x)
  x <- gsub('^LG ','LARGO ',x)
  x <- gsub('^LRG ','LARGO ',x)
  x <- gsub('^PQ ','PARQUE ',x)
  x <- gsub('^MARG ','MARGINAL ',x)
  
  # Padronize common street names 
  x <- gsub(" DR "," DOUTOR ",x) 
  x <- gsub(" DRA "," DOUTORA ",x) 
  x <- gsub(" PROF "," PROFESSOR ",x) 
  x <- gsub(" ENG "," ENGENHEIRO ",x) 
  x <- gsub(" EDUC "," EDUCADOR ",x) 
  x <- gsub(' EDUCA ',' EDUCADOR ',x)
  x <- gsub(" CON "," CONEGO ",x) 
  x <- gsub(" CONS "," CONSELHEIRO ",x) 
  x <- gsub(" JORN "," JORNALISTA ",x) 
  x <- gsub(" MAEST "," MAESTRO ",x) 
  x <- gsub(' ARQ ',' ARQUITETO ',x)
  x <- gsub(' JR',' JUNIOR',x)
  
  x <- gsub(" CEL "," CORONEL ",x) 
  x <- gsub(" MAJ "," MAJOR ",x)
  x <- gsub(" SGT "," SARGENTO ",x)
  x <- gsub(" BRIG "," BRIGADEIRO ",x) 
  x <- gsub(" TN "," TENENTE ",x)
  x <- gsub(" MAL "," MARECHAL ",x) 
  x <- gsub(" TEN "," TENENTE ",x) 
  x <- gsub(" CAP "," CAPITAO ",x) 
  x <- gsub(" GAL "," GENERAL ",x) 
  x <- gsub(" ALM "," ALMIRANTE ",x) 
  x <- gsub(" CB "," CABO ",x) 
  
  x <- gsub(" CDSSA "," CONDESSA ",x) 
  x <- gsub(' CDDE ',' CONDE DE ',x)
  x <- gsub(" CD "," CONDE ",x) 
  x <- gsub(" MON "," MONSENHOR ",x) 
  x <- gsub(" BR "," BARAO ",x) 
  x <- gsub(' BRDO ',' BARAO DO ',x)
  x <- gsub(" DUQ "," DUQUE ",x) 
  x <- gsub(" VISC "," VISCONDE ",x) 
  x <- gsub(' VIS ',' VISCONDE ',x)
  x <- gsub('VISCDE ','VISCONDE DE ',x)
  x <- gsub(" PRINC "," PRINCIPE ",x) 
  x <- gsub(' COMEN ',' COMENDADOR ',x)
  x <- gsub(' PRSA ',' PRINCESA ',x)
  x <- gsub(' MARQDE ',' MARQUES DE ',x)
  x <- gsub(' MARQ ',' MARQUES ',x)
  x <- gsub(' IMP ',' IMPERATRIZ ',x)
  
  x <- gsub(" PRES "," PRESIDENTE ",x) 
  x <- gsub(" DEP "," DEPUTADO ",x) 
  x <- gsub(" SEN "," SENADOR ",x)
  x <- gsub(" GOV "," GOVERNADOR ",x)
  x <- gsub(" EMB "," EMBAIxADOR ",x) 
  x <- gsub(" DES "," DESEMBARGADOR ",x) 
  x <- gsub(" VER "," VEREADOR ",x) 
  x <- gsub(" MIN "," MINISTRO ",x) 
  x <- gsub(" REG "," REGENTE ",x) 
  
  x <- gsub(" S "," SAO ",x) 
  x <- gsub(" STO "," SANTO ",x) 
  x <- gsub(" SRA "," SENHORA ",x) 
  x <- gsub(" NSA "," NOSSA ",x)
  x <- gsub(" NSASRA "," NOSSA SENHORA ",x) 
  x <- gsub(" NSRA "," NOSSA SENHORA ",x) 
  x <- gsub(" NSRADO "," NOSSA SENHORA DO ",x) 
  x <- gsub(" STA "," SANTA ",x)
  x <- gsub(" REV "," REVERENDO ",x) 
  x <- gsub(" PE "," PADRE ",x) 
  x <- gsub(" PDE "," PADRE ",x) 
  x <- gsub(' CARD ',' CARDEAL ',x)
  
  # Padronize numbers
  x <- gsub(' 25 ',' VINTE E CINCO ',x)
  x <- gsub(' 14 ',' QUATORZE ',x)
  x <- gsub(' 31 ',' TRINTA E UM ',x)
  x <- gsub(' 23 ',' VINTE E TRES ',x)
  x <- gsub(' 9 ',' NOVE ',x)
  
  # Common mistakes for SP streets
  x <- gsub('IGNACIO ','INACIO ',x)  
  x <- gsub('PROFESSOR LUIS INACIO ','PROFESSOR LUIZ INACIO ',x)
  x <- gsub('LUIZ INACIO DE ANHAIA MELLO ','LUIZ INACIO ANHAIA MELLO ',x)
  x <- gsub('ABRAHAO ','ABRAAO ',x)
  x <- gsub('BETTENCOURT ','BITENCOURT ',x)
  x <- gsub(' TERESA ',' TEREZA ',x)
  x <- gsub(' PAN - AMERICANA ',' PAN-AMERICANA ',x)
  x <- gsub(' LUIS CAPELLO ',' LUIZ COPELLO ',x)
  x <- gsub(' NOSSA SENHORA O ',' NOSSA SENHORA DO O ',x)
  x <- gsub(' NOSSA SENHORA DO SABARA ',' NOSSA SENHORA DE SABARA ',x)
  x <- gsub(' CASTELO BRANCO - LOCAL ',' CASTELO BRANCO ',x)
  
  # Typos
  x <- gsub('xAV ','AVENIDA ',x)
  x <- gsub('RDR ','RUA DOUTOR ',x)
  x <- gsub('AVGOV ','AVENIDA GOVERNADOR ',x)
  x <- gsub('AVPRES ','AVENIDA PRESIDENTE ',x)
  x <- gsub('AVPROF ','AVENIDA PROFESSOR ',x)
  x <- gsub('AVMARQ ','AVENIDA MARQUES ',x)
  x <- gsub('AVSANTO ','AVENIDA SANTO ',x)
  x <- gsub('AVSTO ','AVENIDA SANTO ',x)
  x <- gsub('AVVINTE ','AVENIDA VINTE ',x)
  x <- gsub('RJOSE ','RUA JOSE ',x)
  
  # Padronize bridges' names
  x <- ifelse(stringr::str_detect(x,'PONTE JAGUARE') | stringr::str_detect(x,'HIRANT SANAZAR'),
              'PONTE JAGUARE - HIRANT SANAZAR',x)
  x <- ifelse(stringr::str_detect(x,'PONTE FREGUESIA DO O'),
              'PONTE FREGUESIA DO O',x)
  x <- ifelse(stringr::str_detect(x,'PONTE DOS REMEDIOS') | stringr::str_detect(x,'PONTE CARMEN FERNANDES NEVES'),
              'PONTE DOS REMEDIOS - CARMEN F NEVES',x)
  x <- ifelse(stringr::str_detect(x,'PONTE DO LIMAO') | stringr::str_detect(x,'PONTE LIMAO') | stringr::str_detect(x,'PONTE ADHEMAR F'),
              'PONTE DO LIMAO - ADHEMAR FERREIRA DA SILVA',x)
  x <- ifelse(stringr::str_detect(x,'PONTE DAS BANDEIRAS') | stringr::str_detect(x,'PONTE BANDEIRAS') |stringr::str_detect(x,'PONTE DA BANDEIRAS') | stringr::str_detect(x,'PONTE SENADOR ROMEU TUMA'),
              'PONTE DAS BANDEIRAS - SENADOR ROMEU TUMA',x)
  x <- ifelse(stringr::str_detect(x,'PONTE DA CASA VERDE') | stringr::str_detect(x,'PONTE CASA VERDE') | stringr::str_detect(x,'PONTE JORNALISTA WALTER ABRAHAO'),
              'PONTE DA CASA VERDE - JORNALISTA WALTER ABRAHAO',x)
  x <- ifelse(stringr::str_detect(x,'PONTE CRUZEIRO DO SUL') | stringr::str_detect(x,'PONTE JORNALISTA ARY SILVA'),
              'PONTE CRUZEIRO DO SUL - JORNALISTA ARY SILVA',x)
  x <- ifelse(stringr::str_detect(x,'PONTE CIDADE UNIVERSITARIA') | stringr::str_detect(x,'PONTE PREFEITO WILLIAM SALEM')  | stringr::str_detect(x,'PONTE DA CIDADE UNIVERSITARIA'),
              'PONTE CIDADE UNIVERSITARIA - PREFEIRO WILLIAM SALEM',x)
  x <- ifelse(stringr::str_detect(x,'PONTE DO MORUMBI') | stringr::str_detect(x,'PONTE CAIO POMPEU DE TOLEDO'),
              'PONTE DO MORUMBI - CAIO POMPEU DE TOLEDO',x)
  x <- ifelse(stringr::str_detect(x,'PONTE ARICANDUVA') | stringr::str_detect(x,'PONTE MIGUEL ARRAES') | stringr::str_detect(x,'PONTE DOUTOR MIGUEL ARRAES'),
              'PONTE ARICANDUVA - DOUTOR MIGUEL ARRAES',x)
  x <- ifelse(stringr::str_detect(x,'PONTE DA VILA GUILHERME') | stringr::str_detect(x,'PONTE C W O BAUMGART')| stringr::str_detect(x,'PONTE VILA GUILHERME'),
              'PONTE DA VILA GUILHERME - C W O BAUMGART',x)
  x <- ifelse(stringr::str_detect(x,'PONTE DO PIQUERI') | stringr::str_detect(x,'PONTE JOELMIR BETING'),
              'PONTE DO PIQUERI - JOELMIR BETING',x)
  x <- ifelse(stringr::str_detect(x,'PONTE DO TATUAPE') | stringr::str_detect(x,'PONTE DEPUTADO RICARDO IZAR') | stringr::str_detect(x, 'PONTE TATUAPE'),
              'PONTE DO TATUAPE - DEPUTADO RICARDO IZAR',x)
  
  x <- gsub('ROBERTO ZUCOLLO','ROBERTO ROSSI ZUCOLLO',x)
  x <- ifelse(stringr::str_detect(x,'PONTE EGENHEIRO ARY TORRES'), 'PONTE ENGENHEIRO ARY TORRES',x)
  x <- ifelse(stringr::str_detect(x,'ROBERTO ZUCOLLO'), 'PONTE ENGENHEIRO ROBERTO ROSSI ZUCOLLO',x)
  
  # Padronize Marginais
  x <- ifelse(stringr::str_detect(x,'MARGINAL TIETE'), 'MARGINAL TIETE',x)
  x <- ifelse(stringr::str_detect(x,'MARGINAL PINHEIROS'), 'MARGINAL PINHEIROS',x)
  
  # Padronize street numbers
  x <- gsub('^ALTURA DO NUMERO',"",x)
  x <- gsub('^ALTURA N',"",x)
  x <- gsub('^ALTURA DO N',"",x)
  x <- gsub('^ALTURA ',"",x)
  x <- gsub('^ALT NUMERO',"",x)
  x <- gsub('^ALT N',"",x)
  x <- gsub('^ALT DO N',"",x)
  x <- gsub('^ALT ',"",x)
  
  # Final trim 
  x <- stringr::str_trim(x)
  
  return(x)
}

# Fix and save
floods <- floods[, flood_point := clean_addres(flood_point)]
floods <- floods[, reference := clean_addres(reference)]
floods <- floods[, location := paste(flood_point,reference,sep = ' ')]

readr::write_rds(floods,here::here('data','flood_points_clean.rds'))