### fun.R: Some usefull functions
### 

`%nin%` = Negate(`%in%`)

`%nlike%` = Negate(`%like%`)

# Functions to transform df with coords to sf object

to_spatial <- function(df1, coordenada = c("lon", "lat")) {
  x <- st_as_sf(df1, coords = coordenada, crs = 4326)
}

# Remove common accents in portuguese
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

#' Transforms a `sf` objects of points to coordinates lon and lat
#' 
#' `sfc_as_cols` returns a data.frame with lat and lon columns from a `sf` object
#' 
#' Adapted from https://github.com/r-spatial/sf/issues/231
#' 
#' @param x A `sf` object with points 
#' @param names A vector with desired output lon and lat

sfc_as_cols <- function(x, names = c("lon","lat")) {
  
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))                                                                                                                                                                                                                                                     
  
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  ui <- dplyr::bind_cols(x,ret)
  
  st_set_geometry(ui, NULL)
}

# Function to switch between git remotes in a project

change_remotes <- function(remote, branch = "master") {
  
  command <- sprintf("git branch --set-upstream-to=%s/%s", remote, branch)
  
  shell(command)
  
}

## Function to clean and dissolve the borders of polygons by groups
dissolve_polygons <- function(mysf, group_column){
  
  
  # a) make sure we have valid geometries
  temp_sf <- sf::st_make_valid(mysf)
  temp_sf <- temp_sf %>% st_buffer(0)
  
  # b) make sure we have sf MULTIPOLYGON
  temp_sf1 <- temp_sf %>% st_cast("MULTIPOLYGON")
  
  # c) long but complete dissolve function
  dissolvefun <- function(grp){
    
    # c.1) subset region
    temp_region <- subset(mysf, get(group_column, mysf)== grp )
    
    
    # c.2) create attribute with the number of points each polygon has
    points_in_each_polygon = sapply(1:dim(temp_region)[1], function(i)
      length(st_coordinates(temp_region$geom[i])))
    
    temp_region$points_in_each_polygon <- points_in_each_polygon
    mypols <- subset(temp_region, points_in_each_polygon > 0)
    
    # d) convert to sp
    sf_regiona <- mypols %>% as("Spatial")
    sf_regiona <- rgeos::gBuffer(sf_regiona, byid=TRUE, width=0) # correct eventual topology issues
    
    # c) dissolve borders to create country file
    result <- maptools::unionSpatialPolygons(sf_regiona, rep(TRUE, nrow(sf_regiona@data))) # dissolve
    
    
    # d) get rid of holes
    outerRings = Filter(function(f){f@ringDir==1},result@polygons[[1]]@Polygons)
    outerBounds = sp::SpatialPolygons(list(sp::Polygons(outerRings,ID=1)))
    
    # e) convert back to sf data
    outerBounds <- st_as_sf(outerBounds)
    outerBounds <- st_set_crs(outerBounds, st_crs(mysf))
    st_crs(outerBounds) <- st_crs(mysf)
    
    # retrieve code_region info and reorder columns
    outerBounds <- dplyr::mutate(outerBounds, group_column = grp)
    outerBounds <- dplyr::select(outerBounds, group_column, geometry)
    names(outerBounds)[1] <- group_column
    return(outerBounds)
  }
  
  
  # Apply sub-function
  groups_sf <- lapply(X = unique(get(group_column, mysf)), FUN = dissolvefun )
  
  # rbind results
  temp_sf <- do.call('rbind', groups_sf)
  return(temp_sf)
}

