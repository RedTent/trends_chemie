import_meetpunten <- function(meetpuntencsv="meetpunten.csv"){
  require(dplyr)
  require(readr)
  meetpuntendf <- read_csv2(meetpuntencsv,col_types = cols())
  names(meetpuntendf) <- tolower(names(meetpuntendf))
  meetpuntendf <- meetpuntendf %>% rename(X=x,Y=y)
  meetpuntendf
}

import_meetpunten_latlong <- function(meetpuntencsv="meetpunten.csv", X = "X", Y = "Y"){
  require(rgdal)
  require(dplyr)
  meetpuntendf <- import_meetpunten(meetpuntencsv)
  longlat <- meetpuntendf %>% filter(X != 0, Y != 0) %>%  mutate(long = X, lat = Y)
  coordinates(longlat) = ~long+lat
  proj4string(longlat) <- CRS("+init=EPSG:28992")
  longlat <- spTransform(longlat,"+init=EPSG:4326")
  meetpuntendf <- left_join(meetpuntendf, select(as_data_frame(longlat), mp, long, lat), by = "mp")
}

import_parameters <- function(parametercsv='parameters.csv'){
  require(readr)
  parameterdf <-read_csv2(parametercsv, col_types = cols())
  parameterdf
}


df_to_named_list <- function(df, waarden=1, namen=2){
  require(dplyr)
  values <- c(dplyr::select(df, waarden), use.names = FALSE, recursive = TRUE)
  names_values <- c(dplyr::select(df, namen), use.names = FALSE, recursive = TRUE)
  names(values) <- names_values
  values
}

HHSKthema <- function(){
  require(ggplot2)
  hhskgroen <<- "#8dc63f"
  hhskblauw <<- "#0079c2"
  hhskthema <<- theme_light() + 
    theme( plot.title = element_text(color = hhskgroen, face = "bold", hjust = 0.5),
           axis.title = element_text(color = hhskblauw, face = "bold"),
           axis.text = element_text(color = hhskblauw),
           axis.ticks = element_line(color = hhskblauw),
           axis.line.x = element_line(color = hhskblauw, size = 0.5),
           panel.border = element_rect(color = hhskblauw, size = 1),
           panel.grid.major = element_line(color = hhskgroen,linetype = "dotted", size = 0.5),
           panel.grid.minor = element_line(color = hhskgroen,linetype = "dotted", size = 0.5),
           strip.background = element_rect(size = 20, fill = "white"),
           strip.text = element_text(color = hhskgroen, size = 14, face = "bold")
           
    )
}