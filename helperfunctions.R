import_data <- function(datacsv="fys_chem.csv"){
  require(readr)
  require(dplyr)
  require(lubridate)
  #kolomnamen <- c("mp","datum","parnr","par","eenheid","detectiegrens","waarde")
  df <- read_csv2(file=datacsv,col_types=cols(datum=col_date(format="%d-%m-%Y %H:%M:%S")))
  df <- dplyr::filter(df,!is.na(waarde)) # alle metingen moeten een meetwaarde hebben
  #Toevoegen jaren en maanden
  df$jaar <-as.integer(year(df$datum))
  df$maand <- as.integer(month(df$datum))
  
  #info zodat je weet wat je importeert
  print(paste("Laatste meetdatum is",max(df$datum)))
  df
}

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

p_waarde <- function(zstat){
  p <- min(pnorm(zstat),abs(pnorm(zstat)-1))
  p
  
}

ber_zstat <- function(s,var_s){
  zstat <- (s-sign(s))/sqrt(var_s)
}

sen_JT <- function (waarden,datums){ 
  #modified zyp.sen
  
  zyp.slopediff <- function(i, xx, yy, n) (yy[1:(n - i)] - yy[(i + 1):n])/(xx[1:(n - i)] - xx[(i + 1):n])
  
  x <- datums
  y <- waarden
  n <- length(x)
  
  slopes <- unlist(lapply(1:(n - 1), zyp.slopediff, x, y, n))
  sni <- which(is.finite(slopes))
  slope <- median(slopes[sni])
  intercepts <- y - slope * x
  intercept <- median(intercepts)
  res <- list(intercept = intercept, slope= slope)
  return(res)
}

trends <- function(data, seasonal = FALSE){
  require(dplyr)
  library(trend)
  #library(zyp)
  
  if(!seasonal){
  trendtest<- data %>% 
    group_by(mp, parnr, par) %>% 
    mutate(aantal = n()) %>% 
    filter(aantal>2) %>%
    summarise(z_stat = mk.test(waarde)$statistic, trendrichting_mk = sign(z_stat), p_waarde_mk = p_waarde(z_stat)) %>% 
    select(-z_stat)}
  
  
  
  if(seasonal){
  trendtest <- data %>%
    group_by(mp, parnr, par, maand) %>% 
    mutate(aantal = n()) %>% 
    filter(aantal > 2) %>%
    summarise(s_stat =s_score(waarde), var_s = var_s(waarde)) %>% #print() %>% 
    ungroup() %>% 
    group_by(mp,parnr,par) %>% 
    summarise(s_stat_tot = sum(s_stat, na.rm=TRUE), var_s_tot = sum(var_s, na.rm=TRUE), z_stat = ber_zstat(s_stat_tot,var_s_tot), trendrichting_mk = sign(s_stat_tot), p_waarde_mk=p_waarde(z_stat)) %>% 
    select(-s_stat_tot,-var_s_tot,-z_stat)}
  
  trendtest <- trendtest %>% mutate(groep = case_when(trendrichting_mk==1&p_waarde_mk<0.05 ~ "Stijgende trend",
                                                      trendrichting_mk==-1&p_waarde_mk<0.05 ~ "Dalende trend",
                                                      TRUE ~ "Geen trend"))
  trendtest
  
}




s_score <- function (x){
  n <- length(x)
  S <- 0
  for (k in 1:(n - 1)) {
    for (j in k:n) {
      S <- S + sign(x[j] - x[k])
    }
  }
  return(S)
}

var_s <- function (x) {
  t <- table(x)
  n <- length(x)
  tadjs <- sum(t * (t - 1) * (2 * t + 5))
  varS <- (n * (n - 1) * (2 * n + 5) - tadjs)/18
  return(varS)
}

create_app_data <- function(){
library(lubridate)
library(dplyr)  
  
  data <- import_data("data/fys_chem.zip") %>% semi_join(y = filter(meetpuntendf, meetpuntsoort == "Regulier"), by = "mp")
  appdata <- data %>% filter(parnr<100|(parnr>199&parnr<302)|(parnr>999&parnr<2000))
  appdata3 <- appdata %>% mutate(datum = paste(day(datum),month(datum),year(datum),sep="-"))
  appdata3 <- appdata3 %>% mutate(datum = paste0(datum," 0:00:00"))
  write.table(appdata3,"data/fys_chem_app.csv", row.names = FALSE, sep=";", dec=",", na = "", quote=FALSE)

}