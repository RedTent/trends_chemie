
source("helperfunctions.R")

library(shiny)
library(dplyr)
library(leaflet)
library(lubridate)
library(readr)
library(rgdal)
library(zyp)
library(trend)
library(ggplot2)


HHSKthema()
waterschapsgrens <- readOGR(dsn='data/shape/wsgrens2.shp', stringsAsFactors = FALSE)
meetpuntendf <- import_meetpunten("data/meetpunten2.csv")
parameterdf <- import_parameters("data/parameters.csv")

par_choice <- parameterdf %>%  filter(parnr<100|(parnr>199&parnr<302)|(parnr>999&parnr<2000)) %>% df_to_named_list(waarden = 1, namen = 3)


# UI
ui <- fluidPage(theme = "shiny_ORIG_JT.css",
   
   
   titlePanel(title = p(img(src = "logo website.png", id="HHSK_logo", height=80), "TRENDS IN CHEMIE", align="center",style="padding-bottom:40px"), windowTitle = "HHSK - Ontwikkeling soorten"),
   
   
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "par_sel", label = "Kies parameter", choices = par_choice),
        sliderInput(inputId = "periode", label = "Kies periode", value = c(1963,2017), step = 1, min = 1963, max =2017, round = TRUE, sep = "")
      ), # end of side bar
      
      mainPanel(
        leafletOutput("kaart", height = 600)
      )# end of main panel
   )
) # end of UI

# SERVER
server <- function(input, output, session) {
   
  output$kaart <- renderLeaflet({
    leaflet() %>% addTiles() %>% addPolylines(data = waterschapsgrens)})
   
   
} # end of server



# Run the application 
shinyApp(ui = ui, server = server)

