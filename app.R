
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
library(compiler)

#enableJIT(3)


HHSKthema()
waterschapsgrens <- readOGR(dsn='data/shape/wsgrens2.shp', stringsAsFactors = FALSE)
meetpuntendf <- import_meetpunten("data/meetpunten2.csv")
parameterdf <- import_parameters("data/parameters.csv")
data <- import_data("data/fys_chem.zip")

par_choice <- parameterdf %>%  filter(parnr<100|(parnr>199&parnr<302)|(parnr>999&parnr<2000)) %>% df_to_named_list(waarden = 1, namen = 3)


# UI
ui <- fluidPage(theme = "shiny_ORIG_JT.css",
   
   
   titlePanel(title = p(img(src = "logo website.png", id="HHSK_logo", height=80), "TRENDS IN CHEMIE", align="center",style="padding-bottom:40px"), windowTitle = "HHSK - Ontwikkeling soorten"),
   
   
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "par_sel", label = "Kies parameter", choices = par_choice),
        sliderInput(inputId = "periode", label = "Kies periode", value = c(2007,2017), step = 1, min = 1963, max =2017, round = TRUE, sep = "", animate = TRUE),
        tableOutput("tabel")
      ), # end of side bar
      
      mainPanel(
        leafletOutput("kaart", height = 800)
      )# end of main panel
   )
) # end of UI

# SERVER
server <- function(input, output, session) {
  
  mp_trend <- reactive({
    data_sel <- data %>% filter(parnr == input$par_sel, jaar >= input$periode[1], jaar <= input$periode[2])
    mp <- meetpuntendf %>% select(mp,lat,long)
    seasonal <- ifelse(input$par_sel<100,TRUE,FALSE)
    trends_mp <- trends(data_sel,seasonal=seasonal) %>% left_join(mp, by="mp")
    print(filter(trends_mp, mp=="00067"))
    trends_mp
  }) 
  
  #kaart 
  output$kaart <- renderLeaflet({
    leaflet() %>% addTiles() %>% addPolylines(data = waterschapsgrens)})
   
  observe({
    pal <- colorFactor(c("red","grey","blue"),levels=c("Stijgende trend","Geen trend","Dalende trend"))
    leafletProxy("kaart", session) %>% clearMarkers() %>% 
    addCircleMarkers(data=mp_trend(),group=~groep, opacity=0, fillOpacity = 1, fillColor=~pal(groep), radius = 6) 
    }) 
  
  #data_sel <- reactive({data_sel <- data %>% filter(parnr == input$par_sel, jaar >= input$periode[1], jaar <= input$periode[2])})
  
  output$tabel <- renderTable({mp_trend() %>% group_by(groep) %>% summarise(aantal = n())})
  
   
} # end of server



# Run the application 
shinyApp(ui = ui, server = server)

