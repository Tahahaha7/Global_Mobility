# R script for the passport index project


library(sp)
library(sf)
library(shiny)
library(maps)
library(leaflet)
library(rworldxtra)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)


#graph_data <- read.csv("C:/Users/Taha/Documents/passport/graph_data.csv", header = T, stringsAsFactors = T)
#passports_data <- read.csv("C:/Users/Taha/Documents/passport/passports.csv", header = T, stringsAsFactors = T, check.names = FALSE)

graph_data <- read.csv("graph_data.csv", header = T, stringsAsFactors = T)
passports_data <- read.csv("passports.csv", header = T, stringsAsFactors = T, check.names = FALSE)
passports <- data.frame(passports_data[,-1], row.names = passports_data[,1], check.names = FALSE)
passport_matrix <- read.csv("passport_index_matrix.csv", header = T, stringsAsFactors = T, check.names = FALSE)
passport_matrix <- data.frame(passport_matrix[,-1], row.names = passport_matrix[,1], check.names = FALSE)
data("countriesHigh")

ui <- bootstrapPage(theme = shinytheme("darkly"),
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  navbarPage(div(class="header", "Global Citizen"), id="nav", windowTitle='Global Citizen',
             
             tabPanel("World Map", icon = icon("globe", lib="glyphicon"),
                      div(class="outer", tags$head(includeCSS("styles.css")),
  
  addSpinner((leafletOutput(outputId = "map", width = "100%", height = "100%")), spin = "double-bounce", color = "#A65628"),
  
  absolutePanel(id='controls', class = "panel panel-default", fixed = T,
                draggable = T, bottom = 10, right = "auto", left = 50, top = "auto",
                width = '15%', height = "auto",
                
                selectizeInput("source1", h3("Your Passport"), width = '100%',
                            choices = c(as.character(sort(unique(graph_data$source)))), multiple = FALSE,
                            options = list(placeholder = 'Select', onInitialize = I('function() { this.setValue(""); }'))
                            ),
                
                selectizeInput("source2", h3("Other Passports"), width = '100%',
                            choices = c(as.character(sort(unique(graph_data$target)))), multiple = FALSE,
                            options = list(placeholder = 'Select',
                                           onInitialize = I('function() { this.setValue(""); }'))
                            ),
                
                br(), br(),
                
                #actionButton("run", "Click Me"),
                
                h3(uiOutput("num_countries"), align = "center"),
                ),
  
  tags$div(id="cite", 'Data compiled from ', tags$em('Passport Index'))
  )
  ),
            tabPanel('About', icon = icon("cog", lib="glyphicon"), 
                     #div(class="outer", tags$head(includeCSS("styles.css"))),
                     fluidPage(column(includeHTML('readme.html'), width = 8, offset = 2))
                     )
  )
)

server <- function(input, output, session) {
  
  output$num_countries <- renderText({
    paste(
      br("You Can Visit"),
      br(strong(passports[c(input$source1), c(input$source2)])),
      br("Visa Free Countries")
      )
  })
  
  combo <- reactive({
    
    country1 <- graph_data[graph_data$source == input$source1, ]
    country2 <- graph_data[graph_data$source == input$source2, ]
    combo_data <- rbind(country1[country1$status != 'VR', ], country2[country2$status != 'VR', ])
    combo_data <- combo_data[!duplicated(combo_data$target),]
    combo_data <- combo_data[order(combo_data$alpha),]
    return(combo_data)
    
  })
  
  
  output$map <- renderLeaflet({leaflet(countriesHigh, options = leafletOptions(zoomControl = FALSE)) %>%
      
      setView(0, 35, 3) %>%
    
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = T)) %>%

      htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'bottomleft' }).addTo(this)}") %>%
      
      addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = .5, fillColor = '#c44266',
                  data = countriesHigh[countriesHigh$ADM0_A3 == iso.alpha(input$source1, n=3), ]) %>%
      
      addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = .5, fillColor = '#c44266',
                  data = countriesHigh[countriesHigh$ADM0_A3 == iso.alpha(input$source2, n=3), ]) %>%
      
      addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = .1, fillColor = '#cc4c02', 
                  data = countriesHigh[countriesHigh$ADM0_A3 %in% combo()$alpha, ], 
                  popup = sprintf("<center><strong>%s</strong></br>%s</center>", combo()$target, combo()$status) %>% lapply(htmltools::HTML),
                  popupOptions = labelOptions(closeButton = F, closeOnClick = T))
    })
  
  
}
shinyApp(ui, server)