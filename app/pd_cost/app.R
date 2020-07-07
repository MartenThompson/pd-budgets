#https://populardemocracy.org/news-and-publications/congress-must-divest-billion-dollar-police-budget-and-invest-public-education
# data: https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vTF6KmXAdiHTaOsIZ4ksk5FBJANHdVdKfJjbr4x6AR0SzdFHkdDXV7Knm4rd4Rkgu1zxUUdknEZ3yTl/pubhtml/sheet?headers=false&gid=687000778

# change to cartogram?
# https://www.esri.com/arcgis-blog/products/arcgis-online/mapping/how-to-build-a-cartogram-in-microsoft-office-and-arcgis-online/

library(shiny)
library(leaflet)

# leaflet tutorial: https://rstudio.github.io/leaflet/
# leaflet map providers: https://leaflet-extras.github.io/leaflet-providers/preview/

dat <- read.csv("data/city_budgets.csv")
dat$labels <- lapply(seq(nrow(dat)), function(i) {
  paste0( '<p><strong>', dat[i, "city"], '</strong> (', dat[i, 'year'], ')</p>', 
          '<p>Total Spending: $', format(dat[i, 'tot_pol_spending'], big.mark = ',', trim = TRUE),'</p>', 
          '<p>Per-Capita: $', format(dat[i, 'per_cap'], big.mark=',', trim=TRUE), '</p>',
          '<a href="', dat[i, 'source'], '" target="_blank">Budget Source</a>') 
})



displayDDChoices <- list('Per-Capita Cost','Total Budget')
displayYearChoices <- c(2017,2020)

ui <- fluidPage(
  fluidRow(
    column(4, selectInput(inputId = 'userDisplayChoice', label='Metric', choices = displayDDChoices,
                           selected = displayDDChoices[[1]])),
    column(4, offset=1, selectInput(inputId = 'userYear', label = 'Year', choices = displayYearChoices,
                                     selected = displayYearChoices[2]))
  ),
  tags$br(),
  leafletOutput('mainMap'),
  tags$br(),
  tags$p('Code and data availabe on', tags$a(href='https://github.com/MartenThompson/pd-budgets.git', target='_blank', 'GitHub'))
)

server <- function(input, output, session) {
  
  plot_data <- eventReactive(c(input$userDisplayChoice, input$userYear), {
    # print(input)
    ret_data <- dat[dat$year == input$userYear,]
    
    if (input$userDisplayChoice == 'Total Budget') {
      ret_data$metric <- ret_data$tot_pol_spending * 0.0002 # SCALING
    } else if (input$userDisplayChoice == 'Per-Capita Cost') {
      ret_data$metric <- ret_data$per_cap * 300 # SCALING
    } else {
      ret_data$metric <- NULL 
    }
    
    ret_data
  })
  
  output$mainMap <- renderLeaflet({
    m <- leaflet()

    m <- addProviderTiles(m, provider = providers$Stamen.TonerLite,
                          options = providerTileOptions(minZoom = 2, maxZoom = 7))
    m <- setView(m, lng = -96, lat = 37, zoom = 4)  # initial view
    m <- addCircles(m, data = plot_data(), lat = ~lat_e, lng = ~lon_n, weight = 1, 
                    radius = ~metric, popup = ~lapply(labels, htmltools::HTML), 
                    # label = ~lapply(labels, htmltools::HTML), prefer popup
                    color ='red', fillOpacity = 0.5)
  })
}

shinyApp(ui, server)