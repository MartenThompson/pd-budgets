library(shiny)
library(leaflet)
library(DT)

# Use this R Shiny application to quickly visualize the budget of major cities' police departments.

##################
##### SETUP ######

# all data used in this application
dat <- read.csv("data/city_budgets.csv")

# HTML for the little popups on hover
dat$labels <- lapply(seq(nrow(dat)), function(i) {
  paste0( '<p><strong>', dat[i, "city"], '</strong> (', dat[i, 'year'], ')</p>', 
          '<p>Total Spending: $', format(dat[i, 'tot_pol_spending'], big.mark = ',', trim = TRUE),'</p>', 
          '<p>Per-Capita: $', format(dat[i, 'per_cap'], big.mark=',', trim=TRUE), '</p>',
          '<a href="', dat[i, 'source'], '" target="_blank">Budget Source</a>') 
})

# pretty dataFrame for table
dat_pretty <- data.frame('City' = dat$city,
                         'Year' = dat$year,
                         'Total Spending' = dat$tot_pol_spending,
                         'Per-Capita' = dat$per_cap,
                         check.names = FALSE)

# Dropdown choices for users
displayDDChoices <- list('Per-Capita Cost','Total Budget')
displayYearChoices <- c(2017,2020)

##################
####### UI #######
ui <- fluidPage(
  tags$head(
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type="text/javascript")
  ),
  fluidRow(
    column(4, selectInput(inputId = 'userDisplayChoice', label='Metric', choices = displayDDChoices,
                           selected = displayDDChoices[[1]])),
    column(4, offset=1, selectInput(inputId = 'userYear', label = 'Year', choices = displayYearChoices,
                                     selected = displayYearChoices[2]))
  ),
  tags$br(),
  leafletOutput('mainMap'),
  tags$p(),
  fluidRow(column(12,
                  dataTableOutput('table'))),
  tags$p('Code and data availabe on', tags$a(href='https://github.com/MartenThompson/pd-budgets.git', target='_blank', 'GitHub')),
  HTML('<div data-iframe-height></div>')
)


##################
##### SERVER #####
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
                    color ='red', fillOpacity = 0.5)
  })
  
  output$table <- renderDataTable({
    dt <- datatable(dat_pretty, rownames = FALSE)
    
    formatCurrency(dt, columns=c(3,4), digits = 0)
    }) 
  # Note: if you just pass renderDataTable a dataframe, it will style this table much differently
  # and it contains its own options(...) to play with. Otherwise, you do what I did here and 
  # style everything when you make dt. I'm concerned I won't be able to tweak it as much this way.
}

shinyApp(ui, server)