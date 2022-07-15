# title: 'RShiny Project'
# author: "Kyle Kobayashi"
# date: "October 14th, 2021"


library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(lubridate)
library(rsconnect)
library(chron)
library(wordcloud2)
library(RColorBrewer)

# Initialization of variables and data sets

chicago_crimes <- read.csv("data/clean_chicago_crimes.csv") %>%
                              mutate(time = chron(times = time)) %>%
                              mutate(date = as.Date(date))

month_names <- c("January",
                 "February",
                 "March",
                 "April",
                 "May",
                 "June",
                 "July",
                 "August",
                 "September",
                 "October",
                 "November",
                 "December")


primary_descr <- sort(c(distinct(chicago_crimes, primary_description)$primary_description))

secondary_descr <- NULL


# Defining functions to be used

# returns a data set of Chicago crimes that filters based on user inputs
get_filtered_crimes <- function(input){
  
  # Represents the start time of the 2 slider input
  # Will be used to show crimes that happen after the 'start_time'
  start_time <- chron(times = format(as.POSIXct(input$timerange[1]), format = "%H:%M:%S"))
  
  # Represents the end time of the 2 slider input
  # Will be used to show crimes that happen before the 'end_time'
  end_time <- chron(times = format(as.POSIXct(input$timerange[2]), format = "%H:%M:%S"))
  
  # Filters the chicago_crimes data set based on:
  #   - matching primary description
  #   - matching secondary descriptions
  #   - whether a crime occurs between two times (HH:MM:SS)
  #   - whether a crime occurs between two dates (MM/DD/YYYY)
  filtered_crimes <- chicago_crimes %>%
    filter(primary_description == input$primary,
           secondary_description %in% input$secondary,
           time >= start_time,
           time <= end_time,
           date >= input$daterange[1],
           date <= input$daterange[2])
  
}


# given a date input in the format of YYYY/MM/DD, returns a date in the format of 'Month-Name Day Year' (ex: December 25 2020)
get_month_day_year <- function(date){
  month_name <- month_names[parse_number(format(date, "%m"))]
  year <- format(date, "%Y")
  day <- format(date, "%d")
  
  paste(month_name, day, year, sep=" ")
}

# given a a list of two times, returns a string in the format of HH:MM - HH:MM (ex: 10:00 - 21:00)
get_time_range <- function(timerange){
  start <- format(as.POSIXct(timerange[1]), format = "%H:%M")
  end <- format(as.POSIXct(timerange[2]), format = "%H:%M")
  
  paste("(",start," to ",end,")", sep="")
}

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Chicago Crimes"),
  dashboardSidebar(
    sidebarMenu(
    
      tags$style(HTML(".sidebar-menu li a { font-size: 12px; }")),
      menuItem(
        selectInput("primary",
                    label = "Primary Description",
                    choices = primary_descr)
      ),

      menuItem(
        selectizeInput("secondary",
                       "Secondary Description:",
                       choices = secondary_descr,
                       multiple = TRUE,
                       selected = "Secondary Description",
                       options = list(plugins= list('remove_button')))
      ),

      menuItem(
        dateRangeInput("daterange", "Date range:",
                       start = "2020-09-13",
                       end   = "2021-09-14",
                       min   = "2020-09-13",
                       max   = "2021-09-14",
                       format = "mm/dd/yy")
      ),

      menuItem(
        sliderInput("timerange", "Time Range:",
                    timezone = "CST",
                    min = as.POSIXct("2021-01-01 00:00:00"),
                    max = as.POSIXct("2021-01-01 23:59:59"),
                    value = as.POSIXct(c("2021-01-01 00:00:00", "2021-01-01 23:59:59")),
                    timeFormat="%H:%M:%S",
                    ticks = FALSE,
                    step = 3600)
      )
    )
  ),
  dashboardBody(
    fluidRow(
      tabBox(
        title = NULL, 
        width = 12,
        id = "tabset1", 
        height = "450px",
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Trend", plotOutput("plot")),
        tabPanel("Word Cloud", wordcloud2Output("cloud"))
      )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    observe({
      # The following sections dynamically update the select inputs for secondary descriptions when a primary description is selected
      
      # Creates a tibble of all distinct primary_descriptions and secondary_descriptions pairings
      # After creating the tibble, filters the tibble for primary_descriptions that match the user's input
      filtered_descriptions <- chicago_crimes %>%
        distinct(primary_description, secondary_description) %>%
        filter(primary_description == input$primary)
      
      # Store the unique secondary_descriptions from filtered_descriptions
      secondary_descr <- sort(c(filtered_descriptions$secondary_description))
      
      # Updates the select inputs with a new list of secondary_descriptions
      updateSelectizeInput(session, 
                           "secondary",
                           choices = secondary_descr)
    })
  
  
    # Renders a word cloud to show the proportions of crimes in Chicago
    output$cloud <- renderWordcloud2({
      word_count <- count(chicago_crimes, primary_description)
      
      color_range_number <- length(unique(word_count$n))
      color <- colorRampPalette(brewer.pal(9,"Reds")[3:7])(color_range_number)[factor(word_count$n)]

      wordcloud2(data=word_count, 
                 size=1.2, 
                 minRotation = 0, 
                 maxRotation = 0, 
                 rotateRatio = 1, 
                 color=color)
      
    })
    
    
    
    # Renders a line graph to show the trends of crimes in Chicago
    output$plot <- renderPlot({
      
      filtered_crimes <- get_filtered_crimes(input)
      
      # Needed to specify the order of the dates on the x-axis
      month_order <- c("09/2020",
                       "10/2020",
                       "11/2020",
                       "12/2020",
                       "01/2021",
                       "02/2021",
                       "03/2021",
                       "04/2021",
                       "05/2021",
                       "06/2021",
                       "07/2021",
                       "08/2021",
                       "09/2021")
      
      # includes the number of crimes with the secondary description for each month/year
      secondary_count <- count(filtered_crimes,
                                    secondary_description,
                                    month_year)
      
      
      # Outputs a blank plot if no secondary descriptions are selected
      # Otherwise, plots a linear graph of the number of crimes with the secondary description over a date range
      # Will plot multiple lines if multiple secondary descriptions are selected
      if(length(input$secondary) == 0){
        ggplot(secondary_count) +
          geom_line() +
          labs(
            title = paste("Number of",str_to_title(input$primary),"Crimes in Chicago"),
            subtitle = paste("Between",
                             get_month_day_year(input$daterange[1]),
                             "and",
                             get_month_day_year(input$daterange[2]),
                             get_time_range(input$timerange)),
            x = "Date",
            y = "Number of Incidents"
          )
      }
      else{
        ggplot(secondary_count,
               aes(x = factor(month_year, levels = month_order),
                   y = n,
                   group = secondary_description,
                   color = secondary_description)) +
          geom_line() +
          labs(
            title = paste("Number of",str_to_title(input$primary),"Crimes in Chicago"),
            subtitle = paste("Between",
                             get_month_day_year(input$daterange[1]),
                             "and",
                             get_month_day_year(input$daterange[2]),
                             get_time_range(input$timerange)),
            x = NULL,
            y = "Number of Incidents",
            color = NULL
          ) +
          theme(legend.position = "bottom") +
          guides(color = guide_legend(nrow = 1, byrow = TRUE))
      }
    })
    
    
    # Renders a map of Chicago that will be used to mark the locations of crimes
    output$map <- renderLeaflet({    
      
        filtered_crimes <- get_filtered_crimes(input)
        
        # stores colors that will be used to mark different crimes on the map
        pal <- colorFactor(
          palette = colorRampPalette(rainbow(10))(length(unique(filtered_crimes$secondary_description))),
          domain = NULL)
        
        # creates map of chicago and marks the locations of crimes in the filtered data set
        chicago_map <- leaflet(data = filtered_crimes, height = 2000) %>% 
          setView(lng = -87.623177, lat = 41.881832, zoom = 9.5) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addCircleMarkers(lng = ~longitude, 
                     lat = ~latitude, 
                     radius = 4,
                     color = ~pal(secondary_description),
                     fill = ~pal(secondary_description),
                     label = ~paste("Case #: ",
                                    case_number[1]), 
                     popup = ~paste("<b>",
                                    primary_description,
                                    "</b>",
                                    "<br/>",
                                    secondary_description,
                                    "<br/>",
                                    "<br/>",
                                    "Location: ",
                                    location_description,
                                    "<br/>", 
                                    get_month_day_year(date),
                                    "<br/>",
                                    time,
                                    "<br/>",
                                    "<br/>",
                                    location
                                    )) %>% 
          addLegend("bottomright", pal = pal, values = ~secondary_description,
                    title = "Secondary Description")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
