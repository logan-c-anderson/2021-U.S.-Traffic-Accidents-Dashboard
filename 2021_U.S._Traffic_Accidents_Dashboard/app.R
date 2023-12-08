# ---
# title: "2021 U.S. Traffic Accidents App"
# author: "Logan Anderson"
# format: Shiny App
# ---

# load libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
# For the world maps
library(maps)
library(leaflet)

# data
allData <- read.csv("combined_accident_data.csv")

grouped <- allData %>%
  group_by(STATENAME, WEATHERNAME) %>%
  summarise(TotalAccidents = n(), .groups = "drop") %>%
  mutate(Accidents_Per_1k = TotalAccidents / 1000)

grouped2 <- allData %>%
  group_by(MAKENAME, WEATHERNAME) %>%
  summarise(TotalAccidents = n(), .groups = "drop") %>%
  mutate(Accidents_Per_1k = TotalAccidents / 1000)

# Define UI for application for Shiny App
ui <- fluidPage(
  theme = shinytheme("superhero"), # Theme
  navbarPage(
    "United States Accidents (2021)",
    
    # App title
    tabPanel(
      "Location",
      sidebarPanel(
        titlePanel("Select State:"),
        tags$h1(""),
        selectInput("state_input",
                    "Select State:",
                    choices = unique(grouped$STATENAME)
        ) # selectInput
      ),
      mainPanel(
        h1("Accident Data By Location"),
        h3("Count of Accidents In Each Type of Weather Condition"),
        plotOutput("bar_plot")
      ),
      sidebarPanel(
        selectInput("select_state", h3("Select State"),
                    choices = unique(grouped$STATENAME),
                    selected = "Alabama"
        ),
      ),
      mainPanel(
        # Map
        titlePanel("Map of Accidents In The U.S."), # title for map
        leafletOutput("AccidentsMap")
      )
    ), # Location, tabpanel
    
    tabPanel(
      "Season",
      sidebarPanel(
        titlePanel("Accidents per Season"),
        tags$h3("Choose Season"),
        radioButtons("radio",
                     label = h3("Radio buttons"),
                     choices = list(
                       "Spring" = "April showers bring May flowers :)",
                       "Summer" = "Good times and tan lines :)",
                       "Fall" = "Happy Fall, Y'all! :)",
                       "Winter" = "Brrrr... Man, why do I live in South Dakota >:("
                     ),
                     selected = "April showers bring May flowers :)"
        ),
        hr(),
        fluidRow(column(3, verbatimTextOutput("value")))
      ),
      mainPanel(
        h2("I don't want to add another bar plot..."),
        img(src = "https://i.redd.it/zdcwrd8teb041.png", width = "431px"),
        p("Enjoy this meme instead. :)")
      ),
    ), # Season, tabpanel
    
    tabPanel(
      "Vehicle",
      sidebarPanel(
        titlePanel("Accidents Per Make of Car"),
        tags$h3("Select Make of Car:"),
        radioButtons("Makes",
                     "",
                     choices = unique(grouped2$MAKENAME)
        ) # radioButtons
      ), # sideBarPanel
      mainPanel(
        h1("Accident Data By Make of Car"),
        h3("Count of Accidents For Each Make"),
        plotOutput("vehicle_plot")
      ), # mainPanel
    ) # Vehicle, tabpanel
  ) # NavbarPage
) # fluid Page




# Define server logic required for Shiny App
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "select_state", choices = unique(grouped$STATENAME), selected = input$state_input)
  })
  
  
  # Location Bar plot
  output$bar_plot <- renderPlot({
    filtered_data <- reactive({
      filter_data <- grouped
      if (input$state_input != "All") {
        filter_data <- filter_data %>% filter(STATENAME == input$state_input)
      }
      return(filter_data)
    })
    
    ggplot(filtered_data(), aes(x = WEATHERNAME, y = TotalAccidents)) +
      geom_bar(stat = "identity", fill = "cyan4") +
      labs(
        title = input$state_input,
        x = "Weather Conditions",
        y = "Total Accidents"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 30), # title size
        axis.title.x = element_text(size = 18), # X-axis label size
        axis.title.y = element_text(size = 18), # Y-axis label size
        axis.text = element_text(size = 18) # X-axis/y-axis tick label size
      )
  }) # renderPlot for Bar Plot
  # Location Bar Plot
  
  # Map
  output$AccidentsMap <- renderLeaflet({
    leaflet(allData %>%
              dplyr::filter(
                STATENAME == input$select_state
              )) %>%
      addTiles() %>%
      addMarkers(lat = ~LATITUDE, lng = ~LONGITUD)
  }) # renderLeaflet
  # Map
  
  # Seasons Radio Buttons
  output$value <- renderPrint({
    input$radio
    radio <- switch(input$radio,
                    "Spring" = "April showers bring May flowers :)",
                    "Summer" = "Good times and tan lines :)",
                    "Fall" = "Happy Fall, Y'all! :)",
                    "Winter" = "Brrrr... Man, why do I live in South Dakota >:(",
                    "Spring"
    ) # switch
  })
  # Seasons Radio Buttons
  
  # Vehicles Radio Buttons
  output$vehicle_plot <- renderPlot({
    selected_make <- input$Makes
    
    filtered_data <- reactive({
      filter_data <- grouped2
      if (!is.null(selected_make)) {
        filter_data <- filter_data %>% filter(MAKENAME == selected_make)
      }
      return(filter_data)
    })
    
    ggplot(filtered_data(), aes(x = WEATHERNAME, y = TotalAccidents)) +
      geom_bar(stat = "identity", fill = "hotpink3") +
      labs(
        title = paste("Selected Make:", selected_make),
        x = "Weather Conditions",
        y = "Total Accidents"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20), # Title size
        axis.title.x = element_text(size = 18), # X-axis label size
        axis.title.y = element_text(size = 18), # Y-axis label size
        axis.text = element_text(size = 18) # X-axis/y-axis tick label size
      )
  })
  
  # Vehicles Radio Buttons
} # Function




# Run the application
shinyApp(ui = ui, server = server)
