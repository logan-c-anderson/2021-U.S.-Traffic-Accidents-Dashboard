# ---
# title: "2021 U.S. Traffic Accidents Dashboard"
# author: "Logan Anderson"
# format: Shiny App
# ---

# Deployed at :
# Source code at GitHub: https://github.com/logan-c-anderson/2021-U.S.-Traffic-Accidents-Dashboard.git


# load libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
# For the world maps
library(maps)
library(leaflet)

library(shinydashboard)
library(shiny)
library(gt)
library(dplyr)
library(readxl)
library(GGally)
library(ggridges)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(forcats)
library(MASS)
library(DT)
library(plotly)
library(RColorBrewer)
library(shinyjs)

#load data
allData <- read.csv("combined_accident_data.csv")

#Add Icon/Logo along with the title in the header
title <- tags$a(href='https://www.nhtsa.gov/',
                icon("car", height = '50', width = '50'),'U.S. Accidents Dashboard (2021)')


# Define UI for application for Shiny App
# Define UI for the dashboard application
ui <- dashboardPage(
  dashboardHeader(title = title, titleWidth = 400, #define dashboard header
                  tags$li(class="dropdown", tags$a(href="https://www.strava.com/athletes/42866350", icon("strava"), "My Account", target="_blank")), #Youtube
                  tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/logan-anderson-18191923a/", icon("linkedin"), "My Profile", target="_blank")), #LinkedIn
                  tags$li(class="dropdown", tags$a(href="https://github.com/logan-c-anderson/2021-U.S.-Traffic-Accidents-Dashboard", icon("github"), "Source Code", target="_blank")) #GitHub
  ),
  dashboardSidebar(
    #Set tabs for sidebar
    sidebarMenu(
      menuItem("Homepage", tabName = "tab1"),
      menuItem("Location", tabName = "tab2"),
      menuItem("Season", tabName = "tab3")#,
      # menuItem("EPS Per Market Cap", tabName = "tab4")
    ) #sideBarMenu
  ),  #define dashboard side bar
  
  #body of tabs
  dashboardBody(
    includeCSS("custom1.css"),
    tags$li(class = "nav-item", tags$a(class = "nav-link", href = "", "Download 2021 U.S.Accident Data"),
            style = "margin-right: 30px;",
    ),
    tabItems(
      #Tab 1 - Home
      tabItem(tabName = "tab1",
              h3("United States Traffic Accidents Data Dashboard (2021)"),
              p("🎉 Welcome to the United States Traffic Accidents Data Dashboard app! 🎉"),
              p("This is a dashboard designed for the purpose of analyzing and communicating traffic accidents in the U.S. with the goal of
              improving traffic in the future and saving lives on the road!"),
              p("To use this app, use the sidebar to select topic to analyze, manipulate the widgets within each tab to change the
                analysis according to your preferences! To download a high
                quality image of the plot you've created, you can also download it 
                with the download button. To see the raw data, use the raw data tab 
                for an interactive form of the table. The data dictionary is as follows: "),
              p(""),
              p(""),
              h3("U.S. Traffic Accidents Dashboard Data Dictionary"),
              p("These variables will be used throughout the analysis. Feel free to reference back to this page as often as possible."),
              tabsetPanel(
                tabPanel(
                  fluidPage(gt_output("table_plot"))
                ) #tabsPanel
              ) #tabsetPanel
              ), #Tab - Homepage
      #Tab 1 -Homepage
      
      #Tab 2 - Location
      tabItem(
        tabName = "tab2",
        h3("U.S. Traffic Accidents Location Data"),
        p("Locations"),
        fluidPage(
          titlePanel("Interactive United States Map"),
            sidebarLayout(
              sidebarPanel(
                selectInput("state_select", h3("Select State:"),
                            choices = unique(allData$STATENAME),
                            multiple = TRUE),                     #For User To Select State
                selectInput("month_select", h3("Select Month"),
                            choices = unique(allData$MONTHNAME),
                            multiple = TRUE),
                sliderInput("fatality_slider", h3("Select Minimum Fatalities:"),
                            min = 1, max = max(allData$FATALS), value = 1, step = 1)
                ), #SidebarPanel
              mainPanel(p("Hello World"))
              ) #sideBarLayout
          ) #fluidPage
        ), #tabItem
      #Tab 2 - Location
      
      #Tab 3 - Distributions
      tabItem(
        tabName = "tab3",
        h3("Distributions"),
        tabsetPanel(
          tabPanel(
            titlePanel("Configure Ridgeline"),
            tags$h1(""),
          ) #tabPanel
        ), #tabSetPanel
      ) #tabItem
    ), #tabItems
  ) #dashboardBody
) #dashboardPage






server <- function(input, output, session) {
  
  # observe({
  #   updateSelectInput(session, "select_state", choices = unique(grouped$STATENAME), selected = input$state_input)
  # }) #Location Pre-set
  # 
  #Tab 1 - Homepage
  #data dictionary
  output$table_plot <- render_gt({
    data.frame(
      Variables = c(
        "Fatality",
        "State Name",
        "County Name",
        "Month"
      ),
      Description = c(
        "Whether the accident was fatal or not...",
        "The name of the state in which the accident occured...",
        "The name of the county in which the accident occured...",
        "The name month in which the accident occured..."
      )
    ) %>%
      gt() %>%
      tab_header(title = md("Dashborad Variables"),
                 subtitle = md("S&P 500"))
  }) #data dictionary
  #Tab 1 - Homepage
  

  # Tab 2 - Location
  #map
  output$map <- renderLeaflet({
    #Filter data - State
    filtered_data <- if (any(input$state_select == "All")) {allData} 
    else {
      allData %>%
        filter(STATENAME %in% input$state_select)
    }
    
    # Filter data - Month
    if (any(input$month_select == "All")) {
      filtered_data <- allData
    } else {
      filtered_data <- allData %>%
        filter(MONTHNAME %in% input$month_select)
    }
    
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = allData,
                 lat = ~LATITUDE,
                 lng = ~LONGITUD,
                 popup = ~paste("Fatalities: ", FATALS, "<br>Month: ", MONTHNAME, "<br>Day: ", DAYNAME))
    })
    
  observe({
    leafletProxy("map") %>%
      removeShape("rectangle") %>%
      addRectangles(
        lng1 = -125,  # Minimum longitude for the continental U.S.
        lat1 = 24,    # Minimum latitude for the continental U.S.
        lng2 = -65,   # Maximum longitude for the continental U.S.
        lat2 = 49,    # Maximum latitude for the continental U.S.
        color = "red",   # Border color
        weight = 2,      # Border width
        fill = FALSE     # No fill
      )
  })
  

    # Tab 2 - Location
  
  
  #Tab 3 - Season
  #code code
  #code
  # code
  #Tab 3 - Season
  
  
}




# Run the application
shinyApp(ui = ui, server = server)

