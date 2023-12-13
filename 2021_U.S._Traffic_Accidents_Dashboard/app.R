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
library(DT)
library(ggtext)
library(ggcorrplot)

#load data
allData <- read.csv("combined_accident_data.csv")

#structure of the data
allData %>% str()
#summary of the data
allData %>% summary()
#first 5 rows
allData %>% head()

#Second menuItem visualization
#create histogram and boxplot for distribution panel
p1 <-  allData %>%
  plot_ly() %>%
  add_histogram(~VNUM_LANNAME) %>%
  layout(xaxis = list(title = "VNUM_LANNAME"))
#Box plot
p2 <-  allData %>%
  plot_ly() %>%
  add_boxplot(~VNUM_LANNAME) %>%
  layout(yaxis = list(showticklabels = F))
#Stacking plots
subplot(p2, p1, nrows = 2, shareX = TRUE) %>% 
hide_legend() %>% 
  layout(title = "Distribution Chart - Histogram and Boxplot",
         yaxis = list(title = "Frequency"))


#Add Icon/Logo along with the title in the header
title <- tags$a(href='https://www.google.com',
                icon("car", height = '50', width = '50'),'U.S. Accidents Dashboard (2021)')


# Define UI for application for Shiny App
# Define UI for the dashboard application
ui <- dashboardPage(
  dashboardHeader(title = title, titleWidth = 400, #define dashboard header
                  tags$li(class="dropdown", tags$a(href="https://www.strava.com/athletes/42866350", icon("strava"), "Strava", target="_blank")), #Strava
                  tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/logan-anderson-18191923a/", icon("linkedin"), "My Profile", target="_blank")), #LinkedIn
                  tags$li(class="dropdown", tags$a(href="https://github.com/logan-c-anderson/2021-U.S.-Traffic-Accidents-Dashboard", icon("github"), "Source Code", target="_blank")) #GitHub
  ),
  dashboardSidebar(
    #Set tabs for sidebar
    sidebarMenu(
      id = "sidebar",
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem(text = "Visualizations", tabName = "vis", icon = icon("chart-line")),
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'distro'", selectInput(inputId = "var1", label = "Select the variable", choices = c1, selected = "VNUM_LANNAME")),
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'trends'", selectizeInput("var2", label = "Select variable type", choices = c1)),
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'relation'", selectInput(inputId = "var3", label = "Select the X variable", choices = c1, selected = "FATALS")),
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'relation'", selectInput(inputId = "var4", label = "Select the Y variable", choices = c1, selected = "VSPD_LIM")),
      menuItem(text = "Chloropleth Map", tabName = "map", icon = icon("map")),
      menuItem("Homepage", tabName = "tab1"),
      menuItem("Location", tabName = "tab2"),
      menuItem("Season", tabName = "tab3")#,
      # menuItem("EPS Per Market Cap", tabName = "tab4")
    ) #sideBarMenu
  ),  #define dashboard side bar
  
  #body of tabs
  dashboardBody(
    includeCSS("custom1.css"),
    tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www.nhtsa.gov/", "NHTSA Website"),
            style = "margin-right: 30px;",
    ),
    tabItems(
      #First tab item
      tabItem(tabName = "data",
              #tab box
              tabBox(id="t1", width = 12,
                     tabPanel("About", icon = icon("address-card"), fluidRow(
                       column(width = 8, tags$img(src="https://previews.123rf.com/images/denayunebgt/denayunebgt2204/denayunebgt220400108/184544454-car-accident-background-illustration-with-two-cars-colliding-or-hitting-something-on-the-road.jpg", 
                                                  width = 450, height = 300),
                              tags$br(),
                              tags$a("Photo by: Rini Astiyah on 123rf.com"), align = "center"),
                       column(width = 4, tags$br(),
                              tags$p("This data set was provided by Dr. Ge in STAT 442 at SDSU.")
                              )
                     ) #fluidRow
                     ), #tabPanel
                     tabPanel("Data", icon = icon("address-card"), dataTableOutput("dataTable")),
                     tabPanel("Structure", icon = icon("address-card"), verbatimTextOutput("structure")),
                     tabPanel("Summary Stats", icon = icon("address-card"), verbatimTextOutput("summary"))
                     ), #tabBox
              ), #tabitem
      #Second tab item
      tabItem(tabName = "vis",
              tabBox(id = "t2", width = 12, 
                     tabPanel(title = "Some Trends Per State", value = "trends", plotlyOutput("bar")),
                     tabPanel(title = "Distribution", value = "distro", plotlyOutput("histplot")),
                     tabPanel(title = "Correlation Matrix", plotlyOutput("cor")),
                     tabPanel(title = "Relationship among var 1 & var 2", value = "relation", 
                              radioButtons(inputId = "fit", label = "Select smooth method", choices = c("loess", "lm"), selected = "lm", inline = TRUE),
                              plotlyOutput("scatter"))
                     )#tabBox
                     ),
                    #Third tabItem
      tabItem(tabName = "map",
              box(h1("placeholder"))
              ),
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
  
  #Structure of the data
  output$structure <- renderPrint(allData %>% str())
  
  #Summary of the data
  output$summary <- renderPrint(allData %>% summary())
  
  #DataTable
  output$dataTable <- renderDataTable(allData)
  
  #Stacked histogram and boxplot
  output$histplot <- renderPlotly({
    p1 <-  allData %>%
      plot_ly() %>%
      add_histogram(~get(input$var1)) %>%
      layout(xaxis = list(title = input$var1))
    #Box plot
    p2 <-  allData %>%
      plot_ly() %>%
      add_boxplot(~get(input$var1)) %>%
      layout(yaxis = list(showticklabels = F))
    #Stacking plots
    subplot(p2, p1, nrows = 2, shareX = TRUE) %>% 
    hide_legend() %>% 
      layout(title = "Distribution Chart - Histogram and Boxplot",
             yaxis = list(title = "Frequency"))
  })
  
  #Choices for selectInput - without states columns
  c1 <- allData %>% 
    select(-ST_CASE) %>% 
    names()
  
#Creating scatter plot for relationships using ggplot
  output$scatter <- renderPlotly({
p <- allData %>% 
    ggplot(aes(x=get(input$var3), y=get(input$var4))) + 
    geom_point() +
    geom_smooth(method=get(input$fit)) +
    labs(title = paste("Relationship Between", input$var3, "and", input$var4),
         x = input$var3,
         y = input$var4) + 
    theme(plot.title = element_textbox_simple(size = 10,
                                              halign = 0.5))
  ggplotly(p)
  })
  
  output$cor <- renderPlotly({
    #Compute a matrix of p-values
    p.mat <- cor_pmat(my_df)
    
    corr.plot <- ggcorrplot(
      corr, 
      hc.order = TRUE,
      lab = TRUE,
      outline.color = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
  })
  
  
  #Bar charts - state wise trend
  output$bar <- renderPlotly({
    allData %>% 
      plot_ly() %>% 
      add_bars(x=~STATENAME, y=~get(input$var2)) %>% 
      layout(title = paste("Statewise blank for", input$var2),
             xaxis = list(title = "State"),
             yaxis = list(title = paste(input$var2), "some more words..."))
  })
  
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

