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


# Define the new variable names
new_variable_names <- c(
  "State", "CaseNumber", "VehicleNumber", "Occupants", "DayOfWeek", "Month",
  "MonthName", "Hour", "MinuteName", "HarmfulEvent", "CollisionManner",
  "UnitType", "HitAndRun", "RegistrationState", "VehicleOwner", "VehicleMake",
  "VehicleMakeModel", "BodyType", "ModelYear", "Jackknife", "VehicleConfiguration",
  "CargoBodyType", "BusUse", "TravelSpeedCategory", "Rollover", "FirstImpactArea",
  "VehicleDeformed", "MainHarmfulEvent", "DriverLicenseStatus", "DriverHeight",
  "DriverWeight", "PreviousAccident", "PreviousDWI", "PreviousSpeeding",
  "PreviousOther", "FirstRegistrationMonth", "FirstRegistrationYear",
  "LastRegistrationMonth", "LastRegistrationYear", "SpeedingRelatedFactor",
  "TrafficwayType", "NumberOfLanes", "SpeedLimit", "SpeedLimitName",
  "VerticalAlignment", "VerticalProfile", "PavementType", "SurfaceCondition",
  "TrafficControlDeviceCondition", "TrafficControlFunction", "FirstPreCrashEvent",
  "SecondPreCrashEvent", "ThirdPreCrashEvent", "FourthPreCrashEvent",
  "FifthPreCrashEvent", "AccidentType", "NumberOfDeaths", "DriverDrinking",
  "FirstHarmfulEventVehicleMake", "FirstHarmfulEventVehicleModel",
  "FirstHarmfulEventBodyClass", "GVWRFrom", "GVWRTo", "County", "City",
  "Latitude", "Longitude", "WeatherConditions", "Fatalities"
)

# Rename the variables in allData
allData2 <- allData %>% rename(!!!setNames(names(allData), new_variable_names))


#structure of the data
allData2 %>% str()
#summary of the data
allData2 %>% summary()
#first 5 rows
allData2 %>% head()

#Choices for selectInput - without states columns
c1 <- allData2 %>% 
  select(-CaseNumber) %>% 
  names()

#Second menuItem visualization
#create histogram and boxplot for distribution panel
p1 <-  allData2 %>%
  plot_ly() %>%
  add_histogram(~NumberOfLanes) %>%
  layout(xaxis = list(title = "NumberOfLanes"))
#Box plot
p2 <-  allData2 %>%
  plot_ly() %>%
  add_boxplot(~NumberOfLanes) %>%
  layout(yaxis = list(showticklabels = F))
#Stacking plots
subplot(p2, p1, nrows = 2, shareX = TRUE) %>% 
  hide_legend() %>% 
  layout(title = "Distribution Chart - Histogram and Boxplot",
         yaxis = list(title = "Frequency"))

#Top 5 states with high fatality rates
allData2 %>% 
  select(State, Fatalities) %>% 
  arrange(desc(Fatalities)) %>% 
  head(5)
#Top 5 states with low fatality rates
allData2 %>% 
  select((State), Fatalities) %>% 
  arrange(Fatalities) %>% 
  head(5)

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
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'distro'", selectInput(inputId = "var1", label = "Select the variable", choices = c1, selected = "NumberOfLanes")),
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'trends'", selectizeInput("var2", label = "Select variable type", choices = c1)),
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'relation'", selectInput(inputId = "var3", label = "Select the X variable", choices = c1, selected = "Fatalities")),
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'relation'", selectInput(inputId = "var4", label = "Select the Y variable", choices = c1, selected = "SpeedLimit")),
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
                       column(width = 4, tags$img(src="https://previews.123rf.com/images/denayunebgt/denayunebgt2204/denayunebgt220400108/184544454-car-accident-background-illustration-with-two-cars-colliding-or-hitting-something-on-the-road.jpg", 
                                                  width = 325, height = 275),
                              tags$br(),
                              tags$a("Photo by: Rini Astiyah on 123rf.com"), align = "center"),
                       column(width = 8, tags$br(),
                              h3("United States Traffic Accidents Data Dashboard (2021)"),
                              tags$p("🎉 Welcome to the United States Traffic Accidents Data Dashboard app! 🎉"),
                              tags$p("This is a dashboard designed for the purpose of analyzing and communicating 
                               traffic accidents in the U.S. with the goals of raising awareness of traffic 
                               accidents in the U.S., improving traffic in the future, and saving lives on the road!"),
                              tags$p("To use this app, use the sidebar to select topic to analyze, manipulate the 
                               widgets within each tab to change the analysis according to your preferences! To 
                               download a high quality image of the plot you've created, you can also download it 
                               with the download button. To see the raw data, use the raw data tab for an 
                               interactive form of the table."),
                              tags$p("This data set was provided by Dr. Ge in STAT 442 at SDSU.")
                       )
                     ) #fluidRow
                     ), #tabPanel
                     tabPanel("Data Dictionary", icon = icon("book"), fluidRow(
                       column(width = 4, tags$br(), tags$h3("United States Traffic Accidents Data Dashboard (2021) Data Dictionary"), 
                       tags$p("These variables will be used throughout the analysis. Feel free to reference
                              back to this page as often as possible."), align = "center"),
                       column(width = 8, gt_output("table_plot"))
                       ) #fluidRow
                              ),
                     tabPanel("Data", icon = icon("table-cells"), dataTableOutput("dataTable")),
                     tabPanel("Structure", icon = icon("landmark"), verbatimTextOutput("structure")),
                     tabPanel("Summary Stats", icon = icon("calculator"), verbatimTextOutput("summary"))
              ), #tabBox
      ), #tabitem
      #Second tab item
      tabItem(tabName = "vis",
              tabBox(id = "t2", width = 12, 
                     tabPanel(title = "Accident Trends Per State", value = "trends", 
                              fluidRow(tags$div(align="center"), box(tableOutput("top5"), title = textOutput("head1"), collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE),
                                       tags$div(align="center"), box(tableOutput("low5"), title = textOutput("head2"), collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE)),
                              plotlyOutput("bar")),
                     tabPanel(title = "Distribution", value = "distro", plotlyOutput("histplot")),
                     tabPanel(title = "Correlation Matrix", plotlyOutput("cor")),
                     tabPanel(title = "Relationship Among Variables", value = "relation", 
                              radioButtons(inputId = "fit", label = "Select smooth method", choices = c("loess", "lm"), selected = "lm", inline = TRUE),
                              plotlyOutput("scatter"))
              )#tabBox
      ),
      #Third tabItem
      tabItem(tabName = "map",
              box(h1("placeholder"))
      ),
      
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
                          choices = unique(allData2$State),
                          multiple = TRUE),                     #For User To Select State
              selectInput("month_select", h3("Select Month"),
                          choices = unique(allData2$MonthName),
                          multiple = TRUE),
              sliderInput("fatality_slider", h3("Select Minimum Fatalities:"),
                          min = 1, max = max(allData2$Fatalities), value = 1, step = 1)
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
  output$structure <- renderPrint(allData2 %>% str())
  
  #Summary of the data
  output$summary <- renderPrint(allData2 %>% summary())
  
  #DataTable
  output$dataTable <- renderDataTable(allData2)
  
  #Stacked histogram and boxplot
  output$histplot <- renderPlotly({
    p1 <-  allData2 %>%
      plot_ly() %>%
      add_histogram(~get(input$var1)) %>%
      layout(xaxis = list(title = input$var1))
    #Box plot
    p2 <-  allData2 %>%
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
  c1 <- allData2 %>% 
    select(-CaseNumber) %>% 
    names()
  
  #Creating scatter plot for relationships using ggplot
  output$scatter <- renderPlotly({
    p <- allData2 %>% 
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
    allData2 %>% 
      plot_ly() %>% 
      add_bars(x=~State, y=~get(input$var2)) %>% 
      layout(title = paste("Statewise blank for", input$var2),
             xaxis = list(title = "State"),
             yaxis = list(title = paste(input$var2), "some more words..."))
  })
  
  #Rendering the boxheader
  output$head1 <- renderText(
    paste("5 States With high rate of", input$var2)
  )
  #Rendering the boxheader
  output$head2 <- renderText(
    paste("5 States With low rate of", input$var2)
  )
  #Rendering table with 5 states with high rates for specific traffic accident attribute
  output$top5 <- renderTable({
    #Top 5 states with high fatality rates
    allData2 %>% 
      select(State, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
  })
  
  #Rendering table with 5 states with low rates for specific traffic accident attribute
  output$low5 <- renderTable({
    #Top 5 states with low fatality rates
    allData2 %>% 
      select((State), input$var2) %>% 
      arrange((get(input$var2))) %>% 
      head(5)
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
        "GICS Sector",
        "GICS Sub Industry",
        "Founded",
        "Symbol",
        "Security",
        "52w high",
        "52w low",
        "%YTD",
        "Market Cap",
        "Beta",
        "EPS",
        "PE"
      ),
      Description = c(
        "Global Industry Classification Standard (GICS) sector to which the company belongs. 
        GICS is a system for categorizing stocks into sectors and industries.",
        "Further classification of the company's industry within the GICS sector.",
        "The year the company was founded.",
        "Stock symbol that uniquely identifies the company's stock on the stock exchange.",
        "The name or description of the financial security (stock) being traded.",
        "The highest stock price the company reached in the last 52 weeks.",
        "The percentage difference between the current stock price and the 52-week low.",
        "Percent change in price from the start of the year.",
        "Market capitalization of the company in billions.",
        "Measure of volatility. It indicates the stock's sensitivity to market movements.",
        "Earnings Per Share.",
        "Price to Earnings Ratio."
      )
    ) %>%
      gt() %>%
      tab_header(title = md("Dashborad Variables"),
                 subtitle = md("S&P 500"))
  }) #data dictionary
  #Tab 1 - Homepage
  
  }




# Run the application
shinyApp(ui = ui, server = server)