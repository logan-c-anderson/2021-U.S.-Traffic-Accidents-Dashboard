# ---
# title: "2021 U.S. Traffic Accidents Dashboard"
# author: "Logan Anderson"
# format: Shiny App
# ---

# Deployed at : https://lcanderson.shinyapps.io/United-States-Traffic-Accidents-2021-Dashboard/
# Source code at GitHub: https://github.com/logan-c-anderson/2021-U.S.-Traffic-Accidents-Dashboard.git


# load libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(tidyr)
library(forcats)
library(MASS)
library(shinyjs)
library(DT)
library(plotly)
library(GGally)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(ggcorrplot)
library(gt)

#load data
allData <- read.csv("combined_accident_data.csv")


##Cleaning Data##
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
  "FifthPreCrashEvent", "AccidentType", "Deaths", "DriverDrinking",
  "FirstHarmfulEventVehicleMake", "FirstHarmfulEventVehicleModel",
  "FirstHarmfulEventBodyClass", "GVWRFrom", "GVWRTo", "County", "City",
  "Latitude", "Longitude", "WeatherConditions", "Fatalities"
)

# Rename the variables in allData
allData2 <- allData %>% rename(!!!setNames(names(allData), new_variable_names))

#Hit and Run
allData2$HitAndRun <- ifelse(allData2$HitAndRun == "Yes", 1, 0)
#NumberOfLanes
allData2$NumberOfLanes <- ifelse(allData2$NumberOfLanes == "Seven or more lanes", 7,
                                 ifelse(allData2$NumberOfLanes == "Five lanes", 5,
                                        ifelse(allData2$NumberOfLanes == "Four lanes", 4,
                                               ifelse(allData2$NumberOfLanes == "Three lanes", 3,
                                                      ifelse(allData2$NumberOfLanes == "Two lanes", 2,
                                                             ifelse(allData2$NumberOfLanes == "One lane", 1, NA))))))
#Rollover
allData2$Rollover <- ifelse(allData2$Rollover %in% c("Rollover, Tripped by Object/Vehicle", "Rollover, Untripped", "Rollover, Unknown Type"), 1, 0)
#DriverDrinking
allData2$DriverDrinking <- ifelse(allData2$DriverDrinking == "Yes", 1, 0)
#PreviousDWI
allData2$PreviousDWI <- as.numeric(ifelse(allData2$PreviousDWI %in% c("None", "1", "2", "3", "4", "5", "6"), allData2$PreviousDWI, NA))
#PreviousAccident
allData2$PreviousAccident <- as.numeric(ifelse(allData2$PreviousAccident %in% c("None", "1", "2", "3", "4", "5", "6", "7", "8"), allData2$PreviousAccident, NA))
#PreviousSpeeding
allData2$PreviousSpeeding <- as.numeric(ifelse(allData2$PreviousSpeeding %in% c("None", "1", "2", "3", "4", "5", "6", "7", "8", "9"), allData2$PreviousSpeeding, NA))
#Bus
allData2$BusUse <- ifelse(allData2$BusUse == "Bus", 1, 0)
##Cleaning Data##


#structure of the data
allData2 %>% str()
#summary of the data
allData2 %>% summary()
#first 5 rows
allData2 %>% head()

#Choices for selectInput - without CaseNumber columns
c1 <- allData2 %>% 
  select(-CaseNumber) %>% 
  names()

#Choices for selectInput - limited columns
c2 <- allData2 %>% 
  select(State, Occupants, DayOfWeek, Month, Hour, HitAndRun, BusUse, Fatalities, Deaths, 
         SpeedLimit, Rollover, PreviousAccident, PreviousDWI, PreviousSpeeding) %>% 
  names()
  
#new df
selected_columns <- allData2 %>%
  select(State, Occupants, DayOfWeek, Month, Hour, HitAndRun, BusUse, Fatalities, Deaths,
         SpeedLimit, Rollover, PreviousAccident, PreviousDWI, PreviousSpeeding)

# Group by State and calculate the sum for each selected column
state_sums <- selected_columns %>%
  group_by(State) %>%
  summarise(across(everything(), sum, na.rm = TRUE))


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
title <- tags$a(href='https://lcanderson.shinyapps.io/United-States-Traffic-Accidents-2021-Dashboard/', icon("car", height = '50', width = '50'),'U.S. Accidents Dashboard (2021)')

# Select the variables for correlation analysis
selected_variables <- c("Hour", "Month", "Occupants", "SpeedLimit", "Deaths", "Fatalities")

# Create a new dataframe with selected variables
correlation_data <- allData2[, selected_variables]
correlation_data_numeric <- as.data.frame(sapply(correlation_data, function(x) as.numeric(as.character(x))))



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
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'relation'", selectInput(inputId = "var3", label = "Select the X variable", choices = c2, selected = "SpeedLimit")),
      conditionalPanel("input.sidebar == 'vis' && input.t2 == 'relation'", selectInput(inputId = "var4", label = "Select the Y variable", choices = c2, selected = "Fatalities"))
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
                              h3("United States Traffic Accidents Data Dashboard (2021)", align = "center"),
                              
                              tags$h3("🚦 Welcome to the United States Traffic Accidents Data Dashboard! 🚦", align = "center"),

                              tags$p("Explore and analyze intriguing insights into traffic accidents across the United States in 2021. 
                                     This dashboard is designed for the purpose of using data-driven insights to enhance nationwide understanding of traffic 
                                     incidents, with a focus on raising awareness, improving traffic safety, and ultimately, 
                                     saving lives on the road!"),
                              
                              tags$p("Navigate through the dataset information and visualizations using the sidebar. Customize your 
                                     analysis by selecting specific variables in the prompts, tailoring the data exploration to 
                                     your preferences!"), tags$h1(""),
                              
                              tags$h6("This dataset was provided by Dr. Ge from STAT 442 at South Dakota State University.", align = "center")
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
                              radioButtons(inputId = "fit", label = "Select Smooth Method", choices = c("loess", "lm"), selected = "lm", inline = TRUE),
                              plotlyOutput("scatter"))
              )#tabBox
      ),
      #Third tabItem
      tabItem(tabName = "map",
              box(h1("placeholder"))
      )
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
    p <- state_sums %>% 
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
    p.mat <- cor_pmat(correlation_data_numeric)
    
    corr.plot <- ggcorrplot(
      cor, 
      hc.order = TRUE,
      lab = TRUE,
      outline.color = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
  })
  
  
  #Bar charts - state wise trend
  output$bar <- renderPlotly({
    state_sums %>% 
      plot_ly() %>% 
      add_bars(x=~State, y=~get(input$var2)) %>% 
      layout(title = paste("Statewise View of", input$var2),
             xaxis = list(title = "State"),
             yaxis = list(title = paste(input$var2), "Placeholder"))
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
    state_sums %>% 
      select(State, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
  })
  
  #Rendering table with 5 states with low rates for specific traffic accident attribute
  output$low5 <- renderTable({
    #Top 5 states with low fatality rates
    state_sums %>% 
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
        "FifthPreCrashEvent", "AccidentType", "Deaths", "DriverDrinking",
        "FirstHarmfulEventVehicleMake", "FirstHarmfulEventVehicleModel",
        "FirstHarmfulEventBodyClass", "GVWRFrom", "GVWRTo", "County", "City",
        "Latitude", "Longitude", "WeatherConditions", "Fatalities"
      ),
      Description = c(
        "State where the accident occurred.",
        "A unique identifier for the accident case.",
        "A unique identifier for the vehicle involved in the accident.",
        "Number of occupants in the vehicle.",
        "Day of the week when the accident occurred.",
        "Month when the accident occurred.",
        "Name of the month when the accident occurred.",
        "Hour when the accident occurred.",
        "Name of the minute when the accident occurred.",
        "Type of harmful event.",
        "Manner of collision.",
        "Type of unit involved in the accident.",
        "Whether the accident involved a hit and run.",
        "State of registration for the vehicle.",
        "Owner of the vehicle.",
        "Make of the vehicle.",
        "Make and model of the vehicle.",
        "Body type of the vehicle.",
        "Model year of the vehicle.",
        "Whether a jackknife occurred in the accident.",
        "Configuration of the vehicle.",
        "Type of cargo body.",
        "Indicates if the vehicle is used as a bus.",
        "Speed category of travel.",
        "Whether a rollover occurred.",
        "Location of the first harmful event impact.",
        "Extent of vehicle deformation.",
        "Main harmful event leading to the accident.",
        "Driver's license status.",
        "Driver's height.",
        "Driver's weight.",
        "Whether the driver had a previous accident.",
        "Whether the driver had a previous DWI.",
        "Whether the driver had a previous speeding violation.",
        "Whether the driver had a previous violation other than speeding.",
        "Month of first vehicle registration.",
        "Year of first vehicle registration.",
        "Month of last vehicle registration.",
        "Year of last vehicle registration.",
        "Factor related to speeding.",
        "Type of trafficway.",
        "Number of lanes.",
        "Speed limit.",
        "Name of the speed limit.",
        "Vertical alignment of the roadway.",
        "Vertical profile of the roadway.",
        "Type of pavement.",
        "Surface condition of the roadway.",
        "Condition of the traffic control device.",
        "Function of the traffic control device.",
        "First pre-crash event.",
        "Second pre-crash event.",
        "Third pre-crash event.",
        "Fourth pre-crash event.",
        "Fifth pre-crash event.",
        "Type of accident.",
        "Number of deaths.",
        "Whether the driver was drinking.",
        "Make of the vehicle involved in the first harmful event.",
        "Model of the vehicle involved in the first harmful event.",
        "Body class of the vehicle involved in the first harmful event.",
        "Gross vehicle weight rating from.",
        "Gross vehicle weight rating to.",
        "County where the accident occurred.",
        "City where the accident occurred.",
        "Latitude of the accident location.",
        "Longitude of the accident location.",
        "Weather conditions at the time of the accident.",
        "Number of fatalities in the accident."
      )
    ) %>%
      gt() %>%
      tab_header(title = md("U.S. Traffic Accidents Data Dictionary"),
                 subtitle = md("2021"))
  }) #data dictionary
  
  }




# Run the application
shinyApp(ui = ui, server = server)
