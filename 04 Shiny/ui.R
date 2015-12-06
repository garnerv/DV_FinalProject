#ui.R

require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Barchart", tabName = "barchart", icon = icon("th")),
      menuItem("Scatter Plot", tabName = "scatterplot", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "crosstab",
        sliderInput("KPI1", "KPI_Low_Max_value:", 
                    min = 1, max = 50,  value = 30),
        sliderInput("KPI2", "KPI_Medium_Max_value:", 
                    min = 50, max = 70,  value = 70),
        textInput(inputId = "title", 
                  label = "Crosstab Title",
                  value = "College Graduation Percentiles by Tuition"),
        actionButton(inputId = "clicks1",  label = "Click me"),
        plotOutput("distPlot1")
      ),
      
      # Second tab content
      tabItem(tabName = "barchart",
        actionButton(inputId = "clicks2",  label = "Click me"),
        plotOutput("distPlot2")
      ),
      
      # Third tab content
      tabItem(tabName = "scatterplot",
        actionButton(inputId = "clicks3",  label = "Click me"),
        plotOutput("distPlot3")
      )
    )
  )
)
  