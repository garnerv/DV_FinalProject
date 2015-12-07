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
      menuItem("Scatter Plot", tabName = "scatterplot", icon = icon("th")),
      menuItem("Join", tabName = "JoinedPlot", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "crosstab",
        sliderInput("KPI1", "College Graduation Rate (as %) Low Max:", 
                    min = 1, max = 50,  value = 30),
        sliderInput("KPI2", "College Graduation Rate (as %) Medium Max:", 
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
        plotOutput("distPlot3")),
        
        tabItem(tabName = "JoinedPlot",
            actionButton(inputId = "clicks4",  label = "Click me"),
            plotOutput("distPlot4")
      )
    )
  )
)

  