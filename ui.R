library(shiny)

navbarPage(
  # *Input() functions
  # *Output() functions
  title = "Welcome",
  id = "nav",
  position = "static-top",
  collapsible = TRUE,
  selected = "Basic Visualizations",
  tabPanel(title = "About"),
  
  tabPanel("Basic Visualizations",
    sidebarLayout(       
      sidebarPanel(
        selectInput(inputId = "visualization", label = "Visualization", choices = c("Bar plot", "Line chart", "Table")),
        width = 2,
        conditionalPanel(
          condition = "input.visualization == 'Line chart'",
          checkboxGroupInput(
            inputId = "lineSources", 
            label = "Pick the sources", 
            choices = c( "Coal", "Geo Thermal", "Hydro", "Natural Gas", "Nuclear", "Petrol", "Solar","Wind", "Wood")
            )
          )
        ),
      mainPanel(
        width = 10,
        plotOutput("chart1"),
        plotOutput("chart2"),
      )
    )
  )
  

)