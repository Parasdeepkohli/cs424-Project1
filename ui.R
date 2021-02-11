library(shiny)

navbarPage(
  # *Input() functions
  # *Output() functions
  title = "Welcome",
  id = "nav",
  position = "static-top",
  collapsible = TRUE,
  selected = "Basic Visualizations",
  tabPanel(title = "About"), # About page to be built here
  
  tabPanel("Basic Visualizations", # First 40% of project in this panel
    sidebarLayout(       
      sidebarPanel(
        selectInput(inputId = "visualization", label = "Visualization", choices = c("Bar plot", "Line chart", "Table"), selected = "Line chart"),
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
          conditionalPanel(
            condition = "input.visualization == 'Bar plot'",
            plotOutput("stackedbar1"),
            plotOutput("stackedbar2")
          ),
        conditionalPanel(
          condition = "input.visualization == 'Line chart'",
          plotOutput('linechart1'),
          plotOutput("linechart2")
        )
      )
    )
  )
)