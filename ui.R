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
        tags$head(tags$style("#stackedbar1{height:90vh !important;}
                             #stackedbar2{height:90vh !important;}
                             #linechart1{height:90vh !important;}
                             #linechart2{height:90vh !important;}")),
        selectInput(
          inputId = "visualization", 
          label = "Visualization", 
          choices = c("Bar plot (amt)", "Bar plot (%)", "Line chart (amt)", "Line chart (%)", "Raw table"),
          selected = "Raw table"
          ),
        width = 2,
        conditionalPanel(
          condition = "input.visualization == 'Line chart (amt)' || input.visualization == 'Line chart (%)'",
          checkboxGroupInput(
            inputId = "lineSources", 
            label = "Pick the sources", 
            choices = c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")
              ),
          checkboxInput(inputId = 'all', label = 'All', value = TRUE)
            )
          ),
        mainPanel(
          width = 10,
          uiOutput("VizControl")
      )
    )
  )
)
