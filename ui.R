library(shiny)
library(DT)

navbarPage(
  
  title = "Nav",
  id = "nav",
  position = "static-top",
  collapsible = TRUE,
  selected = "About",
  tabPanel(
    title = "About",
    tags$div(
      tags$h1("Welcome to Project 1 of CS 424!", `style` = "text-align:center"),
      tags$h4("Created by: Parasdeep (Spring 2021)", `style` = "text-align:right"),
      tags$u(tags$h3("Purpose:", `style` = "font-weight:bold")),
      tags$ul(tags$li("Visualize the energy produced by various sources in the U.S.A (Basic Visualizations)", `style` = "font-size:20px"),
              tags$li("Compare the energy produced by various sources between any two states",`style` = "font-size:20px"),
              tags$li("Geographically visualize variance in energy production across the country", `style` = "font-size:20px")),
      tags$u(tags$h3("The Data:", `style` = "font-weight:bold")),
             tags$ul(tags$li("A CSV file detailing the energy produced in the U.S.A from 1990 - 2019", `style` = "font-size:20px"),
                     tags$li("The data divides energy production by the following categories: Year, State, Producer, and source",`style` = "font-size:20px"),
                     tags$li("Please find the link to the data source here:", tags$a(`href` = "https://www.eia.gov/electricity/data/state/", "Source"), `style` = "font-size:20px")),
      tags$u(tags$h3("Guide:", `style` = "font-weight:bold")),
      tags$ul(tags$li("Please use the navbar above to navigate the app", `style` = "font-size:20px"),
              tags$li("Base Visualizations: Full page dedicated to a single visualization",`style` = "font-size:20px"),
              tags$li("Compare: Base visualizations with comparisons between states", `style` = "font-size:20px"),
              tags$li("US map: Spread the comparisons from compare over the map of the U.S.A", `style` = "font-size:20px")) 
          )
          ),
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
          selected = "Bar plot (amt)"
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
  ),
  
  tabPanel( "Compare states",
    
    sidebarLayout(
      
      sidebarPanel(
        width = 2,
        selectInput(
          inputId = "zone1", 
          label = "State 1", 
          choices = c(state.name, "Washington D.C.", "US Total"),
          selected = "US Total"
        ),
        selectInput(
          inputId = "year1", 
          label = "Year for state 1", 
          choices = seq(1990, 2019, by = 1),
          selected = 1990
        ),
        selectInput(
          inputId = "source1", 
          label = "Source for state 1", 
          choices = sort(c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")),
          selected = "Coal"
        ),
    
        tags$div("", `style`= "background-color: grey; width: 100%; height: 20px"), # Just to neatly divide the inputs for states
        
        selectInput(
          inputId = "zone2", 
          label = "State 2", 
          choices = sort(c(state.name, "Washington D.C.", "US Total")),
          selected = "Illinois"
        ),
        selectInput(
          inputId = "year2", 
          label = "Year for state 2", 
          choices = seq(1990, 2019, by = 1),
          selected = 1990
        ),
        selectInput(
          inputId = "source2", 
          label = "Source for state 2", 
          choices = c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood"),
          selected = "Coal"
        )
        
      ),
      mainPanel(
        
        width = 10,
        fluidRow(
          splitLayout(
            cellWidths = c("33%","33%","33%"),
            plotOutput("compare11",height = "300px"), plotOutput("compare12", height = "300px"), div(dataTableOutput("compare13"), 
                                                                                                         style = "font-size:80%")
            )
        ), 
        fluidRow(
          splitLayout(
            cellWidths = c("33%","33%","33%"),
            plotOutput("compare21",height = "300px"), plotOutput("compare22",height = "300px"), div(dataTableOutput("compare23"), 
                                                                                                    style = "font-size:80%")
          )
        )
        
      )
    )
    
  )
)
