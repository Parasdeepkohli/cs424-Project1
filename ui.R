library(shiny)

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
      tags$u(tags$h3("Getting started:", `style` = "font-weight:bold")),
      tags$ul(tags$li("Please use the navbar at the top of this page to navigate to your desired visualization", `style` = "font-size:20px"),
              tags$li("As the data is quite dense, the visualizations are best viewed fullscreen",`style` = "font-size:20px"),
              tags$li("Most of the visualizations ask for and react to your input. Feel free to tinker with them!", `style` = "font-size:20px")),  
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
