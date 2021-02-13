library(shiny)
library(ggplot2)
library(DT)
library(scales)
library(usmap)
library(stringr)

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
              tags$li("Compare: Base visualizations with comparisons between states through filters", `style` = "font-size:20px"),
              tags$li("US map: Inside Compare, change tabs to geographical comparison of data", `style` = "font-size:20px"),
              tags$li("If you receive an error in Compare, it means that no data is available for that combination of filters", `style` = "font-size:20px")),
      tags$u(tags$h3("Known bugs:", `style` = "font-weight:bold")),
      tags$ul(tags$li("Choosing a source/year/state combination that has no corresponding value in the data returns an error", `style` = "font-size:20px"),
              tags$li("If your choices result in a single observation, the y-ticks will be bugged and repeat the same value",`style` = "font-size:20px"),
              tags$li("Both of the second graphs in compare states are 'squished' due to the legend size. X-axis labels become distorted",`style` = "font-size:20px"))
      
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
            choices = c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood"),
              ),
          checkboxInput(inputId = 'all', label = 'All', value = TRUE),
          tags$head(tags$script(src = "message-handler.js")),
          actionButton(inputId = "interest1", label ="Interesting 1!")
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
        conditionalPanel( condition = "input.plotvmap == 'Plots & Tables'",
          selectInput(
            inputId = "zone1", 
            label = "State 1", 
            choices = sort(c(state.name, "Washington D.C.", "US Total")),
            selected = "US Total"
          )
        ),
        selectInput(
          inputId = "year1", 
          label = "Year 1", 
          choices = c("(All)", seq(1990, 2019, by = 1)),
          selected = 1990
        ),
        selectInput(
          inputId = "source1", 
          label = "Source 1", 
          choices = c("(All)", "Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood"),
          selected = "Coal"
        ),
        
        conditionalPanel( condition = "input.plotvmap == 'Plots & Tables'",
          selectInput(
            inputId = "zone2", 
            label = "State 2", 
            choices = sort(c(state.name, "Washington D.C.", "US Total")),
            selected = "Illinois"
          )
        ),
        selectInput(
          inputId = "year2", 
          label = "Year 2", 
          choices = c("(All)", seq(1990, 2019, by = 1)),
          selected = 1990
        ),
        selectInput(
          inputId = "source2", 
          label = "Source 2", 
          choices = c("(All)","Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood"),
          selected = "Coal"
        ),
        conditionalPanel( condition = "input.plotvmap == 'Plots & Tables'",
                          actionButton(inputId = "interest2", "Interesting 2!"),
                          actionButton(inputId = "interest3", "Interesting 3!")
        ),
        
        conditionalPanel( condition = "input.plotvmap != 'Plots & Tables'",
          tags$h5("Note: Grey means no data available"),
          actionButton(inputId = "interest4", "Interesting 4!"),
          actionButton(inputId = "interest5", "Interesting 5!")
        ),
        
      ),
      mainPanel(
        
        width = 10,
        tabsetPanel( id = "plotvmap",
          tabPanel( "Plots & Tables",
            fluidRow(
              splitLayout(
                cellWidths = c("20%","20%","20%", "20%", "20%"),
                plotOutput("compare11",height = "240px"), plotOutput("compare12", height = "240px"), 
                plotOutput("compare13", height = "240px"), plotOutput("compare14", height = "240px"), 
                div(dataTableOutput("compare15"), style = "font-size:60%")
                )
            ), 
            fluidRow(
              splitLayout(
                cellWidths = c("20%","20%","20%", "20%", "20%"),
                plotOutput("compare21",height = "240px"), plotOutput("compare22", height = "240px"), 
                plotOutput("compare23", height = "240px"), plotOutput("compare24", height = "240px"), 
                div(dataTableOutput("compare25"), style = "font-size:60%")
              )
            )
          ), # Tab panel from Tabset ends
          tabPanel( "US map",
                    fluidRow(
                      splitLayout(
                        cellWidths = c("50%", "50%"),
                        plotOutput("map1", height = "300px", width = "600px"),
                        plotOutput("map2", height = "300px", width = "600px")
                      )
            ),
            fluidRow(
              splitLayout(
                cellWidths = c("50%", "50%"),
                plotOutput("map3", height = "300px", width = "600px"),
                plotOutput("map4", height = "300px", width = "600px")
              )
            )
          )
        ) # Tabset panel ends
      )# Main panel from sidebarLayout ends 
    )# SidebarLayout ends
    
  ) # Table panel for Compare ends
)# End of global navbar panel
