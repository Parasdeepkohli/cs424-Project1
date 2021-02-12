library(shiny)
library(ggplot2)
library(DT)


function(input, output, session) {

    source("Pre-processing.R")
    source("Add-units.R")
    source("Add-commas.R")
  
    output$VizControl <- renderUI({
      
        if (input$visualization == "Bar plot (amt)"){plotOutput("stackedbar1")}
        else if (input$visualization == "Bar plot (%)"){plotOutput("stackedbar2")}
        else if (input$visualization == "Line chart (amt)"){plotOutput("linechart1")}
        else if (input$visualization == "Line chart (%)"){plotOutput("linechart2")}
        else if (input$visualization == "Raw table"){DT::dataTableOutput("table1")}
        
    })

    observe({
      updateCheckboxGroupInput(
        session, 'lineSources', choices = c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood"),
        selected = if (input$all) c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")
        
      )
    })
    
    output$stackedbar1 <- renderPlot({
        
        ggplot(US_total, aes(fill = ENERGY.SOURCE, y = GENERATION..Megawatthours., x = YEAR)) + 
            geom_bar(position = "stack", stat = "identity", width = 0.6) +
            ggtitle("Amount of energy produced by source in the US") +
            theme(
                text=element_text(size = 15, family = 'sans'), 
                plot.title = element_text(hjust = 0.5, face = 'bold'),
                axis.title = element_text(size = 18),
                legend.text = element_text(size = 16, family = 'sans'),
                legend.background = element_rect(fill = "darkgrey")
                ) +
            labs(fill = "Energy Source") +
            scale_x_continuous(name = "Year", breaks = seq(1990, 2019)) +
            scale_y_continuous(name = "Energy produced (MWh)", labels = addUnits)
        
    })

    output$stackedbar2 <- renderPlot({
        
        ggplot(US_total, aes(fill = ENERGY.SOURCE, y = GENERATION..Megawatthours., x = YEAR)) + 
            geom_bar(position = "fill", stat = "identity", width = 0.6) +
            ggtitle("Percentage of total energy produced by source in the US") +
            theme(
                text=element_text(size = 15, family = 'sans'), 
                plot.title = element_text(hjust = 0.5, face = 'bold'),
                axis.title = element_text(size = 18),
                legend.text = element_text(size = 16, family = 'sans'),
                legend.background = element_rect(fill = "darkgrey")
                ) +
            labs(fill = "Energy Source") +
            scale_x_continuous(name = "Year", breaks = seq(1990, 2019)) +
            scale_y_continuous(name = "Energy produced by share", labels = scales::percent)

    })
    
    output$linechart1 <- renderPlot({
      
        chosen_data <- subset(US_total, ENERGY.SOURCE %in% input$lineSources)
        ggplot(chosen_data, aes(x = YEAR, y = GENERATION..Megawatthours., group = ENERGY.SOURCE)) +
            geom_line(aes(color = ENERGY.SOURCE), size = 1) +
            ggtitle("Amount of energy produced by source in the US by source") +
            theme(
                text=element_text(size = 18, family = 'sans'),
                plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
                legend.text = element_text(size = 18, family = 'sans'),
                legend.background = element_rect(fill = "darkgrey"),
                legend.key.size = unit(2, "line")
                ) +
            labs(color = "Energy Source") +
            scale_x_continuous(name = "Year", breaks = seq(1990, 2019, by = 2)) +
            guides(color = guide_legend(override.aes = list(size = 2))) +
            scale_y_continuous(name = " Average Energy produced, in Millions (MWh)", 
                                labels = addUnits)
        
    })
    
    output$linechart2 <- renderPlot({
      
      chosen_data <- subset(US_total, ENERGY.SOURCE %in% input$lineSources)
      ggplot(chosen_data, aes(x = YEAR, y = Percentages, colour = ENERGY.SOURCE)) +
        geom_line(size = 1) +
        ggtitle("Percentage of energy produced by source in the US by source") +
        theme(
          text=element_text(size = 18, family = 'sans'), 
          plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
          legend.text = element_text(size = 18, family = 'sans'),
          legend.background = element_rect(fill = "darkgrey"),
          legend.key.size = unit(2, "line")
        ) +
        labs(color = "Energy Source") +
        scale_x_continuous(name = "Year", breaks = seq(1990, 2019, by = 2)) +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        scale_y_continuous(name = " Energy produced",  labels = addUnits)
      
    })
    
  output$table1 = DT::renderDataTable(
    class = 'cell-border stripe',
    filter = 'top',
    options = list(columnDefs = list(list(targets = c(3,4), searchable = FALSE, className = 'dt-right'))),
    { 
      US_total$YEAR <- factor(US_total$YEAR)
      US_total$Percentages <- signif(US_total$Percentages, digits = 2)
      US_total$GENERATION..Megawatthours. <- addCommas(US_total$GENERATION..Megawatthours.)
      US_total[, c("YEAR", "ENERGY.SOURCE", "GENERATION..Megawatthours.", "Percentages")]
      
    }, 
    colnames = c("Year", "Energy Source", "Energy generated (MWh)", "Percentage of total energy produced per year (%)")
  )
}
