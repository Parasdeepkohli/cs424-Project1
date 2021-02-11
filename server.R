library(shiny)
library(ggplot2)


function(input, output, session) {

    source("Pre-processing.R")
  
    output$PlotControl <- renderUI({
      
        if (input$visualization == "Bar plot (amt)"){plotOutput("stackedbar1")}
        else if (input$visualization == "Bar plot (%)"){plotOutput("stackedbar2")}
        else if (input$visualization == "Line chart (amt)"){plotOutput("linechart1")}
        else if (input$visualization == "Line chart (%)"){plotOutput("linechart2")}
        
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
            scale_y_continuous(name = "Energy produced in Millions (MWh)", breaks = seq(0, 4e9, by = 4e8), labels = c(seq(0, 400, by = 40)))
        
    })

    output$stackedbar2 <- renderPlot({
        
        ggplot(US_total, aes(fill = ENERGY.SOURCE, y = GENERATION..Megawatthours., x = YEAR)) + 
            geom_bar(position = "fill", stat = "identity", width = 0.6) +
            ggtitle("Percentage of total energy produced by source in the US") +
            theme(
                text=element_text(size = 15, family = 'sans'), 
                plot.title = element_text(hjust = 0.5, face = 'bold'),
                axis.title = element_text(size = 16),
                legend.text = element_text(size = 18, family = 'sans'),
                legend.background = element_rect(fill = "darkgrey")
                ) +
            labs(fill = "Energy Source") +
            scale_x_continuous(name = "Year", breaks = seq(1990, 2019)) +
            scale_y_continuous(name = "Energy produced", labels = scales::percent)

    })
    
    output$linechart1 <- renderPlot({
        
        ggplot(US_total, aes(x = YEAR, y = GENERATION..Megawatthours., group = ENERGY.SOURCE)) +
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
            scale_y_continuous(name = " Average Energy produced, in Millions (MWh)", breaks = seq(0, 2e9, by = 2e8), labels = seq(0, 200, by = 20))
        
        
    })
    
    output$linechart2 <- renderPlot({
      
      p <- paste(seq(0, 50, by = 5), "%", sep = "")
      ggplot(US_total, aes(x = YEAR, y = Percentages, colour = ENERGY.SOURCE)) +
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
        scale_y_continuous(name = " Energy produced", breaks = seq(0, 50, by = 5), labels = p )
      
      
    })
    
   
}