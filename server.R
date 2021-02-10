library(shiny)
library(ggplot2)

source("Pre-processing.R")
no_total <- subset(mod_data, mod_data$ENERGY.SOURCE != "Total")
no_total$ENERGY.SOURCE <- as.factor(no_total$ENERGY.SOURCE)

function(input, output) {
    
    output$stackedbar1 <- renderPlot({
        
        ggplot(no_total, aes(fill = ENERGY.SOURCE, y = GENERATION..Megawatthours., x = YEAR)) + 
            geom_bar(position = "stack", stat = "identity", width = 0.6) +
            ggtitle("Amount of energy produced in the US") +
            theme(
                text=element_text(size = 20, family = 'serif'), 
                plot.title = element_text(hjust = 0.5, face = 'bold'),
                legend.background = element_rect(fill = "darkgrey"),
                ) +
            labs(fill = "Energy Source") +
            scale_x_continuous(name = "Year", breaks = seq(1990, 2019)) +
            scale_y_continuous(name = "Energy produced in Millions (MWh)", breaks = seq(0, 1.5e10, by = 3e9), labels = c(seq(0, 15, by = 3)))
            
        
    })

    output$stackedbar2 <- renderPlot({ 
        
        
        ggplot(no_total, aes(fill = ENERGY.SOURCE, y = GENERATION..Megawatthours., x = YEAR)) + 
            geom_bar(position = "fill", stat = "identity", width = 0.6) +
            ggtitle("Percentage of total energy produced in the US") +
            #scale_x_continuous(breaks = seq(1990, 2019, by = 1),1) +
            #scale_y_continuous(breaks = seq(0, 1, by = 0.1),1) +
            theme(
                text=element_text(size = 20, family = 'serif'), 
                plot.title = element_text(hjust = 0.5, face = 'bold'),
                legend.background = element_rect(fill = "darkgrey")
                ) +
            labs(fill = "Energy Source") +
            scale_x_continuous(name = "Year", breaks = seq(1990, 2019)) +
            scale_y_continuous(name = "Energy produced", labels = scales::percent)
    })
    
    
}