library(shiny)
library(ggplot2)
ui <- fluidPage(
    # *Input() functions
    # *Output() functions
    
    plotOutput("hist")
)

server <- function(input, output) {
    
    
    output$hist <- renderPlot({ 
        
        no_total <- subset(mod_data_4, mod_data_4$ENERGY.SOURCE != "Total")
        no_total$ENERGY.SOURCE <- as.factor(no_total$ENERGY.SOURCE)
        ggplot(no_total, aes(fill = ENERGY.SOURCE, y = GENERATION..Megawatthours., x = YEAR)) + 
            geom_bar(position = "fill", stat = "identity", width = 0.5) +
            ggtitle("Percentage of total energy produced per energy source") +
            xlab("Year") +
            ylab("Percentage (%)") +
            scale_x_continuous(breaks = seq(1990, 2019, by = 1),1) +
            scale_y_continuous(breaks = seq(0, 1, by = 0.1),1)
    })
    
    
}

shinyApp(ui = ui, server = server)