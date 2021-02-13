library(shiny)
library(ggplot2)
library(DT)
library(scales)
library(usmap)
library(stringr)


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
                text=element_text(size = 14, family = 'sans'), 
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
                text=element_text(size = 14, family = 'sans'), 
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
            scale_y_continuous(name = " Average Energy produced (MWh)", 
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
        scale_y_continuous(name = " Energy share of total",  labels = addUnits)
      
    })
    
  output$table1 <- DT::renderDataTable(
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
  
  
  # Block for part 2 outputs
  
  
  
  output$compare11 <- renderPlot({ # Bar plot bottom middle
    
    # chosen_data <- subset(US_state_final, YEAR == input$year2 & STATE %in% st & ENERGY.SOURCE == input$source2)
    
    st <- c(setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone1])
    
    if(input$year1 == "(All)")
    {year <- seq(1990, 2019, by = 1)
    minx <- 1990
    maxx <- 2020}
    else{year <- c(input$year1)
    minx <- input$year1
    maxx <- input$year1}
    
    if(input$source1 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
    else{sources <- c(input$source1)}
    
    chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
    ggplot(chosen_data, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) +
      ggtitle(input$zone1) +
      scale_fill_manual( # Consistent color scheme between visualizations
        values = c("Coal" = "#F8766D", "GeoTh" = "#D39200","Hydro" = "#93AA00","N Gas" = "#00BA38","Nuclear" = "#00C19F","Petrol" = "#00B9E3","Solar" = "#619CFF","Wind" = "#DB72FB","Wood" = "#FF61C3"
        )) +
      geom_bar(position = "stack", stat = 'identity') +
      theme( text = element_text(size = 12, family = 'sans'), plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
      labs(fill = "Energy Source") +
      scale_x_continuous("Year", breaks = seq(minx, maxx, 6), labels = seq(minx, maxx, 6)) +
      scale_y_continuous("Energy produced", labels = addUnits)
    
  })
  
  output$compare12 <- renderPlot({ # Bar plot bottom middle
    
    # chosen_data <- subset(US_state_final, YEAR == input$year2 & STATE %in% st & ENERGY.SOURCE == input$source2)
    
    st <- c(setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone1])
    
    if(input$year1 == "(All)")
    {year <- seq(1990, 2019, by = 1)
    minx <- 1990
    maxx <- 2020}
    else{year <- c(input$year1)
    minx <- input$year1
    maxx <- input$year1}
    
    if(input$source1 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
    else{sources <- c(input$source1)}
    
    chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
    ggplot(chosen_data, aes(x = YEAR, y = Percentages, fill = ENERGY.SOURCE)) +
      ggtitle(input$zone1) +
      scale_fill_manual( # Consistent color scheme between visualizations
        values = c("Coal" = "#F8766D", "GeoTh" = "#D39200","Hydro" = "#93AA00","N Gas" = "#00BA38","Nuclear" = "#00C19F","Petrol" = "#00B9E3","Solar" = "#619CFF","Wind" = "#DB72FB","Wood" = "#FF61C3"
        )) +
      geom_bar(stat = 'identity') +
      theme( text = element_text(size = 12, family = 'sans'), plot.title = element_text(hjust = 0.5, face = "bold"), 
             legend.title = element_blank(), legend.text = element_text(size = 10)) +
      labs(fill = "Energy Source") +
      scale_x_continuous("Year", breaks = seq(minx, maxx, 6), labels = seq(minx, maxx, 6)) +
      scale_y_continuous("Percentage of total", labels = addUnits)
    
  })
  
  output$compare13 <- renderPlot({ # Line chart 1 top
    
    # chosen_data <- subset(US_state_final, YEAR == input$year2 & STATE %in% st & ENERGY.SOURCE == input$source2)
    
    st <- c(setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone1])
    
    if(input$year1 == "(All)")
    {year <- seq(1990, 2019, by = 1)
    minx <- 1990
    maxx <- 2020}
    else{year <- c(input$year1)
    minx <- input$year1
    maxx <- input$year1}
    
    if(input$source1 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
    else{sources <- c(input$source1)}
    
    chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
    ggplot(chosen_data, aes(x = YEAR, y = GENERATION..Megawatthours., color = ENERGY.SOURCE)) +
      ggtitle(input$zone1) +
      scale_color_manual( # Consistent color scheme between visualizations
        values = c("Coal" = "#F8766D", "GeoTh" = "#D39200","Hydro" = "#93AA00","N Gas" = "#00BA38","Nuclear" = "#00C19F","Petrol" = "#00B9E3","Solar" = "#619CFF","Wind" = "#DB72FB","Wood" = "#FF61C3"
        )) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      theme( text = element_text(size = 12, family = 'sans'), plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
      labs(fill = "Energy Source") +
      scale_x_continuous("Year", breaks = seq(minx, maxx, 6), labels = seq(minx, maxx, 6)) +
      scale_y_continuous("Energy produced", labels = addUnits)
    
  })
  
  output$compare14 <- renderPlot({ # Bar plot bottom middle
    
    # chosen_data <- subset(US_state_final, YEAR == input$year2 & STATE %in% st & ENERGY.SOURCE == input$source2)
    
    st <- c(setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone1])
    
    if(input$year1 == "(All)")
    {year <- seq(1990, 2019, by = 1)
    minx <- 1990
    maxx <- 2020}
    else{year <- c(input$year1)
    minx <- input$year1
    maxx <- input$year1}
    
    if(input$source1 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
    else{sources <- c(input$source1)}
    
    chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
    ggplot(chosen_data, aes(x = YEAR, y = Percentages, color = ENERGY.SOURCE)) +
      ggtitle(input$zone1) +
      scale_color_manual( # Consistent color scheme between visualizations
        values = c("Coal" = "#F8766D", "GeoTh" = "#D39200","Hydro" = "#93AA00","N Gas" = "#00BA38","Nuclear" = "#00C19F","Petrol" = "#00B9E3","Solar" = "#619CFF","Wind" = "#DB72FB","Wood" = "#FF61C3"
        )) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      theme( text = element_text(size = 12, family = 'sans'), plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
      labs(fill = "Energy Source") +
      scale_x_continuous("Year", breaks = seq(minx, maxx, 6), labels = seq(minx, maxx, 6)) +
      scale_y_continuous("Percentage of total", labels = addUnits)
    
  })
  
  output$compare15 <- DT::renderDataTable( # table top right
    class = 'cell-border stripe',
    options = list(columnDefs = list(list(targets = c(4,5), searchable = FALSE, className = 'dt-right')), dom = 'tp', pageLength = 5,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                     "}")),
    { 
      st <- setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone1]
      
      if(input$year1 == "(All)"){year <- seq(1990, 2019, by = 1)}
      else{year <- c(input$year1)}
      
      if(input$source1 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
      else{sources <- c(input$source1)}
      
      chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
      chosen_data$GENERATION..Megawatthours. <- addCommas(chosen_data$GENERATION..Megawatthours.)
      chosen_data$Percentages <- signif(chosen_data$Percentages, digits = 2)
      chosen_data[, c("YEAR", "STATE", "ENERGY.SOURCE", "GENERATION..Megawatthours.", "Percentages")]
      
    }, 
    colnames = c("Yr", "St", "Src", "(MWh)", "(%)")
  )
  
  output$compare21 <- renderPlot({ # Bar plot 1 bottom
    
    # chosen_data <- subset(US_state_final, YEAR == input$year2 & STATE %in% st & ENERGY.SOURCE == input$source2)
    
    st <- c(setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone2])
    
    if(input$year2 == "(All)")
    {year <- seq(1990, 2019, by = 1)
    minx <- 1990
    maxx <- 2020}
    else{year <- c(input$year2)
    minx <- input$year2
    maxx <- input$year2}
    
    if(input$source2 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
    else{sources <- c(input$source2)}
    
    chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
    ggplot(chosen_data, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) +
      ggtitle(input$zone2) +
      scale_fill_manual( # Consistent color scheme between visualizations
        values = c("Coal" = "#F8766D", "GeoTh" = "#D39200","Hydro" = "#93AA00","N Gas" = "#00BA38","Nuclear" = "#00C19F","Petrol" = "#00B9E3","Solar" = "#619CFF","Wind" = "#DB72FB","Wood" = "#FF61C3"
        )) +
      geom_bar(position = "stack", stat = 'identity') +
      theme( text = element_text(size = 12, family = 'sans'), plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
      labs(fill = "Energy Source") +
      scale_x_continuous("Year", breaks = seq(minx, maxx, 6), labels = seq(minx, maxx, 6)) +
      scale_y_continuous("Energy produced", labels = addUnits)
    
  })
  
  output$compare22 <- renderPlot({ # Bar plot 2 bottom
  
    # chosen_data <- subset(US_state_final, YEAR == input$year2 & STATE %in% st & ENERGY.SOURCE == input$source2)
    
    st <- c(setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone2])
    
    if(input$year2 == "(All)")
    {year <- seq(1990, 2019, by = 1)
      minx <- 1990
      maxx <- 2020}
    else{year <- c(input$year2)
          minx <- input$year2
          maxx <- input$year2}
    
    if(input$source2 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
    else{sources <- c(input$source2)}
    
    chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
    ggplot(chosen_data, aes(x = YEAR, y = Percentages, fill = ENERGY.SOURCE)) +
      ggtitle(input$zone2) +
      scale_fill_manual( # Consistent color scheme between visualizations
        values = c("Coal" = "#F8766D", "GeoTh" = "#D39200","Hydro" = "#93AA00","N Gas" = "#00BA38","Nuclear" = "#00C19F","Petrol" = "#00B9E3","Solar" = "#619CFF","Wind" = "#DB72FB","Wood" = "#FF61C3"
        )) +
      geom_bar(stat = 'identity') +
      theme( text = element_text(size = 12, family = 'sans'), plot.title = element_text(hjust = 0.5, face = "bold"),
             legend.title = element_blank(), legend.text = element_text(size = 10)) +
      labs(fill = "Energy Source") +
      scale_x_continuous("Year", breaks = seq(minx, maxx, 6), labels = seq(minx, maxx, 6)) +
      scale_y_continuous("Percentage of total", labels = addUnits)
    
  })
  
  output$compare23 <- renderPlot({ # Line chart 1 bottom

    st <- c(setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone2])
    
    if(input$year2 == "(All)")
    {year <- seq(1990, 2019, by = 1)
    minx <- 1990
    maxx <- 2020}
    else{year <- c(input$year2)
    minx <- input$year2
    maxx <- input$year2
    }
    
    if(input$source2 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
    else{sources <- c(input$source2)}
    
    chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
    ggplot(chosen_data, aes(x = YEAR, y = GENERATION..Megawatthours., color = ENERGY.SOURCE)) +
      ggtitle(input$zone2) +
      scale_color_manual( # Consistent color scheme between visualizations
        values = c("Coal" = "#F8766D", "GeoTh" = "#D39200","Hydro" = "#93AA00","N Gas" = "#00BA38","Nuclear" = "#00C19F","Petrol" = "#00B9E3","Solar" = "#619CFF","Wind" = "#DB72FB","Wood" = "#FF61C3"
        )) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      theme( text = element_text(size = 12, family = 'sans'), plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
      labs(fill = "Energy Source") +
      scale_x_continuous("Year", breaks = seq(minx, maxx, 6), labels = seq(minx, maxx, 6)) +
      scale_y_continuous("Energy produced", labels = addUnits)
    
  })
  
  output$compare24 <- renderPlot({ # Line chart 2 bottom
    
    # chosen_data <- subset(US_state_final, YEAR == input$year2 & STATE %in% st & ENERGY.SOURCE == input$source2)
    
    st <- c(setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone2])
    
    if(input$year2 == "(All)")
    {year <- seq(1990, 2019, by = 1)
    minx <- 1990
    maxx <- 2020}
    else{year <- c(input$year2)
    minx <- input$year2
    maxx <- input$year2}
    
    if(input$source2 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
    else{sources <- c(input$source2)}
    
    chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
    ggplot(chosen_data, aes(x = YEAR, y = Percentages, color = ENERGY.SOURCE)) +
      ggtitle(input$zone2) +
      scale_color_manual( # Consistent color scheme between visualizations
        values = c("Coal" = "#F8766D", "GeoTh" = "#D39200","Hydro" = "#93AA00","N Gas" = "#00BA38","Nuclear" = "#00C19F","Petrol" = "#00B9E3","Solar" = "#619CFF","Wind" = "#DB72FB","Wood" = "#FF61C3"
        )) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      theme( text = element_text(size = 12, family = 'sans'), plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
      labs(fill = "Energy Source") +
      scale_x_continuous("Year", breaks = seq(minx, maxx, 6), labels = seq(minx, maxx, 6)) +
      scale_y_continuous("Percentage of total", labels = addUnits)
    
  })
  
  output$compare25 <- DT::renderDataTable( # Table bottom right
    class = 'cell-border stripe',
    options = list(columnDefs = list(list(targets = c(4,5), searchable = FALSE, className = 'dt-right')), dom = 'tp', pageLength = 5,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                     "}")),
    { 
      st <- setNames(c(state.abb, "DC", "US-TOTAL"), c(state.name, "Washington D.C.", "US Total"))[input$zone2]
      
      if(input$year2 == "(All)"){year <- seq(1990, 2019, by = 1)}
      else{year <- c(input$year2)}
      
      if(input$source2 == "(All)"){sources <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Wind", "Wood")}
      else{sources <- c(input$source2)}
      
      chosen_data <- subset(US_state_final, YEAR %in% year & STATE == st & ENERGY.SOURCE %in% sources)
      chosen_data$GENERATION..Megawatthours. <- addCommas(chosen_data$GENERATION..Megawatthours.)
      chosen_data$Percentages <- signif(chosen_data$Percentages, digits = 2)
      chosen_data[, c("YEAR", "STATE", "ENERGY.SOURCE", "GENERATION..Megawatthours.", "Percentages")]
      
    },
    colnames = c("Yr", "St", "Src", "(MWh)", "(%)")
  )
  
  # Block for heat map generation
  
  output$map1 <- renderPlot({
    
    
    hi = switch(input$source1, 
                "Coal" = "#F8766D", 
                "GeoTh" = "#D39200",
                "Hydro" = "#93AA00",
                "N Gas" = "#00BA38",
                "Nuclear" = "#00C19F",
                "Petrol" = "#00B9E3",
                "Solar" = "#619CFF",
                "Wind" = "#DB72FB",
                "Wood" = "#FF61C3"
    )
    chosen_data <- subset(US_state_final, YEAR == input$year1 & ENERGY.SOURCE == input$source1)
    colnames(chosen_data)[colnames(chosen_data) == 'STATE'] <- 'state'
    plot_usmap(data = chosen_data, values = "GENERATION..Megawatthours.", regions = "states", size = 1) +
      labs(title = paste0(input$source1, " production in ", input$year1)) +
      theme(plot.title = element_text(size = 15), legend.position = c(0.92,0.3), 
            legend.text = element_text(size = 10), legend.title = element_text(size = 12)) +
      scale_fill_continuous(low = "white", high = hi , name = "Energy produced (MWh)", label = addUnits)
    
  })
  
  output$map2 <- renderPlot({
    
    hi = switch(input$source1, 
                "Coal" = "#F8766D", 
                "GeoTh" = "#D39200",
                "Hydro" = "#93AA00",
                "N Gas" = "#00BA38",
                "Nuclear" = "#00C19F",
                "Petrol" = "#00B9E3",
                "Solar" = "#619CFF",
                "Wind" = "#DB72FB",
                "Wood" = "#FF61C3"
    )
    chosen_data <- subset(US_state_final, YEAR == input$year1 & ENERGY.SOURCE == input$source1)
    colnames(chosen_data)[colnames(chosen_data) == 'STATE'] <- 'state'
    plot_usmap(data = chosen_data, values = "Percentages", regions = "states", size = 1) +
      labs(title = paste0(input$source1, " share in ", input$year1)) +
      theme(plot.title = element_text(size = 15), legend.position = c(0.92,0.3), 
            legend.text = element_text(size = 10), legend.title = element_text(size = 12)) +
      scale_fill_continuous(low = "white", high = hi , name = "Energy share (%)", label = addUnits)
    
  })
  
  output$map3 <- renderPlot({
    
    
    hi = switch(input$source2, 
                "Coal" = "#F8766D", 
                "GeoTh" = "#D39200",
                "Hydro" = "#93AA00",
                "N Gas" = "#00BA38",
                "Nuclear" = "#00C19F",
                "Petrol" = "#00B9E3",
                "Solar" = "#619CFF",
                "Wind" = "#DB72FB",
                "Wood" = "#FF61C3"
    )
    chosen_data <- subset(US_state_final, YEAR == input$year2 & ENERGY.SOURCE == input$source2)
    colnames(chosen_data)[colnames(chosen_data) == 'STATE'] <- 'state'
    plot_usmap(data = chosen_data, values = "GENERATION..Megawatthours.", regions = "states", size = 1) +
      labs(title = paste0(input$source2, " production in ", input$year2)) +
      theme(plot.title = element_text(size = 15), legend.position = c(0.92,0.3), 
            legend.text = element_text(size = 10), legend.title = element_text(size = 12)) +
      scale_fill_continuous(low = "white", high = hi , name = "Energy produced (MWh)", label = addUnits)
    
  })
  
  output$map4 <- renderPlot({
    
    hi = switch(input$source2, 
                "Coal" = "#F8766D", 
                "GeoTh" = "#D39200",
                "Hydro" = "#93AA00",
                "N Gas" = "#00BA38",
                "Nuclear" = "#00C19F",
                "Petrol" = "#00B9E3",
                "Solar" = "#619CFF",
                "Wind" = "#DB72FB",
                "Wood" = "#FF61C3"
    )
    chosen_data <- subset(US_state_final, YEAR == input$year2 & ENERGY.SOURCE == input$source2)
    colnames(chosen_data)[colnames(chosen_data) == 'STATE'] <- 'state'
    plot_usmap(data = chosen_data, values = "Percentages", regions = "states", size = 1) +
      labs(title = paste0(input$source2, " share in ", input$year2)) +
      theme(plot.title = element_text(size = 15), legend.position = c(0.92,0.3), 
            legend.text = element_text(size = 10), legend.title = element_text(size = 12)) +
      scale_fill_continuous(low = "white", high = hi , name = "Energy share (%)", label = addUnits)
    
  })
  
  # Block for 5 interesting buttons
  
  observeEvent(input$interest1, {
    
    updateCheckboxGroupInput(session, "lineSources", selected = c("Coal", "N Gas"))
    session$sendCustomMessage(type = 'testmessage',
                              message = 'While coal has taken a hit, Natural gas consumption is rising! Non-renewables are still strong')
  }
  )
  
  observeEvent(input$interest2, {
    
    updateSelectInput(session, "year1", selected = "(All)")
    updateSelectInput(session, "year2", selected = "(All)")
    updateSelectInput(session, "source1", selected = "Wind")
    updateSelectInput(session, "source2", selected = "Wind")
    updateSelectInput(session, "zone1", selected = "US Total")
    updateSelectInput(session, "zone2", selected = "Texas")
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Texas is the champion of wind energy!')
  }
  )
  
  observeEvent(input$interest3, {
    
    updateSelectInput(session, "year1", selected = "(All)")
    updateSelectInput(session, "year2", selected = "(All)")
    updateSelectInput(session, "source1", selected = "N Gas")
    updateSelectInput(session, "source2", selected = "N Gas")
    updateSelectInput(session, "zone1", selected = "US Total")
    updateSelectInput(session, "zone2", selected = "Texas")
    session$sendCustomMessage(type = 'testmessage',
                              message = '... But it really needs go easy on Natural Gas! 52%?? Wow. Top producer for sure!')
  }
  )
  
  observeEvent(input$interest4, {
    
    updateSelectInput(session, "year1", selected = "1990")
    updateSelectInput(session, "year2", selected = "2019")
    updateSelectInput(session, "source1", selected = "Solar")
    updateSelectInput(session, "source2", selected = "Solar")
    session$sendCustomMessage(type = 'testmessage',
                              message = 'From nothing to something! The black regions mean solar was nearly non-existant in 1990! It also went from a measly 0.1~0.2% to 10%+ in some states!')
  }
  )
  
  observeEvent(input$interest5, {
    
    updateSelectInput(session, "year1", selected = "1990")
    updateSelectInput(session, "year2", selected = "2019")
    updateSelectInput(session, "source1", selected = "N Gas")
    updateSelectInput(session, "source2", selected = "N Gas")
    session$sendCustomMessage(type = 'testmessage',
                              message = 'States are becoming increasingly dependent on Natural Gas for their energy needs, just like Texas was in 1990. In fact, it looks like Florida will soon take the number 1 spot!')

  }
  )

}

