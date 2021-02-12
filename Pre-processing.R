full_data <- read.table(file = "annual_generation_state.csv", sep = ",", header = TRUE, row.names = NULL) # This assumes the file is accessible in your working directory

# Block to convert generation data from string to numbers

mod_data <- full_data
mod_data$GENERATION..Megawatthours.<- as.numeric(gsub(",", "", mod_data$GENERATION..Megawatthours.))

# gsub helped to remove the commas, which were causing as.numeric to return NA instead of numbers

# Block to remove states with missing identifiers

mod_data <- subset(mod_data, mod_data$STATE != "  ")

# Block to convert STATE, TYPE OF PRODUCER, and ENERGY SOURCE to factors and unify total US entries

mod_data$STATE <- as.factor(toupper(mod_data$STATE))
mod_data$TYPE.OF.PRODUCER <- as.factor(mod_data$TYPE.OF.PRODUCER)
mod_data$ENERGY.SOURCE <- as.factor(mod_data$ENERGY.SOURCE)

# Block to remove negative GENERATION numbers and remove the specified sources

mod_data <- subset(mod_data, mod_data$GENERATION..Megawatthours. >= 0)
mod_data <- subset(mod_data, ! ENERGY.SOURCE %in% c("Other","Other Gases", "Other Biomass", "Pumped Storage"))
mod_data$ENERGY.SOURCE <- factor(mod_data$ENERGY.SOURCE)

# Block to change Energy Source names

levels(mod_data$ENERGY.SOURCE) <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Total", "Wind", "Wood")

# Block to remove total from processed data frame (for bar plots)

US_total <- subset(mod_data, mod_data$ENERGY.SOURCE != "Total") # Dataframe without the total energy source
US_total$ENERGY.SOURCE <- factor(US_total$ENERGY.SOURCE)
US_total <- subset(US_total, US_total$STATE == "US-TOTAL") # Dataframe with records containing only US-TOTAL state
US_total$STATE <- factor(US_total$STATE)
US_total <- subset(US_total, US_total$TYPE.OF.PRODUCER == "Total Electric Power Industry") # Data frame with records for total industry
US_total$TYPE.OF.PRODUCER <- factor(US_total$TYPE.OF.PRODUCER)
US_total <- US_total[, c("YEAR", "STATE", "TYPE.OF.PRODUCER", "ENERGY.SOURCE", "GENERATION..Megawatthours.")]

# Block to generate percentage data for line chart

year_sum <- aggregate(GENERATION..Megawatthours. ~ YEAR, data=US_total, FUN = sum) # Data frame of energy produced summed by year
c <- 1
x <- 1
percents <- rep(NA, 270)
for (row in 1:nrow(US_total)){ # Loop to find percentage of every record in US_total
  percents[row] <- (US_total[row,"GENERATION..Megawatthours."]/year_sum[x, "GENERATION..Megawatthours."]) * 100
  # print(US_total[row, "YEAR"])
  # print(year_sum[x, "YEAR"])
  # print((US_total[row,"GENERATION..Megawatthours."]/year_sum[x, "GENERATION..Megawatthours."]) * 100)
  if (c == 9){
    c <- 1
    x <- x + 1
  }
  else{
    c <- c + 1
  }
}

US_total$Percentages <- percents # Add vector of percentages to data frame
