full_data <- read.table(file = "annual_generation_state.csv", sep = ",", header = TRUE) # This assumes the file is accessible in your working directory

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

levels(mod_data$ENERGY.SOURCE) <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Total", "wind", "wood")