full_data <- read.table(file = "annual_generation_state.csv", sep = ",", header = TRUE) # This assumes the file is accessible in your working directory

# Block to convert generation data from string to numbers

mod_data_1 <- full_data
mod_data_1$GENERATION..Megawatthours.<- as.numeric(gsub(",", "", mod_data_1$GENERATION..Megawatthours.))

# gsub helped to remove the commas, which were causing as.numeric to return NA instead of numbers

# Block to remove states with missing identifiers
mod_data_2 <- mod_data_1
mod_data_2 <- subset(mod_data_1, mod_data_2$STATE != "  ")

# Block to convert STATE, TYPE OF PRODUCER, and ENERGY SOURCE to factors and unify total US entries

mod_data_3 <- mod_data_2
mod_data_3$STATE <- as.factor(toupper(mod_data_2$STATE))
mod_data_3$TYPE.OF.PRODUCER <- as.factor(mod_data_2$TYPE.OF.PRODUCER)
mod_data_3$ENERGY.SOURCE <- as.factor(mod_data_2$ENERGY.SOURCE)

# Block to remove negative GENERATION numbers and remove the specified sources

mod_data_4 <- subset(mod_data_3, mod_data_3$GENERATION..Megawatthours. >= 0)
mod_data_4 <- subset(mod_data_4, ! ENERGY.SOURCE %in% c("Other","Other Gases", "Other Biomass", "Pumped Storage"))
mod_data_4$ENERGY.SOURCE <- factor(mod_data_4$ENERGY.SOURCE)

# Block to change Energy Source names

levels(mod_data_4$ENERGY.SOURCE) <- c("Coal", "GeoTh", "Hydro", "N Gas", "Nuclear", "Petrol", "Solar", "Total", "wind", "wood")