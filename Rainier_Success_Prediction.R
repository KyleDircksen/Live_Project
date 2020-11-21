library(tidyverse)
library(reshape2)
library(dplyr)
library(lubridate)

climb = read.csv(file = 'climbing_statistics.csv')

weather = read.csv(file = 'Rainier_Weather.csv')

# Inspecting the overall data to determine next step
head(climb)
head(weather)

summary(climb)
summary(weather)

unique(climb$Route)
unique(climb)

# Removing "Unknown" from routes
climb <- climb[!grepl("Unknown", climb$Route),]

# Checking the colummn names and renaming ï..Date to Date
colnames(climb)
climb <- dplyr::rename(climb, Date = ï..Date)

# Searching for any missing values in climbing
sapply(climb, function(x) sum(is.na(x)))

# dropping Attempted and Succeeded, as we only need date, route and success %
climbing <- climb[, -c(3:4)]

# Search for any missing values in weather
sapply(weather, function(x) sum(is.na(x)))

# Setting the "Date" column as a Date datatype for both climb and weather dataframes
sapply(climbing, class)
climbing$Date <- mdy(climbing$Date)
weather$Date <- mdy(weather$Date)
climb$Date <- mdy(climb$Date)

# Merge weather and climbing tables, on Date
head(climbing)

require(data.table)
setDT(climbing)[weather, c("Temperature.AVG", "Relative.Humidity.AVG", "Wind.Speed.Daily.AVG",
                           "Wind.Direction.AVG", "Solare.Radiation.AVG") := 
                          .(Temperature.AVG, Relative.Humidity.AVG, Wind.Speed.Daily.AVG,
                            Wind.Direction.AVG, Solare.Radiation.AVG), on=c("Date")]

head(climbing)

sapply(climbing, class)
sapply(weather, class)

#Checking for missing values after the merge
sapply(climbing, function(x) sum(is.na(x)))


# Removing all rows with missing values from weather factors
#climbing <- climbing %>% drop_na(Temperature.AVG, Relative.Humidity.AVG, Wind.Speed.Daily.AVG,
#                Wind.Direction.AVG, Solare.Radiation.AVG)

# Too many rows get deleted, which causes some Routes to have no observations


# Going to try to utilize ImputeTS for missing values, using the linear interpolation method
# After creating the model I will try to utilize the drop method above to compare prediction accuracy
library(imputeTS)
df <- na_interpolation(climbing)

sapply(df, function(x) sum(is.na(x)))


# Create unique variables for each route
unique(df$Route)

# Noticed that "Fuhrers Finger" had a duplicate row spelled as "Fuhrer's Finger", so remove apostrophe's
df$Route <- gsub("'", '', df$Route)

# Back to creating variables for each route, dropping the Route column as well
Disappointment_Cleaver <- df[grepl("Disappointment Cleaver", df$Route),]
Disappointment_Cleaver <- Disappointment_Cleaver[, -2]

Little_Tahoma <- df[grepl("Little Tahoma", df$Route),]
Little_Tahoma <- Little_Tahoma[, -2]

Kautz_Glacier <- df[grepl("Kautz Glacier", df$Route),]
Kautz_Glacier <- Kautz_Glacier[, -2]

Emmons_Winthrop <- df[grepl("Emmons-Winthrop", df$Route),]
Emmons_Winthrop <- Emmons_Winthrop[, -2]

Glacier_Only <- df[grepl("glacier only - no summit attempt", df$Route),]
Glacier_Only <- Glacier_Only[, -2]

Fuhrers_Finger <- df[grepl("Fuhrers Finger", df$Route),]
Fuhrers_Finger <- Fuhrers_Finger[, -2]

Success_Cleaver <- df[grepl("Success Cleaver", df$Route),]
Success_Cleaver <- Success_Cleaver[, -2]

Liberty_Ringraham_Directge <- df[grepl("Liberty RIngraham Directge", df$Route),]
Liberty_Ringraham_Directge <- Liberty_Ringraham_Directge[, -2]

Kautz_Cleaver <- df[grepl("Kautz Cleaver", df$Route),]
Kautz_Cleaver <- Kautz_Cleaver[, -2]

Tahoma_Glacier <- df[grepl("Tahoma Glacier", df$Route),]
Tahoma_Glacier <- Tahoma_Glacier[, -2]

Ptarmigan_Ringraham_Directge <- df[grepl("Ptarmigan RIngraham Directge", df$Route),]
Ptarmigan_Ringraham_Directge <- Ptarmigan_Ringraham_Directge[, -2]

Mowich_Face <- df[grepl("Mowich Face", df$Route),]
Mowich_Face <- Mowich_Face[, -2]

Ingraham_Direct <- df[grepl("Ingraham Direct", df$Route),]
Ingraham_Direct <- Ingraham_Direct[, -2]

Sunset_Ringraham_Directge <- df[grepl("Sunset RIngraham Directge", df$Route),]
Sunset_Ringraham_Directge <- Sunset_Ringraham_Directge[, -2]

Curtis_Ringraham_Directge <- df[grepl("Curtis RIngraham Directge", df$Route),]
Curtis_Ringraham_Directge <- Curtis_Ringraham_Directge[, -2]

Tahoma_Cleaver <- df[grepl("Tahoma Cleaver", df$Route),]
Tahoma_Cleaver <- Tahoma_Cleaver[, -2]

Gibralter_Ledges <- df[grepl("Gibralter Ledges", df$Route),]
Gibralter_Ledges <- Gibralter_Ledges[, -2]

Nisqually_Glacier <- df[grepl("Nisqually Glacier", df$Route),]
Nisqually_Glacier <- Nisqually_Glacier[, -2]

Wilson_Headwall <- df[grepl("Wilson Headwall", df$Route),]
Wilson_Headwall <- Wilson_Headwall[, -2]

Gibralter_Chute <- df[grepl("Gibralter Chute", df$Route),]
Gibralter_Chute <- Gibralter_Chute[, -2]

Edmonds_HW <- df[grepl("Edmonds HW", df$Route),]
Edmonds_HW <- Edmonds_HW[, -2]

Sunset_Amphitheater <- df[grepl("Sunset Amphitheater", df$Route),]
Sunset_Amphitheater <- Sunset_Amphitheater[, -2]

Kautz_Headwall <- df[grepl("Kautz Headwall", df$Route),]
Kautz_Headwall <- Kautz_Headwall[, -2]

Liberty_Wall <- df[grepl("Liberty Wall", df$Route),]
Liberty_Wall <- Liberty_Wall[, -2]


# Create a predictive model, with weather as factors

