ridership <- read.csv("~/training/CTA/CTA_Ridership.csv", header = T)
ridership <- ridership[!grepl("Madison/Wabash", ridership$stationname),]
summary(ridership$rides)

write.csv(ridership, file = "dom_beat_stats.csv")

hist(ridership$rides, breaks="FD")
describe(ridership$rides)

consolidated <- aggregate(. ~ stationname, data=ridership, FUN=sum)
hist(consolidated$rides, breaks="FD")
summary(consolidated$rides)

# Total yearly ridership of all stations
total_year_stations <- sum(consolidated$rides)

# Percentage of system ridership per station
consolidated$share <- (consolidated$rides / total_year_stations) * 100

hist(consolidated$share, breaks="FD")

plot(consolidated$share, consolidated$stationname, main="Scatterplot Example", 
     xlab="Share of riders per year", ylab="Station", pch=19)

# Standard deviation for rides per year
consolidated_sd <- apply(consolidated, 2, sd)
consolidated_mean <- mean(consolidated$rides)

standard_deviation_rides <- consolidated_sd["rides"]

consolidated_one_sd_max <- consolidated_mean + standard_deviation_rides
consolidated_one_sd_min <- consolidated_mean - standard_deviation_rides

# Only ridership within sd

consolidated_sd_bound <- consolidated[consolidated$rides >= consolidated_one_sd_min & consolidated$rides <= consolidated_one_sd_max, ]

hist(consolidated_sd_bound$rides, breaks="FD")
describe(consolidated_sd_bound$rides)

boxplot(consolidated_sd_bound$rides,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Riders",
        xlab = "Number of rides per year per station")

plot(consolidated_sd_bound$rides, consolidated_sd_bound$stationname, main="Scatterplot Example", 
     xlab="Number of rides per year ", ylab="Station", pch=19)

# Split rides into four data frames
ridership_grouped <- split(consolidated, sample(1:4, nrow(consolidated), replace=T))

# First data frame
ridership_group_A <- ridership_grouped[[1]]
ridership_group_B <- ridership_grouped[[2]]
ridership_group_C <- ridership_grouped[[3]]
ridership_group_D <- ridership_grouped[[4]]

# Getting column sums of data frame
sum (ridership_group_A$rides, na.rm = TRUE, dims = 1)
sum (ridership_group_B$rides, na.rm = TRUE, dims = 1)
sum (ridership_group_C$rides, na.rm = TRUE, dims = 1)
sum (ridership_group_D$rides, na.rm = TRUE, dims = 1)

hist(ridership_group_A$rides, breaks="FD")
hist(ridership_group_B$rides, breaks="FD")
hist(ridership_group_C$rides, breaks="FD")
hist(ridership_group_D$rides, breaks="FD")

boxplot(ridership_group_A$rides,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Riders",
        xlab = "Number of rides per year per station")

boxplot(ridership_group_B$rides,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Riders",
        xlab = "Number of rides per year per station")

boxplot(ridership_group_C$rides,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Riders",
        xlab = "Number of rides per year per station")

boxplot(ridership_group_D$rides,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Riders",
        xlab = "Number of rides per year per station")

plot(consolidated$rides, consolidated$stationname, main="Scatterplot Example", 
     xlab="Number of rides per year ", ylab="Station", pch=19)

# Tukey's five-number summary: minimum, lower-hinge
#     (i.e., first quartile), median, upper-hinge
#     (i.e., third quartile), maximum
# Doesn't print labels
fivenum(consolidated$rides)

# Alternate descriptive statistics
# Gives n, mean, standard deviation, median, trimmed mean
#   (10% by default),median absolute deviation from median
#   (MAD), min, max, range, skew, kurtosis, and
#   standard error.
# Options for listwise deletion of missing data, methods of 
#   calculating median/skew/kurtosis, amount of trimming, etc.
# Note: Converts categories to sequential numbers and does
#   stats; can be useful in certain situations; marks with *
describe(consolidated$rides)

boxplot(consolidated$rides)

boxplot(consolidated$rides,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Riders",
        xlab = "Number of rides per year per station")

# Crime

crime <- read.csv("~/training/CTA/CTA_Crime.csv", header = T)

summary(crime$Location.Description)
summary(crime$Primary.Type)

types <- table(crime$Primary.Type)
locations <- table(crime$Location.Description)

par(mar=c(15,3,1,3))
barplot(types, las=2)
barplot(locations, las=2)

crime$incident_count <- 1

# Crimes grouped by location
crime_block <- aggregate(. ~ Block, data=crime, FUN=sum)

summary(crime_block$incident_count)
hist(crime_block$incident_count, breaks="FD")
describe(crime_block$incident_count)

boxplot(crime_block$incident_count)

boxplot(crime_block$incident_count,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Riders",
        xlab = "Number of rides per year per station")

plot(crime_block$incident_count, crime_block$Block, main="Crime", 
     xlab="Crime per station", ylab="Station", pch=19)

# Bus

bus <- read.csv("~/training/CTA/CTA-Bus.csv", header = T)
summary(bus$boardings)
hist(bus$boardings, breaks="FD")

plot(bus$boardings, bus$stop_id, main="Scatterplot Example", 
     xlab="Number of boardings per stop ", ylab="Stop", pch=1)

describe(bus$boardings)

boxplot(bus$boardings)

boxplot(bus$boardings,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Riders",
        xlab = "Avg boardings per stop per day")

#Find out which bus stops discharge more passengers than receive
bus$alighting_surplus <- (bus$alightings - bus$boardings)

#Find out which bus stops receive more passengers than discharge
bus$boarding_surplus <- (bus$boardings - bus$alightings)

summary(bus$alighting_surplus)
hist(bus$alighting_surplus, breaks="FD")
describe(bus$alighting_surplus)

boxplot(bus$alighting_surplus)

boxplot(bus$alighting_surplus,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Alighting surplus",
        xlab = "Avg per day per stop")

#Speeding
speed <- read.csv("~/training/CTA/Speed_Camera_Violations.csv", header = T)

summary(speed$VIOLATIONS)
hist(speed$VIOLATIONS, breaks="FD")
describe(speed$VIOLATIONS)

plot(speed$VIOLATIONS, speed$ADDRESS, main="Scatterplot Example", 
     xlab="Number of speeding violations", ylab="Address", pch=1)

boxplot(speed$VIOLATIONS)

boxplot(speed$VIOLATIONS,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Speeding violations",
        xlab = "Violations")

#Consolidate per address for entire year

speed_year <- aggregate(. ~ ADDRESS, data=speed, FUN=sum)

summary(speed_year$VIOLATIONS)
hist(speed_year$VIOLATIONS, breaks="FD")
describe(speed_year$VIOLATIONS)

plot(speed_year$VIOLATIONS, speed_year$ADDRESS, main="Scatterplot Example", 
     xlab="Number of speeding violations", ylab="Address", pch=1)

boxplot(speed_year$VIOLATIONS)

boxplot(speed_year$VIOLATIONS,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Speeding violations",
        xlab = "Violations per camera per year")

# Red lights

red <- read.csv("~/training/CTA/Red_Light_Camera_Violations.csv", header = T)

summary(red$VIOLATIONS)
hist(red$VIOLATIONS, breaks="FD")
describe(red$VIOLATIONS)

plot(red$VIOLATIONS, red$ADDRESS, main="Scatterplot Example", 
     xlab="Number of red violations", ylab="Address", pch=1)

boxplot(red$VIOLATIONS)

boxplot(red$VIOLATIONS,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Red violations",
        xlab = "Violations")

# Red light violations yearly totals

red_year <- aggregate(. ~ ADDRESS, data=red, FUN=sum)

summary(red_year$VIOLATIONS)
hist(red_year$VIOLATIONS, breaks="FD")
describe(red_year$VIOLATIONS)

plot(red_year$VIOLATIONS, red_year$ADDRESS, main="Scatterplot Example", 
     xlab="Number of red violations", ylab="Address", pch=1)

boxplot(red_year$VIOLATIONS)

boxplot(red_year$VIOLATIONS,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Red violations",
        xlab = "Reds per camera per year")

## Towed vehicles

tow <- read.csv("~/training/CTA/Towed_Vehicles.csv", header = T)

tow_make <- table(tow$Make)
summary(tow$Make)
tow_make_sorted <- sort(tow_make,decreasing=T)

barplot(tow_make_sorted, las=2)

tow$make_count <- 1

tow_make_agg <- aggregate(. ~ Make, data=tow, FUN=sum)

boxplot(tow_make_agg$make_count,
        col = "red",
        notch = F,
        horizontal = T,
        main = "Tows",
        xlab = "Tows per make per year")

# Building violations
violate <- read.csv("~/training/CTA/Building_Violations.csv", header = T)
summary(violate$VIOLATION.DESCRIPTION)

violate$tally <- 1

bldg_address <- aggregate(. ~ ADDRESS, data=violate, FUN=sum)

summary(bldg_address$tally)
hist(bldg_address$tally, breaks="FD")
describe(bldg_address$tally)

boxplot(bldg_address$tally)

boxplot(bldg_address$tally,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Bldg inspection reports",
        xlab = "Num reports per bldg since 2002")

# Crime 2015 complete analysis

crime_2015 <- read.csv("~/training/CTA/Crimes-2015.csv", header = T)

# Split crimes according to type

crime_2015_split <- split(crime_2015,crime_2015$Primary.Type)

crimes_2015_by_type <- sort(sapply(crime_2015_split, NROW), decreasing = TRUE)

par(mar=c(15,5,1,5))
barplot(crimes_2015_by_type, las=2)

# transform crimes by type into data frame from vector

crimes_type_stats <- data.frame(crimes_2015_by_type)

# get population estimate of Chicago in 2015

chicago_pop <- 2720556

# Crime rates per 1,000 persons

crimes_type_stats$per_capita <- 100000 / (chicago_pop / crimes_type_stats$crimes_2015_by_type)

boxplot(crimes_type_stats$per_capita,
        col = "red",
        notch = F,
        horizontal = T,
        main = "Crime per capita",
        xlab = "Crimes per 100,000 residents")

describe(crimes_type_stats$per_capita)

plot(crimes_type_stats$per_capita, crimes_type_stats$crimes_2015_by_type, main="Crime", 
     xlab="Crimes per capita", ylab="Crime", pch=1)

### Group crimes by different geographical criteria

# By beat

crime_2015_beat <- split(crime_2015,crime_2015$Beat)

crimes_2015_by_beat <- sort(sapply(crime_2015_beat, NROW), decreasing = TRUE)

par(mar=c(15,5,1,5))
barplot(crimes_2015_by_beat, las=2)

crimes_beat_stats <- data.frame(crimes_2015_by_beat)

# By Location type

crime_2015_loc_type <- split(crime_2015,crime_2015$Location.Description)

crime_2015_loc_type <- sort(sapply(crime_2015_loc_type, NROW), decreasing = TRUE)

par(mar=c(15,5,1,5))
barplot(crime_2015_loc_type, las=2)

crimes_loc_stats <- data.frame(crime_2015_loc_type)

crimes_loc_stats$per_capita <- 100000 / (chicago_pop / crimes_loc_stats$crime_2015_loc_type)

# Simple odds

crimes_loc_stats$odds_one_in <- (chicago_pop / crimes_loc_stats$crime_2015_loc_type)

plot(crimes_loc_stats$per_capita, crimes_loc_stats$crime_2015_loc_type, main="Crime", 
     xlab="Crimes per capita according to location", ylab="Crime", pch=1)

boxplot(crimes_loc_stats$per_capita,
        col = "red",
        notch = F,
        horizontal = T,
        main = "Crime per capita (location)",
        xlab = "Crimes per 100,000 residents by location")

# Specific crime types

crime_specific <- crime_2015

crime_specific$crime_full <- paste(crime_2015$Primary.Type,"-",crime_2015$Description)

crime_specific_list <- split(crime_specific,crime_specific$crime_full)

crime_specific_list_totals <- sort(sapply(crime_specific_list, NROW), decreasing = TRUE)

par(mar=c(15,5,1,5))
barplot(crime_specific_list_totals, las=2)

crimes_spec_stats <- data.frame(crime_specific_list_totals)

crimes_spec_stats$per_capita <- 100000 / (chicago_pop / crimes_spec_stats$crime_specific_list_totals)

crimes_spec_stats$odds_one_in <- (chicago_pop / crimes_spec_stats$crime_specific_list_totals)

boxplot(crimes_spec_stats$crime_specific_list_totals,
        col = "red",
        notch = F,
        horizontal = T,
        main = "Number of crimes",
        xlab = "Crimes by type")

# Exercise: determine beats with the highest number of domestic battery incidents

# 1) get list of just domestic battery incidents

crime_domestic <- data.frame(crime_specific_list["BATTERY - DOMESTIC BATTERY SIMPLE"])

# 2) Split list by beat

crime_dom_beats <- split(crime_domestic,crime_domestic$BATTERY...DOMESTIC.BATTERY.SIMPLE.Beat)

# 3) 

crime_dom_by_beat <- sort(sapply(crime_dom_beats, NROW), decreasing = TRUE)

# 4) Convert back to data.frame

crimes_dom_beat_stats <- data.frame(crime_dom_by_beat)

write.csv(crimes_dom_beat_stats, file = "dom_beat_stats.csv")

# Murders
crime_murder <- data.frame(crime_specific_list["HOMICIDE - FIRST DEGREE MURDER"])
crime_murder_beats <- split(crime_murder,crime_murder$HOMICIDE...FIRST.DEGREE.MURDER.Beat)
crime_murder_by_beat <- sort(sapply(crime_murder_beats, NROW), decreasing = TRUE)
crime_murder_beat_stats <- data.frame(crime_murder_by_beat)

write.csv(crime_murder_beat_stats, file = "murder_beat_stats.csv")

# Theft under 500
crime_theft_five_hundred <- data.frame(crime_specific_list["THEFT - $500 AND UNDER"])
crime_theft_five_hundred_beats <- split(crime_theft_five_hundred,crime_theft_five_hundred$THEFT....500.AND.UNDER.Beat)
crime_theft_five_hundred_by_beat <- sort(sapply(crime_theft_five_hundred_beats, NROW), decreasing = TRUE)
crime_theft_five_hundred_beat_stats <- data.frame(crime_theft_five_hundred_by_beat)

# Prepare data frame for export
colnames(crime_theft_five_hundred_beat_stats) <- c("crimes")
crime_theft_five_hundred_beat_stats$beat_num <- as.numeric(row.names(crime_theft_five_hundred_beat_stats))

write_json(crime_theft_five_hundred_beat_stats, path = "theft_500_under.json")

# Theft under 500
crime_rob_gun <- data.frame(crime_specific_list["ROBBERY - ARMED: HANDGUN"])
crime_rob_gun_beats <- split(crime_rob_gun,crime_rob_gun$ROBBERY...ARMED..HANDGUN.Beat)
crime_rob_gun_by_beat <- sort(sapply(crime_rob_gun_beats, NROW), decreasing = TRUE)
crime_rob_gun_beat_stats <- data.frame(crime_rob_gun_by_beat)

# Prepare data frame for export
colnames(crime_rob_gun_beat_stats) <- c("crimes")
crime_rob_gun_beat_stats$beat_num <- as.numeric(row.names(crime_rob_gun_beat_stats))

write_json(crime_rob_gun_beat_stats, path = "rob_gun_stats.json")

# JSON generator

test_index <- 0

generateJSON<- function(listItem){
  temp_frame <- data.frame(listItem)
  temp_frame_beats <- split(temp_frame,temp_frame$ROBBERY...ARMED..HANDGUN.Beat)
  temp_frame_beats_by_beat <- sort(sapply(temp_frame_beats, NROW), decreasing = TRUE)
  temp_frame_beat_stats <- data.frame(temp_frame_beats_by_beat)
  
  # Prepare data frame for export
  colnames(temp_frame_beat_stats) <- c("crimes")
  temp_frame_beat_stats$beat_num <- as.numeric(row.names(temp_frame_beat_stats))
  
  path <- paste(c("test", (test_index + 1), ".json"), collapse = "")
  
  write_json(temp_frame_beat_stats, path = path)
  
};

test <- "HOMICIDE...FIRST.DEGREE.MURDER.Beat"

test_name <- gsub('/.*[beat]/g', 'Beat', "HOMICIDE...FIRST.DEGREE.MURDER.Beat", fixed=TRUE)

