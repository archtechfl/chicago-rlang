socio_data <- read.csv("chicago-socio.csv", header = T)

summary(socio_data)

hist(socio_data$PERCENT.OF.HOUSING.CROWDED, breaks="FD")
hist(socio_data$PERCENT.HOUSEHOLDS.BELOW.POVERTY, breaks="FD")
hist(socio_data$PERCENT.AGED.16..UNEMPLOYED, breaks="FD")
hist(socio_data$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA, breaks="FD")
hist(socio_data$PERCENT.AGED.UNDER.18.OR.OVER.64, breaks="FD")
hist(socio_data$PER.CAPITA.INCOME, breaks="FD")

describe(socio_data$PERCENT.OF.HOUSING.CROWDED)
describe(socio_data$PERCENT.HOUSEHOLDS.BELOW.POVERTY)
describe(socio_data$PERCENT.AGED.16..UNEMPLOYED)
describe(socio_data$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA)
describe(socio_data$PERCENT.AGED.UNDER.18.OR.OVER.64)
describe(socio_data$PER.CAPITA.INCOME)

boxplot(socio_data$PERCENT.OF.HOUSING.CROWDED,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Crowding",
        xlab = "Percentage of housing deemed crowded")

boxplot(socio_data$PERCENT.HOUSEHOLDS.BELOW.POVERTY,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Poverty",
        xlab = "Percentage of households below poverty level")

boxplot(socio_data$PERCENT.AGED.16..UNEMPLOYED,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Unemployment",
        xlab = "Percentage age 16+ unemployed")

boxplot(socio_data$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA,
        col = "red",
        notch = T,
        horizontal = T,
        main = "High school diploma",
        xlab = "Percentage age 25+ without high school diploma")

boxplot(socio_data$PERCENT.AGED.UNDER.18.OR.OVER.64,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Child or seniors",
        xlab = "Percentage child or senior")

boxplot(socio_data$PER.CAPITA.INCOME,
        col = "red",
        notch = T,
        horizontal = T,
        main = "Per capita income",
        xlab = "Per capita income of area")

names(socio_data)

# Remove names and other non-quantitative columns
socio_data.quant <- socio_data[c(3,4,5,6,7,8)]

# Correlations
cor(socio_data.quant)

# Correlation test

cor.test(socio_data.quant$PER.CAPITA.INCOME, socio_data.quant$PERCENT.OF.HOUSING.CROWDED)

install.packages("Hmisc")
library("Hmisc")

rcorr(as.matrix(socio_data.quant))

# T test

# Crowded housing and poverty
t.test(socio_data$PERCENT.OF.HOUSING.CROWDED,socio_data$PERCENT.HOUSEHOLDS.BELOW.POVERTY)

# Percentage of children and seniors, compared to poverty
t.test(socio_data$PERCENT.AGED.UNDER.18.OR.OVER.64,socio_data$PERCENT.HOUSEHOLDS.BELOW.POVERTY)
 
# Percentage of children and seniors, compared to poverty
t.test(socio_data$PERCENT.AGED.UNDER.18.OR.OVER.64,socio_data$PER.CAPITA.INCOME)

#ANOVA
anova1 <- aov(PERCENT.AGED.UNDER.18.OR.OVER.64 ~ PER.CAPITA.INCOME, data=socio_data.quant)
summary(anova1)

# Chicago crime correlation

crime <- read.csv("Crimes-2015.csv", header = T)

# Get correlation using reshape2
library('reshape2')

rep.df <- dcast(crime,Beat~Primary.Type,length)
cor(rep.df)

# Split crimes by beat
crime_beats <- split(crime,crime$Beat)

# Get crime totals by beat
crimes_by_beat_total <- sort(sapply(crime_beats, NROW), decreasing = TRUE)

# Convert to data frame
crimes_by_beat_total_frame <- data.frame(crimes_by_beat_total)

colnames(crimes_by_beat_total_frame) <- c("crimes")

# Get the IDs of all beats for new data frame
beat_ids <- as.numeric(row.names(crimes_by_beat_total_frame))

# Get number of beats
number_beats <- length(beat_ids)

# Create crime list split by Primary type
crime_list <- split(crime,crime$Primary.Type)

# Get number of primary crimes
number_primary_crimes <- length(crime_list)

# Create empty data frame with empty column
crime_corr <- data.frame(matrix(, nrow=number_beats, ncol=0))

# Change row names to beat ids
row.names(crime_corr) <- beat_ids

# Set column names to names of primary crimes
column_names <- names(crime_list)

# Complete empty data frame so that there are no gaps
# This frame will be used to store data from analysis of primary crimes
crime_corr <- cbind(crime_corr, setNames( lapply(column_names, function(x) x=0), column_names) )

crime_corr <- as.matrix(crime_corr)

match_beats <- function(value){
  crimes <- strtoi(value["crimes"])
  beat_num <- gsub("[[:blank:]]", "", value["beat_num"])
  crime_name <- value["crime_name"]
  crime_corr[beat_num, crime_name] <<- crimes
  print(crime_corr[beat_num, crime_name])
}

assign_data <- function(listItem){
  temp_frame <- data.frame(listItem)
  
  crime_name <- head(temp_frame, 1)$Primary.Type

  temp_frame_beats <- split(temp_frame,temp_frame$Beat)
  temp_frame_beats_by_beat <- sort(sapply(temp_frame_beats, NROW), decreasing = TRUE)
  temp_frame_beat_stats <- data.frame(temp_frame_beats_by_beat)
  
  # Prepare data frame for export
  colnames(temp_frame_beat_stats) <- c("crimes")
  temp_frame_beat_stats$beat_num <- as.numeric(row.names(temp_frame_beat_stats))
  temp_frame_beat_stats$crime_name <- crime_name
  
  apply(temp_frame_beat_stats, 1, match_beats)
  
}

lapply(crime_list, FUN = assign_data)

print(crime_corr)
