# Load data and packages
work_dir <- "C:/research/transform_fun/vbet_validation/vbet_validation"
setwd(work_dir)
evidence <-read.csv("vbet_evidence_03.csv")

library(ggplot2)
library(DBI)
library(tidyverse)
library(RSQLite)

# Change data type of category to factor
evidence$categoryid = as.factor(evidence$categoryid) 

# Create SQL database
vbet_evidence_3_db <-dbConnect(RSQLite::SQLite(), "vbet_evidence_03.db")

# Create evidence table
dbExecute(vbet_evidence_3_db, "CREATE TABLE evidence (
          observationid,
          HUC8,
          categoryid,
          confidence,
          userid,
          notes,
          created_date,
          updated_date,
          category_name,
          StreamOrder,
          DrainageAreaSqkm,
          InputZone,
          HAND,
          ChannelDist,
          TWI,
          Slope
          );")

# Loading csv into 'evidence' table:
evidence <- read.csv("vbet_evidence_03.csv", stringsAsFactors = FALSE)
names(evidence)

dbWriteTable(vbet_evidence_3_db, "evidence", evidence, append = TRUE)

# Checking to make sure it's reading the data properly

evidence <- dbGetQuery(vbet_evidence_3_db, "SELECT * FROM evidence;")
evidence

## Distributions - All Observations
# Box plots

ggplot(data = evidence,
       mapping = aes(y = TWI)) +
  geom_boxplot() +
  labs (y = "Topographic Wetness Index")

ggplot(data = evidence,
       mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Slope (degrees)")

# Hitting an error here too
ggplot(data = evidence,
       mapping = aes(y = HAND)) +
  geom_boxplot() +
  labs (y = "Height Above Nearest Drainage (m)")

ggplot(data = evidence,
       mapping = aes(y = ChannelDist)) +
  geom_boxplot() +
  labs (y = "Distance from Nearest Drainage Cell (m)")

# Histograms

ggplot(data = evidence,
       mapping = aes(x = TWI)) +
  geom_histogram(binwidth = 1) +
  labs (x = "Topographic Wetness Index", y = "Count")

ggplot(data = evidence,
       mapping = aes(x = Slope)) +
  geom_histogram(binwidth = 1) +
  labs (x = "Slope (degrees)", y = "Count")

# Hitting an error here with # of bins
ggplot(data = evidence,
       mapping = aes(x = HAND)) +
  geom_histogram(binwidth = 500) +
  labs (x = "Height Above Nearest Drainage (m)", y = "Count")

ggplot(data = evidence,
       mapping = aes(x = ChannelDist)) +
  geom_histogram(binwidth = 100) +
  labs (x = "Distance from Nearest Drainage Cell (m)", y = "Count")


## Distributions - Valley Bottom Observations
# Valley Bottom - Box plots
evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Slope (degrees)", x = "Valley Bottom")

# Problem here.. 
evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = HAND)) +
  geom_boxplot() +
  labs (y = "HAND", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = TWI)) +
  geom_boxplot() +
  labs (y = "Topographic Wetness Index", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = ChannelDist)) +
  geom_boxplot() +
  labs (y = "Distance from Nearest Drainage Cell (m)")

# Valley Bottom - Histograms
evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(x = Slope)) +
  geom_histogram(binwidth = 1) +
  labs (x = "Slope (degrees)", y = "Count")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(x = HAND)) +
  geom_histogram() +
  labs (y = "Count", x = "HAND")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(x = TWI)) +
  geom_histogram(binwidth = 1) +
  labs (x = "Topographic Wetness Index", y = "Count")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(x = ChannelDist)) +
  geom_histogram() +
  labs (x = "Distance from Nearest Drainage Cell (m)", y = "Count")

## Distributions - Non - Valley Bottom Observations
# Non - Valley Bottom - Box plots
evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Slope (degrees)", x = "Valley Bottom")

# Problem here.. 
evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(y = HAND)) +
  geom_boxplot() +
  labs (y = "HAND", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(y = TWI)) +
  geom_boxplot() +
  labs (y = "Topographic Wetness Index", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(y = ChannelDist)) +
  geom_boxplot() +
  labs (y = "Distance from Nearest Drainage Cell (m)", x = "Valley Bottom")

# Non-Valley Bottom - Histograms
evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(x = Slope)) +
  geom_histogram(binwidth = 1) +
  labs (x = "Slope (degrees)", y = "Count")

evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(x = HAND)) +
  geom_histogram() +
  labs (y = "Count", x = "HAND")

evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(x = TWI)) +
  geom_histogram(binwidth = 1) +
  labs (x = "Topographic Wetness Index", y = "Count")

evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(x = ChannelDist)) +
  geom_histogram(binwidth = 20) +
  labs (x = "Distance from Nearest Drainage Cell (m)", y = "Count")

## In-Channel Observations
evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(x = Slope)) +
  geom_histogram(binwidth = 1) +
  labs (x = "Slope (degrees)", y = "Count")

evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(x = TWI)) +
  geom_histogram(binwidth = 1) +
  labs (x = "TWI", y = "Count")

evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(x = ChannelDist)) +
  geom_histogram(binwidth = 5) +
  labs (x = "Distance", y = "Count") +
  xlim(NA, 100)
  
evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(x = HAND)) +
  geom_histogram(binwidth = 1) +
  labs (x = "HAND", y = "Count")

# Channel box plots
evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Slope (degrees)")

evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(y = TWI)) +
  geom_boxplot() +
  labs (y = "TWI")

evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(y = ChannelDist)) +
  geom_boxplot() +
  labs (y = "Distance")

evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(y = HAND)) +
  geom_boxplot() +
  labs (y = "HAND")

## Valley Bottom by Small, Medium, Large
evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(x = Slope, color = InputZone)) +
  geom_histogram(binwidth = 2) +
  labs (x = "Slope (degrees)", y = "Count", color = "Drainage Area Size")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(x = HAND, color = InputZone)) +
  geom_histogram() +
  labs (y = "Count", x = "HAND", color = "Drainage Area Size")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(x = TWI, color = InputZone)) +
  geom_histogram() +
  labs (x = "Topographic Wetness Index", y = "Count")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(x = ChannelDist, color = InputZone)) +
  geom_histogram() +
  labs (x = "Distance from Nearest Drainage Cell (m)", y = "Count", color = 
          "Drainage Area Size")

## In-Channel Split by Small, Medium, Large
evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(x = Slope, color = InputZone)) +
  geom_histogram(binwidth = 2) +
  labs (x = "Slope (degrees)", y = "Count", color = "Drainage Area Size")

evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(x = HAND, color = InputZone)) +
  geom_histogram() +
  labs (y = "Count", x = "HAND", color = "Drainage Area Size")

evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(x = TWI, color = InputZone)) +
  geom_histogram() +
  labs (x = "Topographic Wetness Index", y = "Count")

evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(x = ChannelDist, color = InputZone)) +
  geom_histogram() +
  labs (x = "Distance from Nearest Drainage Cell (m)", y = "Count", color = 
          "Drainage Area Size") +
  xlim(NA,100)

# Non-Valley Bottom by Small, Medium, Large
evidence %>%
  filter(categoryid %in% c(9,10)) %>%
  ggplot(mapping = aes(x = Slope, color = InputZone)) +
  geom_histogram(binwidth = 2) +
  labs (x = "Slope (degrees)", y = "Count", color = "Drainage Area Size")

evidence %>%
  filter(categoryid %in% c(9,10)) %>%
  ggplot(mapping = aes(x = HAND, color = InputZone)) +
  geom_histogram() +
  labs (y = "Count", x = "HAND", color = "Drainage Area Size")

evidence %>%
  filter(categoryid %in% c(9,10)) %>%
  ggplot(mapping = aes(x = TWI, color = InputZone)) +
  geom_histogram() +
  labs (x = "Topographic Wetness Index", y = "Count")

evidence %>%
  filter(categoryid %in% c(9,10)) %>%
  ggplot(mapping = aes(x = ChannelDist, color = InputZone)) +
  geom_histogram(binwidth = 10) +
  labs (x = "Distance from Nearest Drainage Cell (m)", y = "Count", color = 
          "Drainage Area Size")


#############################
#############################
# Test zone
# This is valley bottom slope values

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(x = Slope, color = InputZone)) +
  geom_histogram(bins = 30, fill = "white", alpha = 0.5, 
                 position = "identity") +
  labs (x = "Slope (degrees)", y = "Count", color = "Drainage Area Size",
        title = "Slope Values")

################################
################################
Ignore below this point
################################
################################
evidence %>%
  ggplot(mapping = aes(x = Slope)) +
  geom_histogram() +
  binwidth = 30 +
  labs (x = "Slope (degrees)", y = "Count")

evidence %>%
  ggplot(mapping = aes(x = HAND)) +
  geom_histogram() +
  labs (x = "HAND (m)")

observations %>%
  ggplot(mapping = aes(x = distance)) +
  geom_histogram() +
  labs (x = "Distance from Nearest Drainage Cell")

observations %>%
  ggplot(mapping = aes(x = TWI)) +
  geom_histogram() +
  labs (x = "Topograpgic Wetness Index")

# Experimenting with color-coding by users in the box plot.. still working this
# out

observations %>%
  ggplot(mapping = aes(x = Slope, y = categoryid)) +
  geom_point () +
  labs(x = "Slope", y = "Likelihood", color = "User") +
  theme_minimal() +
  theme(legend.position = "right")

observations %>%
  ggplot(mapping = aes(x = Slope, y = categoryid, color = userid)) +
  geom_point () +
  labs(x = "Slope", y = "Likelihood", color = "User") +
  theme_minimal() +
  theme(legend.position = "right") 

observations %>%
  ggplot(mapping = aes(x = Slope, color = userid)) +
  geom_boxplot() +
  labs (x = "Slope (degrees)")

# Still in progress of converting these over to new column names
evidence %>%
  ggplot(mapping = aes(x = slope, y = likelihood, color = recorder_id)) +
  geom_point () +
  labs(x = " ", y = "Likelihood", color = "User") +
  theme_minimal() +
  theme(legend.position = "right")

# Summary statistics

max(evidence$Slope)
min(evidence$Slope)
mean(evidence$Slope)
mode(evidence$Slope)
median(evidence$Slope)

max(evidence$HAND)
min(evidence$HAND)
mean(evidence$HAND)
mode(evidence$HAND)
median(evidence$HAND)

max(evidence$ChannelDist)
min(evidence$ChannelDist)
mean(evidence$ChannelDist)
mode(evidence$ChannelDist)
median(evidence$ChannelDist)

max(evidence$TWI)
min(evidence$TWI)
mean(evidence$TWI)
mode(evidence$TWI)
median(evidence$TWI)

## Distributions of Non-Valley Bottom values
# Box plots
evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Slope (degrees)", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(y = HAND)) +
  geom_boxplot() +
  labs (y = "HAND", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(y = TWI)) +
  geom_boxplot() +
  labs (y = "Topographic Wetness Index", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(9, 10)) %>%
  ggplot(mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Distance from Nearest Drainage Cell (m)", x = "Valley Bottom")

# Summary statistics

max(evidence$slope)
min(evidence$slope)
mean(evidence$slope)
mode(evidence$slope)
median(evidence$slope)

max(evidence$hand)
min(evidence$hand)
mean(evidence$hand)
mode(evidence$hand)
median(evidence$hand)

max(evidence$distance)
min(evidence$distance)
mean(evidence$distance)
mode(evidence$distance)
median(evidence$distance)

max(evidence$twi)
min(evidence$twi)
mean(evidence$twi)
mode(evidence$twi)
median(evidence$twi)

