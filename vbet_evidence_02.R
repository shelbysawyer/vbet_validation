# Load data and packages
work_dir <- "C:/research/transform_fun/vbet_validation/vbet_validation"
setwd(work_dir)
evidence <-read.csv("vbet_evidence_02.csv")

library(ggplot2)
library(DBI)
library(tidyverse)
library(RSQLite)

# Change data type of category to factor
evidence$categoryid = as.factor(evidence$categoryid) 

# Create SQL database
vbet_evidence_2_db <-dbConnect(RSQLite::SQLite(), "vbet_evidence_02.db")

# Create evidence table
dbExecute(vbet_evidence_2_db, "CREATE TABLE evidence (
          observationid,
          HUC8,
          categoryid,
          confidence,
          userid,
          notes,
          created_date,
          updated_date,
          category_name,
          HAND,
          ChannelDist,
          TWI,
          Slope
          );")

# Loading csv into 'evidence' table:
evidence <- read.csv("vbet_evidence_02.csv", stringsAsFactors = FALSE)
names(evidence)

dbWriteTable(vbet_evidence_2_db, "evidence", evidence, append = TRUE)

# Checking to make sure it's reading the data properly

evidence <- dbGetQuery(vbet_evidence_2_db, "SELECT * FROM evidence;")
evidence

## Distributions - All Observations
# Box plots

ggplot(data = evidence,
       mapping = aes(y = TWI)) +
  geom_boxplot() +
  labs (y = "Topographic Wetness Index", y = "Count")

ggplot(data = evidence,
       mapping = aes(x = Slope)) +
  geom_boxplot() +
  labs (x = "Slope (degrees)", y = "Count")

ggplot(data = evidence,
       mapping = aes(x = HAND)) +
  geom_boxplot() +
  labs (x = "Height Above Nearest Drainage (m)", y = "Count")

ggplot(data = evidence,
       mapping = aes(x = ChannelDist)) +
  geom_boxplot() +
  labs (x = "Distance from Nearest Drainage Cell (m)", y = "Count")

# Histograms

ggplot(data = evidence,
       mapping = aes(x = TWI)) +
  geom_histogram(binwidth = 1) +
  labs (x = "Topographic Wetness Index", y = "Count")

ggplot(data = evidence,
       mapping = aes(x = Slope)) +
  geom_histogram() +
  labs (x = "Slope (degrees)", y = "Count")

ggplot(data = evidence,
       mapping = aes(x = HAND)) +
  geom_histogram(bins = 10) +
  labs (x = "Height Above Nearest Drainage (m)", y = "Count")

ggplot(data = evidence,
       mapping = aes(x = ChannelDist)) +
  geom_histogram() +
  labs (x = "Distance from Nearest Drainage Cell (m)", y = "Count")


## Distributions - Valley Bottom Observations
# Valley Bottom - Box plots
evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Slope (degrees)", x = "Valley Bottom")

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
  ggplot(mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Distance from Nearest Drainage Cell (m)", x = "Valley Bottom")

# Valley Bottom - Histograms
evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = Slope)) +
  geom_histogram() +
  labs (y = "Slope (degrees)", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = HAND)) +
  geom_histogram() +
  labs (y = "HAND", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = TWI)) +
  geom_histogram() +
  labs (y = "Topographic Wetness Index", x = "Valley Bottom")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = ChannelDist)) +
  geom_histogram() +
  labs (y = "Distance from Nearest Drainage Cell (m)", x = "Valley Bottom")

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
  ggplot(mapping = aes(x = HAND)) +
  geom_histogram(binwidth = 1) +
  labs (x = "HAND", y = "Count")

evidence %>%
  filter(categoryid %in% c(1)) %>%
  ggplot(mapping = aes(x = Slope)) +
  geom_histogram() +
  labs (x = "Slope (degrees)", y = "Count")

###
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

