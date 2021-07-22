# Load data and packages
work_dir <- "C:/research/transform_fun/vbet_validation/vbet_validation"
setwd(work_dir)
evidence <-read.csv("vbet_evidence_02.csv")

library(ggplot2)
library(DBI)
library(tidyverse)
library(RSQLite)

#Change data type of userid to factor
evidence$categoryid = as.factor(evidence$categoryid) 
#(taking this out since it wasn't working)

# Ignore for now. Creating SQL database
vbet_evidence_2_db <-dbConnect(RSQLite::SQLite(), "vbet_evidence_02.db")

# Ignore for now. Creating evidence table
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

# Checking query ability

ggplot(data = evidence,
       mapping = aes(x = TWI)) +
  geom_histogram() +
  labs (x = "Topographic Wetness Index", y = "Count")


# grouped boxplot
ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot()

# Box plots

ggplot(data = evidence,
       mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Slope (degrees)")

evidence %>%
  filter(categoryid %in% c(1, 2, 3, 4, 5, 6, 8)) %>%
  ggplot(mapping = aes(y = Slope)) +
  geom_boxplot() +
  labs (y = "Slope (degrees)", x = "Valley Bottom")

evidence %>%
  ggplot(mapping = aes(y = HAND)) +
  geom_boxplot() +
  labs (y = "HAND (m)")

evidence %>%
  ggplot(mapping = aes(y = ChannelDist)) +
  geom_boxplot() +
  labs (y = "Distance from Nearest Drainage Cell")

evidence %>%
  ggplot(mapping = aes(y = TWI)) +
  geom_boxplot() +
  labs (y = "Topographic Wetness Index")

# Histograms

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

# Histogram for Evidence Rasters

ggplot(data = observations, 
       mapping = aes(x = Slope)) +
  geom_histogram(bins = 10) +
  labs (x = "Slope (degrees)", y = "Count") +
  theme_light()

ggplot(data = observations, 
       mapping = aes(x = HAND)) +
  geom_histogram(bins = 10) +
  labs (x = "HAND (m)", y = "Count")

ggplot(data = observations, 
       mapping = aes(x = ChannelDist)) +
  geom_histogram(bins = 10) +
  labs (x = "Distance from Drainage Cell (m)", y = "Count")

ggplot(data = observations, 
       mapping = aes(x = TWI)) +
  geom_histogram(bins = 10) +
  labs (x = "Topographic Wetness Index (TWI)", y = "Count")

## Distributions of Valley Bottom values
# Box plots
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
  labs (y = "Slope (degrees)", x = "Valley Bottom")

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

