# Load data and packages
work_dir <- "C:\research\transform_fun\vbet_validation\vbet_validation"
setwd(work_dir)
evidence <-read.csv("vbet_evidence_02.csv")

library(ggplot2)
library(DBI)
library(tidyverse)
library(RSQLite)

#Change data type of userid to factor
# evidence$userid = as.factor(evidence$userid) 
#(taking this out since it wasn't working)

# Ignore for now. Creating SQL database
vbet_evidence_2_db <-dbConnect(RSQLite::SQLite(), "vbet_evidence_02.db")

# Ignore for now. Creating evidence table
dbExecute(vbet_evidence_db, "CREATE TABLE evidence (
          observationid,
          categoryid,
          userid,
          huc8,
          hand,
          distance,
          twi,
          slope
          );")

# Ignore this for now. Loading csv into 'observations' table:
observations <- read.csv("vbet_evidence_02.csv", 
                         stringsAsFactors = FALSE)
names(observations)

dbWriteTable(vbet_evidence_db, "evidence", evidence, append = TRUE)

# Checking to make sure it's reading the data properly

evidence <- dbGetQuery(vbet_evidence_db, "SELECT * FROM evidence;")
evidence

# Box plots

observations %>%
  ggplot(mapping = aes(x = Slope)) +
  geom_boxplot() +
  labs (x = "Slope (degrees)")

observations %>%
  ggplot(mapping = aes(x = HAND)) +
  geom_boxplot() +
  labs (x = "HAND (m)")

observations %>%
  ggplot(mapping = aes(x = distance)) +
  geom_boxplot() +
  labs (x = "Distance from Nearest Drainage Cell")

observations %>%
  ggplot(mapping = aes(x = TWI)) +
  geom_boxplot() +
  labs (x = "Topograpgic Wetness Index")

# Histograms

observations %>%
  ggplot(mapping = aes(x = slope)) +
  geom_histogram() +
  labs (x = "Slope (degrees)")

observations %>%
  ggplot(mapping = aes(x = hand)) +
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

