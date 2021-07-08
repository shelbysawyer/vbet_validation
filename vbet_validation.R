vbet_validation<-read.csv("C:/research/transform_fun/data/vbet_validation.csv")

library(ggplot2)
library(DBI)
library(tidyverse)
library(viridis)

# Creating database

vbet_validation_db <-dbConnect(RSQLite::SQLite(), "vbet_validation.db")

# Creating observations table:

dbExecute(vbet_validation_db, "CREATE TABLE observations (
          point_id,
          slope,
          hand,
          distance,
          twi,
          likelihood,
          recorder_id
          );")

# Loading csv into 'observations' table:

observations <- read.csv("data/vbet_validation.csv", 
                    stringsAsFactors = FALSE)
names(observations)

dbWriteTable(vbet_validation_db, "observations", observations, append = TRUE)

# Checking to make sure it's reading the data properly

observations <- dbGetQuery(vbet_validation_db, "SELECT * FROM observations;")
observations

## Experimenting with color-coding users in the box plot

observations %>%
ggplot(mapping = aes(x = slope, color = recorder_id)) +
  geom_boxplot() +
  labs (x = "Slope (degrees)")

## I'd like to color code points BY recorder_id.. can't figure out how to do 
## that in a way that there is a separate color for each instead of a ramp
observations %>%
ggplot(mapping = aes(x = slope, y = likelihood, color = recorder_id)) +
  geom_point () +
  labs(x = " ", y = "Likelihood", color = "User") +
  theme_minimal() +
  theme(legend.position = "right") 

# Histogram for Evidence Rasters

ggplot(data = observations, 
       mapping = aes(x = slope)) +
  geom_histogram(bins = 10) +
  labs (x = "Slope (degrees)", y = "Count") +
  theme_light()

ggplot(data = observations, 
       mapping = aes(x = hand)) +
  geom_histogram(bins = 10) +
  labs (x = "HAND (m)", y = "Count")

ggplot(data = observations, 
       mapping = aes(x = distance)) +
  geom_histogram(bins = 10) +
  labs (x = "Distance from Drainage Cell (m)", y = "Count")

ggplot(data = observations, 
       mapping = aes(x = twi)) +
  geom_histogram(bins = 10) +
  labs (x = "Topographic Wetness Index (TWI)", y = "Count")

## The next thing I want to do is create box plots of the slope values
## but I want to split the plots out by recorder_id (there are 5)


