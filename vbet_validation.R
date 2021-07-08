vbet_validation<-read.csv("C:/research/transform_fun/data/vbet_validation.csv")

## Basic box plot-- this works, but it's ugly
boxplot(vbet_validation$slope)

library(ggplot2)
library(DBI)
library(tidyverse)

# Creating database

vbet_validation_db <-dbConnect(RSQLite::SQLite(), "vbet_validation.db")

# Creating 'observations' table:

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
## I keep getting an error message: object 'slope' not found

observations %>%
ggplot(mapping = aes(y = slope, color = recorder_id)) +
  geom_boxplot() +
  labs (x = "Slope (degrees)", y = "", color = "Recorder ID") +
  scale_color_discrete(labels = c("1",
                                  "2",
                                  "3",
                                  "4",
                                  "5")) +
  theme_minimal()+
  theme(legend.position = "bottom")

TEST 
