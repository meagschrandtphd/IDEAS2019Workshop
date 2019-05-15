# M Schrandt
# 5/15/2019
# Module 5: Project Management

# This is the test script for working with GitHub

#### Set Up ####
# Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Read in the dataset
mers = read.csv('cases.csv')

#### Process data and make plot ####
# Reformat the dates with the lubridate package, but correct a few issues first
mers$hospitalized[890] = c("2015-02-20")
mers=mers[-471,]

mers$onset2=ymd(mers$onset)
mers$hospitalized2=ymd(mers$hospitalized)
class(mers$onset2)

# Get numerical value of days elapsed since start of epidemic
day0=min(na.omit(mers$onset2))
mers$epi.day=as.numeric(mers$onset2 - day0)

# Make plot
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

## Now just adding a comment here to test the update and push functions...