# M Schrandt
# 5/13/2019
# Module 1: Data Visualization
# Course website: https://daphnia.ecology.uga.edu/drakelab/?page_id=2323

#### Set-Up ####
#set wd to mers
setwd("C:/Users/workshop/Desktop/Schrandt/mers")
getwd()

# Load packages
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)

#### Import and process the data ####
#Create R dataframe with the cases datafile and call it mers
mers = read.csv('cases.csv')

head(mers)

#Find out data type of columns
class(mers$onset)

#Reformat the dates with the lubridate package, but correct a few issues first
mers$hospitalized[890] = c("2015-02-20")
mers=mers[-471,]

mers$onset2=ymd(mers$onset)
mers$hospitalized2=ymd(mers$hospitalized)
class(mers$onset2)

#search for the earliest onset date so we can have a running numerical value of days elapsed since start of epidemic
day0=min(na.omit(mers$onset2))
#use the na.omit function in order to "ignore" cases that have a missing date - otherwise min will give us no date value

#create numeric value for the epidemic day
mers$epi.day=as.numeric(mers$onset2 - day0)
#as.numeric produces a numeric value so that we can do mathematical calculations with it

#Let's create a plot
#produce the epidemic curve - a bar plot
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#modify this plot to see how distributed among countries, using the fill aesthetic
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#modify the epidemic curve using position="fill"; this stacks bars and standardizes each to have constant height
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country), position="fill") +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#modify by adding coord_flip to flip the x and y axes
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
  coord_flip() 

#modify by adding coord_polar to give polar coordinates/radial graphic
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
  coord_polar() 

#Calculate the infectious period (onset to hospitalization) and plot histogram
mers$infectious.period=mers$hospitalized2-mers$onset2 #calculate "raw" infectious period
class(mers$infectious.period) #these data classified as "difftime"
mers$infectious.period=as.numeric(mers$infectious.period,units="days") #convert to days

#make the histogram
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#this plot produces negative infectious periods; MERS is actually transmitted in the health care setting
#sometimes so should probably have infectious period only when positive, and set to zero otherwise
mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)
#now re-plot it
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period (positive values only)',
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Investigate frequency of hospital-acquired infections of MERS
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', 
       title='Probability density for MERS infectious period (positive values only',
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Now try an area plot - shades area under curve
ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', 
       title='Probability density for MERS infectious period (positive values only',
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Experiment with a few other univariate plots
#dot plot
ggplot(data=mers) +
  geom_dotplot(mapping=aes(x=infectious.period2, fill=country)) +
  labs(x='Infectious period', y='Frequency', 
       title='Probability density for MERS infectious period (positive values only',
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#bar plot
ggplot(data=mers) +
geom_bar(mapping=aes(x=infectious.period2,fill=country)) +
  labs(x='Infectious period', y='Frequency', 
       title='Probability density for MERS infectious period (positive values only',
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#### Bivariate Plots ####
# Use corrected infectious period variable (infectious.period2) to study change in infectious period over time
# Add a curve fit using geom_smooth (hint: solve using loess method)
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping=aes(color=country)) +
  geom_smooth(method="loess") +
  scale_y_continuous(limits=c(0, 50)) +  #set y axis scale from 0-50
  labs(x='Epidemic day', y='Infectious period', 
       title='Probability density for MERS infectious period (positive values only)',
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 
 #plot infectious.period2 against time, as before, but add a separate smooth fit for each country
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point() +
  geom_smooth(method="loess",mapping=aes(color=country)) +
  scale_y_continuous(limits=c(0, 50)) +  #set y axis scale from 0-50
  labs(x='Epidemic day', y='Infectious period', 
       title='Probability density for MERS infectious period (positive values only)',
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#### Faceting ####
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping=aes(color=country)) +
  facet_wrap(~country) + 
  scale_y_continuous(limits =c(0,50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time',
       caption= "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Now subset a few of the countries
ggplot(data=subset(mers, gender %in% c('M','F') & country %in% c('KSA','Oman','Iran','Jordan','Qatar','South Korea', 'UAE'),
                   mapping=aes(x=epi.day, y=infectious.period2, color=country))) +
  geom_point(mapping=aes(x=epi.day, y=infectious.period2, color=country)) +
  facet_grid(gender~country) +
  scale_y_continuous(limits=c(0,50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by gender and country',
       caption= "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#### Study variation in the case fatality rate ####
#case fatality is the fraction of cases that end in death
#first modify the death column as we did for onset and hospitalization
mers$death2=ymd(mers$death)
mers$survived=ifelse(is.na(mers$death2),'Yes','No')

#Lets do a bar chart for the deaths by epi day and country
#First need to get frequency or percentage of deaths by country
mers2= mers%>%
  group_by(country,gender, survived) %>%
  summarise(n=n()) %>%
  mutate(Percent = n/sum(n)*100)
head(mers2)

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day,fill=survived),position="fill") +
  labs(x='Epidemic day', y='MERS Deaths',
        title='MERS case fatality rate over time',
        caption= "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
  facet_wrap(~country)

#Just plot death rate by day and country
ggplot(data=subset(mers,survived='No' & country %in% c('KSA','Oman','Iran','Jordan','Qatar','South Korea', 'UAE')),
       aes(x=epi.day,fill=country)) +
  geom_bar() +
  scale_y_continuous(limits = c(0,12))+
  labs(x='Epidemic day', y='Percent of MERS fatalities by country',
       title='MERS case fatality rate over time',
       caption= "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
  facet_wrap(~country)

#### More ... ####
#GGplot also has extensions with more geoms - they are installed separately
install.packages("ggalluvial")
library(ggalluvial)
freqplot=ggplot(data=subset(mers2, gender %in% c('M','F') & country %in% c('KSA','Oman','Iran','Jordan','Qatar','South Korea', 'UAE')),
                aes(axis1=country, axis2=gender, y=freq)) +
                scale_x_discrete(limits=c("Country", "Gender")) +
                xlab("Demographic") +
                ylab("Case Fatality Rate") +
                geom_alluvium(aes(fill=survived)) +
                geom_stratum() + geom_text(stat = "stratum", label.strata=TRUE) +
                theme_minimal() +
                ggtitle("MERS infections resulting in death",
                        "stratified by country and gender")
freqplot

epi.curve=ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(epi.curve)
