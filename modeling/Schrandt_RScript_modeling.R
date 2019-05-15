# M Schrandt
# 5/14/2019
# Module 4: Modeling
# Course website: https://daphnia.ecology.uga.edu/drakelab/?page_id=2323

#### Set Up ####
# Set working directory
setwd("C:/Users/workshop/Desktop/Schrandt/modeling")

# Load Libraries
library(tidyverse)
library(magrittr)
library(GGally)
library(modelr)

#### Task 1: Load dataset ####
load("get_LdClimatePop.Rda")

#### Task 2: Use the ggpairs function ####
 # to obtain a 4x4 summary plot of precipitation (prcp), average temperature (avtemp), # population size (size), number of Lyme disease cases (Cases)
plot <- ggpairs(ld.cl.pop,columns=c("prcp","avtemp","size","Cases"))
plot

#### Task 3: Log transform size and cases because they are clumped with many low values and few high values ####
 # Create two new columns for log10(size) and log10(cases+1) and substitute these for the original size
 # and cases supplied when you recreate the ggpairs plot. Why do we add 1 to the number of cases? (because log(0) undefined)
ld.cl.pop %<>% mutate(log10size=log10(size),
                      log10cases=log10(Cases+1))
plot2 <- ggpairs(ld.cl.pop,columns=c("prcp","avtemp","log10size","log10cases"))
plot2

#### Task 4: Temp and Precip appear to be positively correlated; let's explore that with a random subset of data ####
 # Using set.seed(222) for reproducibility, create a new data frame to be a random sample
 # (n=100 rows) of the full data frame and plot precipitation (x-axis) vs average temperature (y-axis).
set.seed(222)
sample <- ld.cl.pop %>% sample_n(100)

sample.plot <- ggplot(sample, aes(x=prcp, y=avtemp)) +
  geom_point()
sample.plot

#### Task 5: Add the best straight line to the plot using geom_smooth ####
sample.plot + 
  geom_smooth(method="lm")  #this adds linear model with the line in blue and confidence intervals (looks like it's standard error according to ggplot reference) in gray

#### Task 6: Create linear model and extract information ####
lm.tempprcp <- lm(avtemp ~ prcp, data = sample)
summary(lm.tempprcp)

 # Extract the slope
summary(lm.tempprcp)$coefficients[2,1]

 # Extract the p-value
summary(lm.tempprcp)$coefficients[2,4]

#### Task 7: What is the slope? Is it significantly different from zero? ####
 # slope = 0.0067
 # p-value = 3.19 e-06 so yes, significantly different from zero at alpha = 0.05

#### Task 8: Write a single line of code to generate a ggplot of total population size by year ####
ld.cl.pop %>% 
  group_by(year) %>%
  summarise(TotalPopulation=sum(size)) %>%
  ggplot(.) +
  geom_point(aes(x=year,y=TotalPopulation))

#### Task 9: Create a data frame called "by_state" from the main data frame, that groups by state, and inspect it ####
by_state <- ld.cl.pop %>%
  group_by(state)

#### Task 10: update this new data frame so that it is nested (simply pass it to nest) ####
by_state %<>% nest
  #notice that this will group the states into tibbles, and the tibbles are collectively called "data"

#### Task 11: display the Georgia data in the console window ####
 # To access data within the nest, use [[]]
 # For example, to access data for Georgia, which is the number 10 state in the nested dataset, use
by_state$data[[10]]

#### Task 12: Write function that takes df as object and returns linear model object that predicts size by year ####
get.PopGrowth.lm <- function(df) {
  lm(size ~ year, data=df)
}

#### Task 13: Add a column to the by_state dataframe that holds the model fit of each line/row
by_state %<>% mutate(model=purrr::map(data, get.PopGrowth.lm))  #the double colon after the package name specifies you want to use that function from that package

 # Add the residuals as another column
by_state %<>% mutate(resids=map2(data, model, add_residuals))  #map 2 takes 2 arguments (data & model) of same length and creates new data from them (residuals in this case)
  #map2 is in a second layer

#### Task 14: what is the structure of "resids"? #### 
str(by_state$resids)
 # list of tibbles

#### Task 15: Write a function that accepts an object of the type in the resids list & returns sum of absolute values ####
sum.resids <- function(x){
  sum(abs(x$resid))
}

 # Use the function to add a column called totalResid to by_state that provides total size of residuals summed over counties and years
by_state %<>% mutate(TotalResid=map(resids,sum.resids))

#### Task 16: write a function that accepts a linear model and returns the slope ####
get.slope <- function(model){
  model$coefficients[2]
}

 # Use the function to create new column that contains slope in the by_state df
by_state %<>% mutate(slope=purrr::map(model, get.slope))  #model here refers to the column model in the by_state df

# This has created yet another list-column in the by_state df
# In order to visualize, we need to un-nest the data structures

slopes <- unnest(by_state, slope)
total.resids <- unnest(by_state,TotalResid)

#### Task 17: Plot the growth rate (slope column) for each state ####
Growth.Plot <- ggplot(slopes, aes(x=state,y=slope)) + 
  geom_point() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1)) +  #this rotates x-axis label 90 degrees
  xlab("State") +
  ylab("Population growth rate")
Growth.Plot

#### Task 18: Plot the summed residuals (TotalResid column) for each state ####
Resid.Plot <- ggplot(total.resids, aes(x=state,y=TotalResid)) + 
  geom_point() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1)) +  #this rotates x-axis label 90 degrees
  xlab("State") +
  ylab("Total residuals")
Resid.Plot

#### Task 19: Repeat Tasks 9 & 10 using dataframe name by_state2
by_state2 <- ld.cl.pop %>%
  group_by(state)
by_state2 %<>% nest

#### Task 20: Write a function that accepts an element of the by_state2$data list-column and returns the spearman correlation coefficient between Lyme disease cases and precipitation ####
get.SpearCorr <- function(df) {
  suppressWarnings(cor.test(df$Cases, df$prcp, method="spearman")$estimate)  #appending $estimate will extract the Spearman correlation coefficient
}

 # use the function to get correlation coefficient for each state
by_state2 %<>% mutate(SpearmanCorr=purrr::map(data, get.SpearCorr))

 # unnest Spearmans so we can visualize it
Spearmans <- unnest(by_state2,SpearmanCorr)
Spearmans %<>% arrange(desc(SpearmanCorr))  #this will order by descending Spearman rank correlation
 
 # now we need to trick ggplot into changing the order of the factor levels (states)
Spearmans$state <- factor(Spearmans$state, levels = unique(Spearmans$state))

 # plot the correlation coefficient for each state
Rain_Disease.Plot <- ggplot(Spearmans, aes(x=state,y=SpearmanCorr)) + 
  geom_point() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1)) +  #this rotates x-axis label 90 degrees
  xlab("State") +
  ylab("Spearman correlation coefficient between Lyme disease cases and precipitation")
Rain_Disease.Plot
