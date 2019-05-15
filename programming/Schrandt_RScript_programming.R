# M Schrandt
# 5/13/2019
# Module 2: Programming
# Course website: https://daphnia.ecology.uga.edu/drakelab/?page_id=2323

#### Set-Up ####
#set wd to the programming folder
setwd("C:/Users/workshop/Desktop/Schrandt/programming")

# Load packages
library(data.table) #to read data from an online source
library(ggplot2)
library(dplyr)

# Declare/Define Functions
 #function to calculate the mean and standard error
Calc.mean.se <- function(state="Colorado", years=1999:2007) {
  # Computes the mean and standard error of the mean for a vector
  # Colorado and the years in the function above are the default values
  #
  # Args: state: vector of state names
  #       years: vector of years to be included
  # 
  # Returns: a dataframe with state name, mean rate and se
  x <- wnv[wnv$State %in% state & wnv$Year %in% years,]
  y <- data.frame(state=x$State, ndr=x$NIDrate) #this is where would calculate NIDrate if it wasn't done before
  m=aggregate(y$ndr, by=list(y$state), FUN = mean)
  se=aggregate(y$ndr, by=list(y$state), FUN = function(x) sd(x)/sqrt(length(x)) )
  out=merge(m,se, by="Group.1")
  names(out) <- c('state', 'mean.NIDrate', 'se.NIDrate')
  return(out)
}

# Import the wnv data from the course website
wnv <- fread('https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv')
head(wnv)

#### Analyses ####
# Plot histogram of total number of cases in each state in each year; use the column 'Total' for this
wnv.cases.plot <- ggplot(data=wnv) + 
  geom_histogram(mapping=aes(x=Total), binwidth = 20) +
  labs(x='Cases', y='Frequency',
       title='Total number of cases/year',
       caption="Data from: https://diseasemaps.usgs.gov/")
wnv.cases.plot

# Plot histogram for logarithm of total number of cases
 #Option 1
log.wnv.cases.plot <- ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=Total)) +
  scale_x_log10() +
  labs(x='Cases', y='Frequency',
       title='Total number of cases/year',
       caption="Data from: https://diseasemaps.usgs.gov/")
log.wnv.cases.plot

  #Option 2
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=log(Total,10)), binwidth=0.1) +
  labs(x='log10Cases', y='Frequency',
       title='Number of cases/year',
       caption="Data from: https://diseasemaps.usgs.gov/")

# Plot the case fatality rate
# First calculate the case fatality rate by dividing fatalities by total and multiply by 100% so it's a percentage
wnv$CFR <- (wnv$Fatal / wnv$Total)

CFR.plot <- ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=CFR),binwidth=0.02) +
  labs(x='Cases', y='Frequency', 
       title='Case fatality rate of West Nile Virus by U.S. State')
CFR.plot

# Verify that the variable Total is simply the sum of the number of febrile cases, 
# neuroinvasive cases, and other cases.
#make new column that sums the 3 columns
wnv$TotalChk <- wnv$EncephMen + wnv$Fever + wnv$Other
#use logic to see if the sum of the two columns are equal
total.diffs <- sum(wnv$TotalChk != wnv$Total)
# can print a statement to the console using paste, that outputs the number of differences
print(paste('Number of Differences:', total.diffs))

# Use modular arithmetic to provide an annual case count for each state rounded
# (down) to the nearest dozen. Use modular arithmetic to extract the rounding errors associated
# with this calculate, then add the errors to obtain the total error.
# the modulus is the remainder after dividing by 12 (dozen)
wnv$Dozen <- floor(wnv$Total/12)
wnv$Mod <- wnv$Total %% 12
TotalError <- sum(wnv$Mod)
TotalError #1241

#Another way to do the same as above:
wnv$Dozen2 <- wnv$Total %/% 12 #returns the quotient of Total divided by 12 to give you dozen
wnv$Mod2 <- wnv$Total %% 12 #returns the remainder of Total divided by 12, also known as the modulus
TotalError2 <- sum(wnv$Mod2)
print(paste('Total error:',TotalError2))

#### Functions ####
# THIS DOES WORK AND IS CORRECT! Yay!
# Let us call the ratio of meningitis/encephalitis cases to the total number of cases 
# the neuroinvasive disease rate (NIDrate).
# I first calculated the NIDrate outside the function
wnv$NIDrate <- wnv$EncephMen/wnv$Total
# Function is line 15 of this code

# Then call up the function to get disease for 3 states
disease <- Calc.mean.se(state=c("California", "Colorado", "New York"))
ggplot(disease, aes(x=state, y=mean.NIDrate, fill=state)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean.NIDrate-se.NIDrate,ymax=mean.NIDrate+se.NIDrate)) +
  labs(x='State', y='Neuroinvasive disease rate',
       title='Neuroinvasive disease rate, 1999-2007 (mean +/- se)')

# Now apply the function to all the states in the dataset
# get vector of all the state names
all.states=unique(wnv$State)
disease.all <- Calc.mean.se(state=all.states)

# Plot for all states
ggplot(disease.all, aes(x=state, y=mean.NIDrate, fill=state)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean.NIDrate-se.NIDrate,ymax=mean.NIDrate+se.NIDrate)) +
  labs(x='State', y='Neuroinvasive disease rate',
       title='Neuroinvasive disease rate, 1999-2007 (mean +/- se)')

#### Use pipes to produce the same plots without using your function ####
wnv %>%
  filter(State %in% c('California','Colorado','New York')) %>%
  group_by(State) %>%
  summarize(mean.NIDrate = mean(EncephMen/Total), 
            se.NIDrate = sd(EncephMen/Total)/sqrt(length(EncephMen/Total))) %>%
  ggplot(aes(x=State, y=mean.NIDrate, fill=State)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean.NIDrate-se.NIDrate, ymax=mean.NIDrate+se.NIDrate)) +
  labs(x='State', y='Neuroinvasive disease rate',
       title='Neuroinvasive disease rate, 1999-2007 (mean +/- se)')

# Work in progress...want to clean up x-axis a bit
wnv %>%
  filter(State %in% unique(State)) %>%
  group_by(State) %>%
  summarize(mean.NIDrate = mean(EncephMen/Total), 
            se.NIDrate = sd(EncephMen/Total)/sqrt(length(EncephMen/Total))) %>%
  ggplot(aes(x=State, y=mean.NIDrate, fill=State)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean.NIDrate-se.NIDrate, ymax=mean.NIDrate+se.NIDrate)) +
  labs(x='State', y='Neuroinvasive disease rate',
       title='Neuroinvasive disease rate, 1999-2007 (mean +/- se)',
       caption="Data from: https://diseasemaps.usgs.gov/")
