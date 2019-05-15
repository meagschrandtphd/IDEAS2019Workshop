# M Schrandt
# 5/14/2019
# Module 3: Data wrangling
# Course website: https://daphnia.ecology.uga.edu/drakelab/?page_id=2323

#### Set Up ####
# Set working directory
setwd("C:/Users/workshop/Desktop/Schrandt/wrangling")

# Load Libraries
library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)

#### Task 1: Read in all three datasets (available on the course website as .csv files) ####
ld <- read_csv("lyme.csv")
pop <- read_csv("pop.csv")
prism <- read_csv("climate.csv")

#### Task 2: Inspect the pop data for why it doesn't conform to tidy data #### 
 # The yearly population sizes are listed as columns, not rows 

#### Task 3: Run code to make the pop data 'tidy' ####
pop %<>% select(fips,starts_with("pop2"))  #from the 'pop' dataset, select the columns fips and any that start with pop2
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit  #make wide data long and make str_year a column identifier and fill values of size
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))  #replace the string 'pop' in the str_year column with "" (nothing), and call that column year
pop %<>% mutate(year=as.integer(year))   #change the year column to an integer data type
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))  #the ^ here means 'starts with' 0, so any fips that starts with zero is going to have leading zero removed
pop %<>% mutate(fips=as.integer(fips))  #change the fips column to an integer data type

 # If you wanted to remove state-wide info at this point,
 # you could filter for any fips that do not end in '000' (fips ending in '000' are state-level)
 # pop2 <- pop %>% filter(! str_ends(as.character(fips),'000')), which is saying to filter where the string does not end in 000 
 # If you wanted to remove the column str_year,
 # you could use the select(-str_year) function in dplyr

#### Task 4: Write code chunk to convert the lyme disease data to tidy format
ld %<>% gather(starts_with("Cases"),key="Case_year",value="Cases") %>% na.omit  #get into long format
ld %<>% mutate(year=str_replace_all(Case_year,"Cases",""))  ##using replace to remove the Cases from year
ld %<>% mutate(year=as.integer(year))  #change year colun to integer

 # write function to make a full fips code to each county
make.fips <- function(st, cty) {
  if (str_length(cty)==3){
    fips <- paste(as.character(st),as.character(cty),sep="") %>% as.integer
  }
  else if (str_length(cty)==2){
    fips <- paste(as.character(st), "0", as.character(cty), sep="") %>% as.integer
  }
  else if (str_length(cty)==1){
    fips <- paste(as.character(st), "00", as.character(cty), sep="") %>% as.integer
  }
  return(fips)
}

 # apply this fips code to the entire dataset, by row
ld %<>% rowwise() %>%
  mutate(fips=make.fips(STCODE,CTYCODE))

 # now we want to rename STNAME and CTYNAME columns
ld %<>% rename(state=STNAME,county=CTYNAME)

#### Task 5: join the lyme disease and climate/PRISM datasets to retain county-year combos that have both disease and climate info ####
ld.cl <- inner_join(ld,prism)  #inner joing works like a SQL inner join, retaining obs that appear in both datasets

#### Task 6: write additional line of code to join the demographic data with these ####
ld.cl.pop <- inner_join(ld.cl,pop)

#### Task 7: write two lines of code to determine how many cases of Lyme disease were reported each year and average by state, county, year ####
cases.by.year <- ld %>% ungroup %>% group_by(year) %>%
  summarise(Total=sum(Cases)) %>% arrange(desc(Total))  #the worst year was 2009
avg.cases.by.state <- ld %>% ungroup %>% group_by(state) %>%
  summarise(AveCases=mean(Cases)) %>% arrange(desc(AveCases))  #3 most impacted states are: Connecticut, Massachusetts, New Jersey

#### Task 8: save the combined dataframe as an R data file and as a .csv file ####
save(ld.cl.pop, file = "get_LdClimatePop.Rda")  #tried using .Rdata but it saved the workspace it looked like, not the dataset???
write_csv(ld.cl.pop, "LdClimatePop.csv")

#### Task 9: annotate the following code chunk for mapping ####
county_map <- map_data("county")  #use the county map in the maps package and turn into data frame
state_map <- map_data("state")  #use the state map in the maps package and turn into data frame
ag.fips <- group_by(ld.cl.pop,fips)  #group the ld.cl.pop dataset by fips code since we will plot by fips
ld.16y<-summarize(ag.fips,all.cases=sum(Cases))  #get some of cases by fips code
ld.16y<-left_join(select(ld.cl.pop,c(state,county,fips)),ld.16y)  #join the two datasets to retain state and county names
ld.16y<-distinct(ld.16y)  #just give the distinct rows
ld.16y %<>% rename(region=state,subregion=county)  #rename state to region and county to subregion
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")  #strip off the "County" part from the subregion so Alachua County becomes Alachua
ld.16y$region<-tolower(ld.16y$region)  #change region to lower-case
ld.16y$subregion<-tolower(ld.16y$subregion)  #change subregion to lower-case
ld.16y$subregion<-str_replace_all(ld.16y$subregion," parish","")  #strip off the "parish" part of any names
ld.16y %<>% mutate(log10cases=log10(1+all.cases))  #perform log(x+1) transformation on the number of Cases
map.ld.16y<-left_join(county_map,ld.16y)  #join the county map lat/long info with the ld.16y dataframe
ggplot(map.ld.16y)+geom_polygon(aes(long,lat,group=group,fill=log10cases),color="gray",lwd=0.2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)))  #use ggplot to map the polygons using long/lat, fill with log# cases using a gradient fill