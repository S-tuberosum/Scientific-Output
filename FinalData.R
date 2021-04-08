#--------------------------------------------
#             READ ME
#--------------------------------------------
#Before you do anything, make sure you have all of the libraries installed
#Also make sure all the provided .xlsx files are in the correct directory
#After doing the above, run the entire script at once and all the data sets will be created

#We are only using data for countries with a population greater than 1 million

#There is missing data for some countries

#Run any of your experiments at the very bottom of the script
#so that it doesn't mess with the created datasets

#The data frame labeled 'FinalData' contains all the data for all the years

#The data frame labeled 'FinalDataYYYY' contains all the data for the year YYYY

library(WDI)
library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)


#--------------------------------------------
#             GET THE DATA
#--------------------------------------------


#population of each country from 2012 to 2018
population = WDI(indicator='SP.POP.TOTL', start=2012, end=2018)

#GDP of each country from 2012 to 2018
gdp = WDI(indicator='NY.GDP.MKTP.CD', start=2012, end=2018)

#research and development expenditure (% of GDP) from 2012 to 2018
RnD_expenditure = WDI(indicator='GB.XPD.RSDV.GD.ZS', start=2012, end=2018)

#Scientific and technical journal articles from 2012 to 2018
Sci_and_tech_journals = WDI(indicator = 'IP.JRN.ARTC.SC', start=2012, end=2018)

#government expenditure on education (% of GDP) from 2012 to 2018
Gov_expenditure_edu = WDI(indicator = 'SE.XPD.TOTL.GD.ZS', start=2012, end=2018)

#researchers per million from 2012 to 2018
researchers_million = WDI(indicator = 'SP.POP.SCIE.RD.P6', start =2012, end =2018)

#Citations per country / add the year column to the data
Citations2012 <- read_excel("scimagojr country rank 2012.xlsx")
Citations2012$year = 2012

Citations2013 <- read_excel("scimagojr country rank 2013.xlsx")
Citations2013$year = 2013

Citations2014 <- read_excel("scimagojr country rank 2014.xlsx")
Citations2014$year = 2014

Citations2015 <- read_excel("scimagojr country rank 2015.xlsx")
Citations2015$year = 2015

Citations2016 <- read_excel("scimagojr country rank 2016.xlsx")
Citations2016$year = 2016

Citations2017 <- read_excel("scimagojr country rank 2017.xlsx")
Citations2017$year = 2017

Citations2018 <- read_excel("scimagojr country rank 2018.xlsx")
Citations2018$year = 2018

#now bind all the Citations into 1 data set
citations <- rbind(Citations2012,Citations2013,Citations2014,Citations2015,Citations2016,
                   Citations2017,Citations2018)

#now delete some columns from the Citations and rename some columns
drop <- c('Rank','Region')
citations = citations[!(names(citations) %in% drop)]
names(citations)[1] <- 'country'


#--------------------------------------------
#         Create the Super data set
#--------------------------------------------


FinalData <- inner_join(population, gdp, by=c('country','year','iso2c'))
FinalData <- inner_join(FinalData, RnD_expenditure, by=c('country','year','iso2c'))
FinalData <- inner_join(FinalData, Sci_and_tech_journals, by=c('country','year','iso2c'))
FinalData <- inner_join(FinalData, Gov_expenditure_edu, by=c('country','year','iso2c'))
FinalData <- inner_join(FinalData, researchers_million, by=c('country','year','iso2c'))

#rename some columns then add the citation data
names(FinalData)[1] <- 'symbol'
names(FinalData)[3] <- 'population'
names(FinalData)[5] <- 'gdp'
names(FinalData)[6] <- 'RnD_Expenditure'
names(FinalData)[7] <- 'sci_tech_articles'
names(FinalData)[8] <- 'education_expenditure'
names(FinalData)[9] <- 'researchers_per_million'
names(FinalData)[10] <- 'literacy_rate'

#now add the citations data
FinalData <- inner_join(FinalData, citations, by=c('country','year'))


#-------------------------------------------------------
#             Clean the Super data set
#-------------------------------------------------------


#now remove all countries with a population less than 1 million
FinalData <- subset(FinalData, population > 1000000)

#create data frame of countries with NA across all the years and remove them
remove <- read.delim("remove.txt")
FinalData <- anti_join(FinalData, remove, by='country')


#replacing all NA values with the sample mean of the column 
FinalData <- group_by(FinalData, country)
FinalData <- mutate(FinalData, mean_rnd = mean(RnD_Expenditure, na.rm = TRUE), 
                mean_rpm = mean(researchers_per_million, na.rm = TRUE)) 

FinalData$RnD_Expenditure <- ifelse(is.na(FinalData$RnD_Expenditure), 
                                FinalData$mean_rnd, FinalData$RnD_Expenditure)

FinalData$researchers_per_million <- ifelse(is.na(FinalData$researchers_per_million), 
                                FinalData$mean_rpm, FinalData$researchers_per_million)

#removing columns we don't need
FinalData <- FinalData[-c(1, 16, 17)]


#-------------------------------------------------------
# Delete all the 'extra' data sets that we don't need
#-------------------------------------------------------


rm(Citations2012)
rm(Citations2013)
rm(Citations2014)
rm(Citations2015)
rm(Citations2016)
rm(Citations2017)
rm(Citations2018)
rm(citations)
rm(gdp)
rm(Gov_expenditure_edu)
rm(literacyRate)
rm(drop)
rm(population)
rm(RnD_expenditure)
rm(Sci_and_tech_journals)
rm(researchers_million)


#--------------------------------------------
# Create the separate data sets for each year
#--------------------------------------------


FinalData2012 <- FinalData %>%
  filter(year == 2012)

FinalData2013 <- FinalData %>%
  filter(year == 2013)

FinalData2014 <- FinalData %>%
  filter(year == 2014)

FinalData2015 <- FinalData %>%
  filter(year == 2015)

FinalData2016 <- FinalData %>%
  filter(year == 2016)

FinalData2017 <- FinalData %>%
  filter(year == 2017)

FinalData2018 <- FinalData %>%
  filter(year == 2018)


