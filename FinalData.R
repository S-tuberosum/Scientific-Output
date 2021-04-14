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

#researchers per million from 2012 to 2018
researchers_million = WDI(indicator = 'SP.POP.SCIE.RD.P6', start =2012, end =2018)

#Citations per country / add the year column to the data
Citations2012 <- read_excel("./Data/scimagojr country rank 2012.xlsx")
Citations2012$year = 2012

Citations2013 <- read_excel("./Data/scimagojr country rank 2013.xlsx")
Citations2013$year = 2013

Citations2014 <- read_excel("./Data/scimagojr country rank 2014.xlsx")
Citations2014$year = 2014

Citations2015 <- read_excel("./Data/scimagojr country rank 2015.xlsx")
Citations2015$year = 2015

Citations2016 <- read_excel("./Data/scimagojr country rank 2016.xlsx")
Citations2016$year = 2016

Citations2017 <- read_excel("./Data/scimagojr country rank 2017.xlsx")
Citations2017$year = 2017

Citations2018 <- read_excel("./Data/scimagojr country rank 2018.xlsx")
Citations2018$year = 2018

#Education index per country / add the year column to the data, change col names

EducationIndex2012 <- read_excel("./Data/EducationIndex2012.xlsx")
EducationIndex2012$year = 2012

EducationIndex2013 <- read_excel("./Data/EducationIndex2013.xlsx")
EducationIndex2013$year = 2013

EducationIndex2014 <- read_excel("./Data/EducationIndex2014.xlsx")
EducationIndex2014$year = 2014

EducationIndex2015 <- read_excel("./Data/EducationIndex2015.xlsx")
EducationIndex2015$year = 2015

EducationIndex2016 <- read_excel("./Data/EducationIndex2016.xlsx")
EducationIndex2016$year = 2016

EducationIndex2017 <- read_excel("./Data/EducationIndex2017.xlsx")
EducationIndex2017$year = 2017

EducationIndex2018 <- read_excel("./Data/EducationIndex2018.xlsx")
EducationIndex2018$year = 2018
names(EducationIndex2018)[names(EducationIndex2018) == 'education_vaue'] <- 'education_value'

#now bind all the Citations and EducationIndex into 1 data set
citations <- rbind(Citations2012,Citations2013,Citations2014,Citations2015,Citations2016,
                   Citations2017,Citations2018)

eduindex <- rbind(EducationIndex2012,EducationIndex2013,EducationIndex2014,
                  EducationIndex2015,EducationIndex2016,EducationIndex2017,
                  EducationIndex2018)

#now delete some columns from the Citations & eduindex and rename some columns
drop <- c('Rank','Region')
citations = citations[!(names(citations) %in% drop)]
names(citations)[1] <- 'country'

names(eduindex)[names(eduindex) == 'Country'] <- 'country'

#--------------------------------------------
#         Create the Super data set
#--------------------------------------------


FinalData <- inner_join(population, gdp, by=c('country','year','iso2c'))
FinalData <- inner_join(FinalData, RnD_expenditure, by=c('country','year','iso2c'))
FinalData <- inner_join(FinalData, Sci_and_tech_journals, by=c('country','year','iso2c'))
FinalData <- inner_join(FinalData, researchers_million, by=c('country','year','iso2c'))

#rename some columns then add the citation data
names(FinalData)[1] <- 'symbol'
names(FinalData)[3] <- 'population'
names(FinalData)[5] <- 'gdp'
names(FinalData)[6] <- 'RnD_Expenditure'
names(FinalData)[7] <- 'sci_tech_articles'
names(FinalData)[8] <- 'researchers_per_million'

#now add the citations data
FinalData <- inner_join(FinalData, citations, by=c('country','year'))
FinalData <- inner_join(FinalData, eduindex, by=c('country','year'))


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
FinalData <- FinalData[-c(1, 14, 16:17)]


#adding our scientific output function

FinalData <- FinalData %>%
  mutate(sci_out =  (Documents*1000000)/Population)

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
rm(EducationIndex2012)
rm(EducationIndex2013)
rm(EducationIndex2014)
rm(EducationIndex2015)
rm(EducationIndex2016)
rm(EducationIndex2017)
rm(EducationIndex2018)
rm(eduindex)
rm(gdp)
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
