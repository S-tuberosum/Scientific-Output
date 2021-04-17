# Code written by Zeeshan Javed, Nichola Millman, and Omar Hassan
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
library(dslabs)
library(broom)
library(cowplot)


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

names(FinalData)[13] <- 'cit_per_doc'
names(FinalData)[15] <- 'education_index'

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

#removing columns we don't need & formatting
FinalData <- FinalData[-c(1, 14, 16:17)]
FinalData$education_index <- as.double(FinalData$education_index)

#adding our scientific output function

FinalData <- FinalData %>%
  mutate(doc_per_mil =  (Documents*1000000)/population)

#add gdp_per_capita to the Final data
FinalData = FinalData%>%
  mutate(gdp_per_capita =  (gdp/population))

#add RnD expenditure dollar amount
FinalData = FinalData%>%
  mutate(RnD_dollar_amount =  (RnD_Expenditure/100) * gdp)

#add RnD expenditure per capita to the FinalData
FinalData = FinalData%>%
  mutate(RnD_per_capita =  (RnD_dollar_amount/population))

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


#--------------------------------------------
#                 analyses
#--------------------------------------------

#-------------------------------------------------
#      Scientific Output
#-------------------------------------------------

#First lets take a look at the relationship between number of documents published per million per country over time

output_over_time = FinalData%>%
  group_by(year)%>%
  summarize(sum(Documents),mean(Documents))

#boxplot no jitter
FinalData$year = factor(FinalData$year)
ggplot(FinalData,aes(x= year, y = doc_per_mil)) +
  geom_boxplot( fill = "#9FDFF1") +
  ylab("Documents per million")

#boxplot with jitter
FinalData$year = factor(FinalData$year)
ggplot(FinalData,aes(x= year, y = doc_per_mil)) +
  geom_boxplot( fill = "#9FDFF1") +
  geom_jitter(height =0.10,width =0.10) +
  ylab("documents per million")

#linear model
fit <- lm(doc_per_mil~year, data = FinalData)
fit
confint(fit)


#Now, let's look at the relationship between citations and articles published


#boxplot not jitter
FinalData$year = factor(FinalData$year)
ggplot(FinalData,aes(x= year, y = cit_per_doc)) +
  geom_boxplot( fill = "#9FDFF1") +
  ylab("Citations per Document")

#boxplot with jitter
FinalData$year = factor(FinalData$year)
ggplot(FinalData, aes(x = year, y = cit_per_doc)) +
  geom_boxplot(fill = "#9FDFF1") +
  geom_jitter(height =0.15, width =0.15) +
  ylab("Citations per document")

#linear model
fit2 <- lm(cit_per_doc~year, data = FinalData)
fit2
confint(fit2)

#Now let's look at output vs population

#point graph over the years

FinalData%>%
  ggplot()+
  geom_point(aes(x= population/100000, y = Documents/1000))+
  xlab("Population")+
  ggtitle("Population vs Published Documents")+
  facet_wrap(~year)+
  theme(plot.title = element_text(hjust = 0.5))

#point graph zoomed in (outliars out of view)
FinalData2018%>%
  ggplot()+
  geom_point(aes(x= population/1000000, y = Documents/100))+
  coord_cartesian(ylim = c(0,3000))+
  ggtitle("Population vs Published Documents 2018")+
  theme(plot.title = element_text(hjust = 0.5))

#linear model and correlation
cor(FinalData$population,FinalData$Documents,method = "pearson")
fit3 <- lm(population~Documents, data = FinalData)
fit3
confint(fit3)

#Now lets look at the top performers

#first, lets graph citations per document vs documents per million
FinalData%>%
  ggplot()+
  geom_point(aes(x= cit_per_doc, y = doc_per_mil))+
  xlab("Citations per document")+
  ylab("Documents per million")+
  facet_wrap(~year)

cor(FinalData$cit_per_doc,FinalData$doc_per_mil,method = "pearson")
fit4 <- lm(cit_per_doc~doc_per_mil, data = FinalData)
fit4
confint(fit4)

#Now lets look at above average countries

#above mean average docs per mil dataset
above_avg2012 = FinalData2012%>%
  filter(doc_per_mil>mean(FinalData2012$doc_per_mil))

above_avg2013 = FinalData2013%>%
  filter(doc_per_mil>mean(FinalData2013$doc_per_mil))

above_avg2014 = FinalData2014%>%
  filter(doc_per_mil>mean(FinalData2014$doc_per_mil))

above_avg2015 = FinalData2015%>%
  filter(doc_per_mil>mean(FinalData2015$doc_per_mil))

above_avg2016 = FinalData2016%>%
  filter(doc_per_mil>mean(FinalData2016$doc_per_mil))

above_avg2017 = FinalData2017%>%
  filter(doc_per_mil>mean(FinalData2017$doc_per_mil))

above_avg2018 = FinalData2018%>%
  filter(doc_per_mil>mean(FinalData2018$doc_per_mil))

above_avg = full_join(above_avg2012,above_avg2013)%>%
  full_join(above_avg2014)%>%
  full_join(above_avg2015)%>%
  full_join(above_avg2016)%>%
  full_join(above_avg2017)%>%
  full_join(above_avg2018)

#graph of above average docs per mil vs citations per document

above_avg%>%
  ggplot()+
  geom_point(aes(x= cit_per_doc, y = doc_per_mil))+
  xlab("Citations per document")+
  ylab("Documents per million")+
  facet_wrap(~year)

above_avg_cor = above_avg%>%
  group_by(year)%>%
  summarize(cor(cit_per_doc,doc_per_mil,method = "pearson"))

#performing a linear regression to see if the above mean average docs_per_mil are increasing over time
fit4 <- lm(year~doc_per_mil, data = above_avg)
fit4
confint(fit4)

#Now, lets consider the top ten countries for docs per million over the years
#first let's filter the dataset for the top ten each year
top_performers2012 = FinalData2012%>%
  filter(doc_per_mil>3000)

top_performers2013 = FinalData2013%>%
  filter(doc_per_mil>3100)

top_performers2014 = FinalData2014%>%
  filter(doc_per_mil>3050)

top_performers2015 = FinalData2015%>%
  filter(doc_per_mil>3104.5)

top_performers2016 = FinalData2016%>%
  filter(doc_per_mil>3150)

top_performers2017 = FinalData2017%>%
  filter(doc_per_mil>3200)

top_performers2018 = FinalData2018%>%
  filter(doc_per_mil>3250)


top_performers = full_join(top_performers2012,top_performers2013)%>%
  full_join(top_performers2014) %>%
  full_join(top_performers2015) %>%
  full_join(top_performers2016) %>%
  full_join(top_performers2017) %>%
  full_join(top_performers2018)

#now let's do a bar graph of each year

top_performers%>%
  ggplot(aes(y= doc_per_mil,x= reorder(country,doc_per_mil), fill = country))+
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Documents per million")+
  xlab("")+
  coord_flip() + 
  facet_wrap(~year)

#Now lets look at citations per doc of the top ten countries

top_performers %>%
  ggplot(aes(y= cit_per_doc,x = country)) +
  geom_bar(stat = "identity",position = "dodge") +
  ylab("Citations per document") +
  xlab("") +
  coord_flip()


#-------------------------------------------------
#      Institutional factors vs scientific output
#-------------------------------------------------


# scatter doc/mil vs researchers/mil
FinalData %>%
  ggplot() +
  geom_point(aes(x= researchers_per_million, y = doc_per_mil)) +
  xlab("Researchers per million") +
  ylab("Documents per million") +
  facet_wrap(~year) +
  theme_minimal() 

# correlations for doc/mil vs researchers/mil
temp1 <- cor.test(FinalData2012$researchers_per_million, 
                  FinalData2012$doc_per_mil, method = 'pearson')
temp1

temp2 <- cor.test(FinalData2013$researchers_per_million, 
                  FinalData2013$doc_per_mil, method = 'pearson')
temp2

temp3 <- cor.test(FinalData2014$researchers_per_million, 
                  FinalData2014$doc_per_mil, method = 'pearson')
temp3

temp4 <- cor.test(FinalData2015$researchers_per_million, 
                  FinalData2015$doc_per_mil, method = 'pearson')
temp4

temp5 <- cor.test(FinalData2016$researchers_per_million, 
                  FinalData2016$doc_per_mil, method = 'pearson')
temp5

temp6 <- cor.test(FinalData2017$researchers_per_million, 
                  FinalData2017$doc_per_mil, method = 'pearson')
temp6

temp7 <- cor.test(FinalData2018$researchers_per_million, 
                  FinalData2018$doc_per_mil, method = 'pearson')
temp7

# scatter doc/mil vs education index
FinalData %>%
  filter(education_index > 0.6) %>%
  ggplot() +
  geom_point(aes(x= education_index, y = doc_per_mil)) +
  xlab("Education Index") +
  ylab("Documents per million") +
  facet_wrap(~year) +
  theme_minimal() 

# correlations for doc/mil vs education index
FinalData %>%
  filter(education_index > 0.6) %>%
  group_by(year)

temp8 <- cor.test(filter(FinalData2012, education_index >0.6)$education_index, 
                  filter(FinalData2012, education_index >0.6)$doc_per_mil, 
                  method = 'pearson')
temp8

temp9 <- cor.test(filter(FinalData2013, education_index >0.6)$education_index, 
                  filter(FinalData2013, education_index >0.6)$doc_per_mil, 
                  method = 'pearson')
temp9

temp10 <- cor.test(filter(FinalData2014, education_index >0.6)$education_index, 
                   filter(FinalData2014, education_index >0.6)$doc_per_mil, 
                   method = 'pearson')
temp10

temp11 <- cor.test(filter(FinalData2015, education_index >0.6)$education_index, 
                   filter(FinalData2015, education_index >0.6)$doc_per_mil, 
                   method = 'pearson')
temp11

temp12 <- cor.test(filter(FinalData2016, education_index >0.6)$education_index, 
                   filter(FinalData2016, education_index >0.6)$doc_per_mil,
                   method = 'pearson')
temp12

temp13 <- cor.test(filter(FinalData2017, education_index >0.6)$education_index, 
                   filter(FinalData2017, education_index >0.6)$doc_per_mil, 
                   method = 'pearson')
temp13

temp14 <- cor.test(filter(FinalData2018, education_index >0.6)$education_index, 
                   filter(FinalData2018, education_index >0.6)$doc_per_mil, 
                   method = 'pearson')
temp14
#-------------------------------------------------
#      Economic factors vs scientific output
#-------------------------------------------------


#graph gdp per capita vs documents per million over the years
FinalData %>%
  ggplot(aes(x = gdp_per_capita, y= doc_per_mil,group=year, color = factor(year))) +
  xlab("GDP per capita($)") +
  ylab("Documents per Million") +
  geom_line()

#Find the correlation 
temp15 <- cor.test(FinalData2012$gdp_per_capita, FinalData2012$doc_per_mil, method = 'pearson')
temp15

temp16 <- cor.test(FinalData2013$gdp_per_capita, FinalData2013$doc_per_mil, method = 'pearson')
temp16

temp17 <- cor.test(FinalData2014$gdp_per_capita, FinalData2014$doc_per_mil, method = 'pearson')
temp17

temp18 <- cor.test(FinalData2015$gdp_per_capita, FinalData2015$doc_per_mil, method = 'pearson')
temp18

temp19 <- cor.test(FinalData2016$gdp_per_capita, FinalData2016$doc_per_mil, method = 'pearson')
temp19

temp20 <- cor.test(FinalData2017$gdp_per_capita, FinalData2017$doc_per_mil, method = 'pearson')
temp20

temp21 <- cor.test(FinalData2018$gdp_per_capita, FinalData2018$doc_per_mil, method = 'pearson')
temp21

#graph RnD expenditure per capita vs documents per million over the years
FinalData %>%
  ggplot(aes(x=RnD_per_capita,y=doc_per_mil,group=year, color = factor(year))) +
  xlab("RnD expenditure per capita($)") +
  ylab("Documents per Million") +
  geom_line()


#Find the correlation 
temp22 <- cor.test(FinalData2012$RnD_per_capita, FinalData2012$doc_per_mil, method = 'pearson')
temp22

temp23 <- cor.test(FinalData2013$RnD_per_capita, FinalData2013$doc_per_mil, method = 'pearson')
temp23

temp24 <- cor.test(FinalData2014$RnD_per_capita, FinalData2014$doc_per_mil, method = 'pearson')
temp24

temp25 <- cor.test(FinalData2015$RnD_per_capita, FinalData2015$doc_per_mil, method = 'pearson')
temp25

temp26 <- cor.test(FinalData2016$RnD_per_capita, FinalData2016$doc_per_mil, method = 'pearson')
temp26

temp27 <- cor.test(FinalData2017$RnD_per_capita, FinalData2017$doc_per_mil, method = 'pearson')
temp27

temp28 <- cor.test(FinalData2018$RnD_per_capita, FinalData2018$doc_per_mil, method = 'pearson')
temp28