#SCI 2000 final project
#Last edited 10.03.2021
library(tidyverse)
library(dslabs)

#file names were not informative from, so here are the original files from world bank renamed
sci_articles_wb <- read_csv("./Data/1a01e7be-410e-4ebc-9fbd-c00fba8894e6_Data.csv")
literacy_rate_wb <- read_csv("./Data/2dcd45e0-28e1-4068-ba4f-a2ab2b61e321_Data.csv")
pop_per_country_wb <- read_csv("./Data/91dba2f1-f248-4de7-847e-1c32a55f9e66_Data.csv")
edu_spending_percent_gdp_wb <- read_csv("./Data/7436a8d4-539e-4c96-a777-c3fed107c4e7_Data.csv")
tax_revenue_wb <- read_csv("./Data/16971c6d-3a0f-486a-abb7-a19c041ac0b1_Data.csv")
resrchers_per_mil_wb <- read_csv("./Data/a0fdfff7-39fd-4031-bcaf-5b6daab003e1_Data.csv")
r_and_d_percent_gdp_wb <- read_csv("./Data/d89fb0de-971d-42e1-ae4a-a94053a8c7c7_Data.csv")
gdp_wb <- read_csv("./Data/ef298dca-e3b7-4a66-ad6e-7d8e302a4b4f_Data.csv")

#First, lets create a dataset for each variable in world bank for the years 2000, 2012-2018

#scientific and tech journal articles by year

sci_articles_wb_00 = sci_articles_wb%>%
  select(`Country Name`,'2000 [YR2000]')%>%
  rename('Scientific and technical journal articles'= '2000 [YR2000]')

sci_articles_wb_12 = sci_articles_wb%>%
  select(`Country Name`,'2012 [YR2012]')%>%
  rename('Scientific and technical journal articles'= '2012 [YR2012]')

sci_articles_wb_13 = sci_articles_wb%>%
  select(`Country Name`,'2013 [YR2013]')%>%
  rename('Scientific and technical journal articles'= '2013 [YR2013]')

sci_articles_wb_14 = sci_articles_wb%>%
  select(`Country Name`,'2014 [YR2014]')%>%
  rename('Scientific and technical journal articles'= '2014 [YR2014]')

sci_articles_wb_15 = sci_articles_wb%>%
  select(`Country Name`,'2015 [YR2015]')%>%
  rename('Scientific and technical journal articles'= '2015 [YR2015]')

sci_articles_wb_16 = sci_articles_wb%>%
  select(`Country Name`,'2016 [YR2016]')%>%
  rename('Scientific and technical journal articles'= '2016 [YR2016]')

sci_articles_wb_17 = sci_articles_wb%>%
  select(`Country Name`,'2017 [YR2017]')%>%
  rename('Scientific and technical journal articles'= '2017 [YR2017]')

sci_articles_wb_18 = sci_articles_wb%>%
  select(`Country Name`,'2018 [YR2018]')%>%
  rename('Scientific and technical journal articles'= '2018 [YR2018]')


#population by year

pop_per_country_wb_00 = pop_per_country_wb%>%
  select(`Country Name`,'2000 [YR2000]')%>%
  rename('Population'= '2000 [YR2000]')

pop_per_country_wb_12 = pop_per_country_wb%>%
  select(`Country Name`,'2012 [YR2012]')%>%
  rename('Population'= '2012 [YR2012]')

pop_per_country_wb_13 = pop_per_country_wb%>%
  select(`Country Name`,'2013 [YR2013]')%>%
  rename('Population'= '2013 [YR2013]')

pop_per_country_wb_14 = pop_per_country_wb%>%
  select(`Country Name`,'2014 [YR2014]')%>%
  rename('Population'= '2014 [YR2014]')

pop_per_country_wb_15 = pop_per_country_wb%>%
  select(`Country Name`,'2015 [YR2015]')%>%
  rename('Population'= '2015 [YR2015]')

pop_per_country_wb_16 = pop_per_country_wb%>%
  select(`Country Name`,'2016 [YR2016]')%>%
  rename('Population'= '2016 [YR2016]')

pop_per_country_wb_17 = pop_per_country_wb%>%
  select(`Country Name`,'2017 [YR2017]')%>%
  rename('Population'= '2017 [YR2017]')

pop_per_country_wb_18 = pop_per_country_wb%>%
  select(`Country Name`,'2018 [YR2018]')%>%
  rename('Population'= '2018 [YR2018]')

# r&d %gdp per year

r_and_d_percent_gdp_wb_00 = r_and_d_percent_gdp_wb%>%
  select(`Country Name`,'2000 [YR2000]')%>%
  rename('Research and development expenditure (% of GDP)'= '2000 [YR2000]')

r_and_d_percent_gdp_wb_12 = r_and_d_percent_gdp_wb%>%
  select(`Country Name`,'2012 [YR2012]')%>%
  rename('Research and development expenditure (% of GDP)'= '2012 [YR2012]')

r_and_d_percent_gdp_wb_13 = r_and_d_percent_gdp_wb%>%
  select(`Country Name`,'2013 [YR2013]')%>%
  rename('Research and development expenditure (% of GDP)'= '2013 [YR2013]')

r_and_d_percent_gdp_wb_14 = r_and_d_percent_gdp_wb%>%
  select(`Country Name`,'2014 [YR2014]')%>%
  rename('Research and development expenditure (% of GDP)'= '2014 [YR2014]')

r_and_d_percent_gdp_wb_15 = r_and_d_percent_gdp_wb%>%
  select(`Country Name`,'2015 [YR2015]')%>%
  rename('Research and development expenditure (% of GDP)'= '2015 [YR2015]')

r_and_d_percent_gdp_wb_16 = r_and_d_percent_gdp_wb%>%
  select(`Country Name`,'2016 [YR2016]')%>%
  rename('Research and development expenditure (% of GDP)'= '2016 [YR2016]')

r_and_d_percent_gdp_wb_17 = r_and_d_percent_gdp_wb%>%
  select(`Country Name`,'2017 [YR2017]')%>%
  rename('Research and development expenditure (% of GDP)'= '2017 [YR2017]')

r_and_d_percent_gdp_wb_18 = r_and_d_percent_gdp_wb%>%
  select(`Country Name`,'2018 [YR2018]')%>%
  rename('Research and development expenditure (% of GDP)'= '2018 [YR2018]')

#education spending %gdp by year

edu_spending_percent_gdp_wb_00 = edu_spending_percent_gdp_wb%>%
  select(`Country Name`,'2000 [YR2000]')%>%
  rename('Government expenditure on education, total (% of GDP)'= '2000 [YR2000]')

edu_spending_percent_gdp_wb_12 = edu_spending_percent_gdp_wb%>%
  select(`Country Name`,'2012 [YR2012]')%>%
  rename('Government expenditure on education, total (% of GDP)'= '2012 [YR2012]')

edu_spending_percent_gdp_wb_13 = edu_spending_percent_gdp_wb%>%
  select(`Country Name`,'2013 [YR2013]')%>%
  rename('Government expenditure on education, total (% of GDP)'= '2013 [YR2013]')

edu_spending_percent_gdp_wb_14 = edu_spending_percent_gdp_wb%>%
  select(`Country Name`,'2014 [YR2014]')%>%
  rename('Government expenditure on education, total (% of GDP)'= '2014 [YR2014]')

edu_spending_percent_gdp_wb_15 = edu_spending_percent_gdp_wb%>%
  select(`Country Name`,'2015 [YR2015]')%>%
  rename('Government expenditure on education, total (% of GDP)'= '2015 [YR2015]')

edu_spending_percent_gdp_wb_16 = edu_spending_percent_gdp_wb%>%
  select(`Country Name`,'2016 [YR2016]')%>%
  rename('Government expenditure on education, total (% of GDP)'= '2016 [YR2016]')

edu_spending_percent_gdp_wb_17 = edu_spending_percent_gdp_wb%>%
  select(`Country Name`,'2017 [YR2017]')%>%
  rename('Government expenditure on education, total (% of GDP)'= '2017 [YR2017]')

edu_spending_percent_gdp_wb_18 = edu_spending_percent_gdp_wb%>%
  select(`Country Name`,'2018 [YR2018]')%>%
  rename('Government expenditure on education, total (% of GDP)'= '2018 [YR2018]')

# Researchers per million ppl per year
resrchers_per_mil_wb_00 = resrchers_per_mil_wb%>%
  select(`Country Name`,'2000 [YR2000]')%>%
  rename('Researchers in R&D (per million people)'= '2000 [YR2000]')

resrchers_per_mil_wb_12 = resrchers_per_mil_wb%>%
  select(`Country Name`,'2012 [YR2012]')%>%
  rename('Researchers in R&D (per million people)'= '2012 [YR2012]')

resrchers_per_mil_wb_13 = resrchers_per_mil_wb%>%
  select(`Country Name`,'2013 [YR2013]')%>%
  rename('Researchers in R&D (per million people)'= '2013 [YR2013]')

resrchers_per_mil_wb_14 = resrchers_per_mil_wb%>%
  select(`Country Name`,'2014 [YR2014]')%>%
  rename('Researchers in R&D (per million people)'= '2014 [YR2014]')

resrchers_per_mil_wb_15 = resrchers_per_mil_wb%>%
  select(`Country Name`,'2015 [YR2015]')%>%
  rename('Researchers in R&D (per million people)'= '2015 [YR2015]')

resrchers_per_mil_wb_16 = resrchers_per_mil_wb%>%
  select(`Country Name`,'2016 [YR2016]')%>%
  rename('Researchers in R&D (per million people)'= '2016 [YR2016]')

resrchers_per_mil_wb_17 = resrchers_per_mil_wb%>%
  select(`Country Name`,'2017 [YR2017]')%>%
  rename('Researchers in R&D (per million people)'= '2017 [YR2017]')

resrchers_per_mil_wb_18 = resrchers_per_mil_wb%>%
  select(`Country Name`,'2018 [YR2018]')%>%
  rename('Researchers in R&D (per million people)'= '2018 [YR2018]')

# Literacy rate for people aged 15-24 per year

literacy_rate_wb_00 = literacy_rate_wb%>%
  select(`Country Name`,'2000 [YR2000]')%>%
  rename('Literacy rate, youth total (% of people ages 15-24)'= '2000 [YR2000]')

literacy_rate_wb_12 = literacy_rate_wb%>%
  select(`Country Name`,'2012 [YR2012]')%>%
  rename('Literacy rate, youth total (% of people ages 15-24)'= '2012 [YR2012]')

literacy_rate_wb_13 = literacy_rate_wb%>%
  select(`Country Name`,'2013 [YR2013]')%>%
  rename('Literacy rate, youth total (% of people ages 15-24)'= '2013 [YR2013]')

literacy_rate_wb_14 = literacy_rate_wb%>%
  select(`Country Name`,'2014 [YR2014]')%>%
  rename('Literacy rate, youth total (% of people ages 15-24)'= '2014 [YR2014]')

literacy_rate_wb_15 = literacy_rate_wb%>%
  select(`Country Name`,'2015 [YR2015]')%>%
  rename('Literacy rate, youth total (% of people ages 15-24)'= '2015 [YR2015]')

literacy_rate_wb_16 = literacy_rate_wb%>%
  select(`Country Name`,'2016 [YR2016]')%>%
  rename('Literacy rate, youth total (% of people ages 15-24)'= '2016 [YR2016]')

literacy_rate_wb_17 = literacy_rate_wb%>%
  select(`Country Name`,'2017 [YR2017]')%>%
  rename('Literacy rate, youth total (% of people ages 15-24)'= '2017 [YR2017]')

literacy_rate_wb_18 = literacy_rate_wb%>%
  select(`Country Name`,'2018 [YR2018]')%>%
  rename('Literacy rate, youth total (% of people ages 15-24)'= '2018 [YR2018]')

#GDP per year. NOTE: only for years 2000, 2012-2015

gdp_wb_00 = gdp_wb%>%
  select(`Country Name`,'2000 [YR2000]')%>%
  rename('GDP'= '2000 [YR2000]')

gdp_wb_12 = gdp_wb%>%
  select(`Country Name`,'2012 [YR2012]')%>%
  rename('GDP'= '2012 [YR2012]')

gdp_wb_13 = gdp_wb%>%
  select(`Country Name`,'2013 [YR2013]')%>%
  rename('GDP'= '2013 [YR2013]')

gdp_wb_14 = gdp_wb%>%
  select(`Country Name`,'2014 [YR2014]')%>%
  rename('GDP'= '2014 [YR2014]')

gdp_wb_15 = gdp_wb%>%
  select(`Country Name`,'2015 [YR2015]')%>%
  rename('GDP'= '2015 [YR2015]')

#Tax revenue %gdp  per year

tax_revenue_wb_00 = tax_revenue_wb%>%
  select(`Country Name`,'2000 [YR2000]')%>%
  rename('Tax revenue (% of GDP)'= '2000 [YR2000]')

tax_revenue_wb_12 = tax_revenue_wb%>%
  select(`Country Name`,'2012 [YR2012]')%>%
  rename('Tax revenue (% of GDP)'= '2012 [YR2012]')

tax_revenue_wb_13 = tax_revenue_wb%>%
  select(`Country Name`,'2013 [YR2013]')%>%
  rename('Tax revenue (% of GDP)'= '2013 [YR2013]')

tax_revenue_wb_14 = tax_revenue_wb%>%
  select(`Country Name`,'2014 [YR2014]')%>%
  rename('Tax revenue (% of GDP)'= '2014 [YR2014]')

tax_revenue_wb_15 = tax_revenue_wb%>%
  select(`Country Name`,'2015 [YR2015]')%>%
  rename('Tax revenue (% of GDP)'= '2015 [YR2015]')

tax_revenue_wb_16 = tax_revenue_wb%>%
  select(`Country Name`,'2016 [YR2016]')%>%
  rename('Tax revenue (% of GDP)'= '2016 [YR2016]')

tax_revenue_wb_17 = tax_revenue_wb%>%
  select(`Country Name`,'2017 [YR2017]')%>%
  rename('Tax revenue (% of GDP)'= '2017 [YR2017]')

tax_revenue_wb_18 = tax_revenue_wb%>%
  select(`Country Name`,'2018 [YR2018]')%>%
  rename('Tax revenue (% of GDP)'= '2018 [YR2018]')



#Now, lets amalgamate all of the world bank sets! Note that gdp isn't included since it gave me a hard time.(i.e multiple lines for same country)
#2000
wb_null_included_00 = merge(sci_articles_wb_00,pop_per_country_wb_00,by = "Country Name")%>%
  merge(r_and_d_percent_gdp_wb_00,by = "Country Name")%>%
  merge(edu_spending_percent_gdp_wb_00,by = "Country Name")%>%
  merge(resrchers_per_mil_wb_00,by = "Country Name")%>%
  merge(literacy_rate_wb_00, by = "Country Name")%>%
  merge(tax_revenue_wb_00,by = "Country Name")

 wb_00 = na.omit(wb_null_included_00)

 #2012
 wb_null_included_12 = merge(sci_articles_wb_12,pop_per_country_wb_12,by = "Country Name")%>%
   merge(r_and_d_percent_gdp_wb_12,by = "Country Name")%>%
   merge(edu_spending_percent_gdp_wb_12,by = "Country Name")%>%
   merge(resrchers_per_mil_wb_12,by = "Country Name")%>%
   merge(literacy_rate_wb_12, by = "Country Name")%>%
   merge(tax_revenue_wb_12,by = "Country Name")
 
 wb_12 = na.omit(wb_null_included_12)
 
 #2013
 wb_null_included_13 = merge(sci_articles_wb_13,pop_per_country_wb_13,by = "Country Name")%>%
   merge(r_and_d_percent_gdp_wb_13,by = "Country Name")%>%
   merge(edu_spending_percent_gdp_wb_13,by = "Country Name")%>%
   merge(resrchers_per_mil_wb_13,by = "Country Name")%>%
   merge(literacy_rate_wb_13, by = "Country Name")%>%
   merge(tax_revenue_wb_13,by = "Country Name")
 
 wb_13 = na.omit(wb_null_included_13)
 
 #2014
 wb_null_included_14 = merge(sci_articles_wb_14,pop_per_country_wb_14,by = "Country Name")%>%
   merge(r_and_d_percent_gdp_wb_14,by = "Country Name")%>%
   merge(edu_spending_percent_gdp_wb_14,by = "Country Name")%>%
   merge(resrchers_per_mil_wb_14,by = "Country Name")%>%
   merge(literacy_rate_wb_14, by = "Country Name")%>%
   merge(tax_revenue_wb_14,by = "Country Name")
 
 wb_14 = na.omit(wb_null_included_14)
 
 #2015
 wb_null_included_15 = merge(sci_articles_wb_15,pop_per_country_wb_15,by = "Country Name")%>%
   merge(r_and_d_percent_gdp_wb_15,by = "Country Name")%>%
   merge(edu_spending_percent_gdp_wb_15,by = "Country Name")%>%
   merge(resrchers_per_mil_wb_15,by = "Country Name")%>%
   merge(literacy_rate_wb_15, by = "Country Name")%>%
   merge(tax_revenue_wb_15,by = "Country Name")
 
 wb_15 = na.omit(wb_null_included_15)
 
 #2016
 wb_null_included_16 = merge(sci_articles_wb_16,pop_per_country_wb_16,by = "Country Name")%>%
   merge(r_and_d_percent_gdp_wb_16,by = "Country Name")%>%
   merge(edu_spending_percent_gdp_wb_16,by = "Country Name")%>%
   merge(resrchers_per_mil_wb_16,by = "Country Name")%>%
   merge(literacy_rate_wb_16, by = "Country Name")%>%
   merge(tax_revenue_wb_16,by = "Country Name")
 
 wb_16 = na.omit(wb_null_included_16)
 
 #2017
 wb_null_included_17 = merge(sci_articles_wb_17,pop_per_country_wb_17,by = "Country Name")%>%
   merge(r_and_d_percent_gdp_wb_17,by = "Country Name")%>%
   merge(edu_spending_percent_gdp_wb_17,by = "Country Name")%>%
   merge(resrchers_per_mil_wb_17,by = "Country Name")%>%
   merge(literacy_rate_wb_17, by = "Country Name")%>%
   merge(tax_revenue_wb_17,by = "Country Name")
 
 wb_17 = na.omit(wb_null_included_17)
 
 #2018
 wb_null_included_18 = merge(sci_articles_wb_18,pop_per_country_wb_18,by = "Country Name")%>%
   merge(r_and_d_percent_gdp_wb_18,by = "Country Name")%>%
   merge(edu_spending_percent_gdp_wb_18,by = "Country Name")%>%
   merge(resrchers_per_mil_wb_18,by = "Country Name")%>%
   merge(literacy_rate_wb_18, by = "Country Name")%>%
   merge(tax_revenue_wb_18,by = "Country Name")
 
 wb_18 = na.omit(wb_null_included_18)
 
wb_null_included_18 %>%
  top_n(n = 10) %>%
  ggplot(aes(`Country Name`, 
             `Scientific and technical journal articles`)) +
  geom_bar(stat = "identity") +
  coord_flip() + # For horizontal bars
  theme(axis.text.y = element_text(size = 6)) +
  xlab("")