#building the crime data set

library(tidyverse)
library(stringr)

#decent data set for county-level crime data in u.s. in 2016
#(https://www.kaggle.com/mikejohnsonjr/united-states-crime-rates-by-county)
us_crime <- read.csv("crime_data_w_population_and_crime_rate.csv")
str(us_crime)

#from INEGI and SNSP (mexican govt agencies) via diego valle:
#(https://elcri.men/es/datos.html)
mx_crime <- read.csv("fuero-comun-municipios.csv")
str(mx_crime)
#filtering out non-2016 data for now, to match u.s. set
mx_crime <- mx_crime %>%
  filter(stringr::str_detect(date, "2016"))

#the types of crimes are not uniform between data sets. normalizing this may require looking through some legal definitions, so for now we'll stick to murder stats, which we assume have the same definition in both countries
us_murder <- us_crime %>%
  select(county_name, MURDER)
#summing across months in the mx dataset
mx_murder <- mx_crime %>%
  filter(modalidad=="HOMICIDIOS") %>%
  group_by(state_code, state, mun_code, municipio) %>%
  summarise(murders=sum(count)) %>% ungroup()
#this should give us the number of murders in each county/municipio in 2016

#cleaning data
#mx_murder has 2457 entries and only 2319 levels in the municipio factor
mx_murder %>% select(municipio) %>% unique() %>% nrow() 
#oh, what if there are municipios in different states with the same name? how to check for this
mx_nonunique_muns <- mx_murder %>% count(municipio) %>% filter(n!=1) #86 non-uniques
mx_nonunique_muns %>% summarise(sum(n))
#224 total counts, which accounts for the discrepancy exactly (2319+224-86=2457)
#this means to merge these 2 sets have to incorporate state into municipio name in mx
mx_murder <- mx_murder %>%
  unite("municipio", c(municipio, state), sep=", ", remove=TRUE)
#looks like the state/municipio codes are basically useless without each other / a key, let's delete them for now
mx_murder <- mx_murder %>% select(municipio, murders)
#and let's add one more col to each set to distinguish which country the obs is from
#and also normalize column names
mx_murder <- mx_murder %>% transmute(country="mx", county=municipio, murders)
us_murder <- us_murder %>% transmute(country="us", county=county_name, murders=MURDER)

#finally the join
murder2016 <- full_join(us_murder, mx_murder)
write.csv(murder2016, "murder2016.csv")