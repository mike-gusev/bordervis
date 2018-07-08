#building a complete usmx data set
#m.o.: import primary documents, select needed data, clean it, append to a growing dataset
library(tidyverse)
library(stringr)
library(stringi)
library(readxl)
load("data/us_region_helper.RData")

#land area first
#mx
load("data/mx_inafed_snim_population.RData")
mx_land_area_sqkm <- pob_datos %>%
  filter(municipios==0) %>% #filter out states/federal
  select("NAME_1"=estado, "NAME_2"=municipio, "area_sqkm"=superficie)
#this dataset is old and calls Coahuila by its old name 
mx_land_area_sqkm <- mx_land_area_sqkm %>%
  filter(str_detect(NAME_1, "Coahuila de Zaragoza")) %>%
  mutate(NAME_1="Coahuila") %>%
  rbind(mx_land_area_sqkm[!str_detect(mx_land_area_sqkm$NAME_1, "Coahuila de Zaragoza"),]) %>%
  arrange(NAME_1, NAME_2)
#us
us_land_area_sqkm <- read_excel("data/uscensus/LND01.xls") %>%
  select(Areaname, LND110210D) %>%
  separate(Areaname, into=c("NAME_2", "abbr"), sep=", ") %>%
  left_join(us_region_helper) %>%
  mutate(area_sqkm=LND110210D*0.3861) %>%
  select("NAME_1", "NAME_2", area_sqkm) %>%
  filter(!is.na(NAME_1))
usmx_land_area_sqkm <- rbind(us_land_area_sqkm, mx_land_area_sqkm)
border_data <- usmx_land_area_sqkm %>%
  mutate(area_sqkm=as.numeric(area_sqkm))
rm(mx_land_area_sqkm, pob_datos, us_land_area_sqkm, usmx_land_area_sqkm)

#population, murders, murder rate
#all come from same doc on mx side
load("data/mx_sesnp_crime.RData")
mx_murder_2016 <- sesnp_crime %>% filter(modalidad=="HOMICIDIOS") %>%
  separate(date, into=c("year", "month"), sep="-") %>%
  filter(year==2016) %>%
  group_by(state, municipio, year) %>%
  summarise(murder=sum(count), population=floor(mean(population))) %>%
  ungroup() %>%
  transmute(NAME_1=str_to_title(state), 
            NAME_2=str_to_title(municipio), 
            murder, population,
            murder_rate=murder*100000/population)
load("data/us_fbi_ucr_crime_2016.RData")
us_murder_2016 <- us_crime %>%
  select(county_name, "murder"=MURDER, population) %>%
  separate(county_name, into=c("NAME_2", "abbr"), sep=", ") %>%
  mutate(NAME_2=str_remove(NAME_2, " County| city| Census Area| Parish| Borough")) %>%
  left_join(us_region_helper, by="abbr") %>%
  dplyr::select(-abbr) %>%
  arrange(NAME_1, NAME_2) %>%
  mutate(murder_rate=murder*100000/population)
#bind
usmx_murder_2016 <- rbind(us_murder_2016, mx_murder_2016)
#rm(mx_murder, sesnp_crime, us_crime, us_murder_2016, mx_murder_2016)
#this dataset is missing accents/diacritics on mx side
#so to imbue it we generate a matching col in border_data to join by
border_data <- border_data %>%
  mutate(rawname1=str_to_title(stri_trans_general(NAME_1, "Latin-ASCII")),
    rawname2=str_to_title(stri_trans_general(NAME_2, "Latin-ASCII"))) %>%
  full_join(usmx_murder_2016, by=c("rawname1"="NAME_1", "rawname2"="NAME_2")) %>%
  select(-rawname1, -rawname2)
rm(usmx_murder_2016)

#pop. density
border_data <- border_data %>%
  mutate(pop_dens_sqkm = population/area_sqkm)

#gini index
#us
us_gini_2010 <- read.csv("data/us_census_acs_gini_2010.csv") %>% 
  dplyr::select("region"=GEO.display.label, "gini"=HD01_VD01) %>%
  separate(region, into=c("NAME_2", "NAME_1"), sep=", ") %>%
  mutate(NAME_2=str_remove(NAME_2, " County| Borough| Census Area| Parish"))
#mx
load("data/mx_coneval_poverty.RData")
mx_gini_2010 <- a %>% dplyr::select("NAME_1"=nom_ent, "NAME_2"=nom_mun, "gini"=gini_10)
mx_gini_2010 <- rbind(
  mx_gini_2010 %>% filter(str_detect(NAME_1, "Coahuila")) %>% mutate(NAME_1="Coahuila"),
  mx_gini_2010 %>% filter(!str_detect(NAME_1, "Coahuila"))
) %>% arrange(NAME_1, NAME_2)
#bind
usmx_gini_2010 <- rbind(us_gini_2010, mx_gini_2010)
border_data <- border_data %>%
  full_join(usmx_gini_2010, by=c("NAME_1", "NAME_2"))
rm(us_gini_2010, mx_gini_2010, usmx_gini_2010, a)

#poverty
#mx
load("data/mx_coneval_poverty.RData")
mx_poverty_2015 <- dplyr::select(a, "NAME_1"=nom_ent, "NAME_2"=nom_mun, "poverty_rate"=pobreza)
mx_poverty_2015 <- rbind(
  mx_poverty_2015 %>% filter(str_detect(NAME_1, "Coahuila")) %>% mutate(NAME_1="Coahuila"),
  mx_poverty_2015 %>% filter(!str_detect(NAME_1, "Coahuila"))
) %>% arrange(NAME_1, NAME_2) %>%
  mutate(poverty_rate=100*poverty_rate)
#us
load("data/us_census_saipe_poverty_2015.RData")
us_poverty_2015 <- pov2015[,c(4,10)]
names(us_poverty_2015) <- c("region", "poverty_rate")
us_poverty_2015 <- us_poverty_2015 %>%
  separate(region, into=c("NAME_2", "abbr"), sep=" \\(") %>%
  transmute(NAME_2=str_remove(NAME_2, " County| Borough| Census Area| Parish"),
            abbr=str_sub(abbr,1,-2), poverty_rate) %>%
  left_join(us_region_helper, by="abbr") %>%
  dplyr::select(NAME_1, NAME_2, poverty_rate) %>% 
  filter(!is.na(NAME_1), NAME_2 != "")
#bind
usmx_poverty_2015 <- rbind(us_poverty_2015, mx_poverty_2015)
border_data <- border_data %>%
  full_join(usmx_poverty_2015, by=c("NAME_1", "NAME_2"))
rm(a, mx_poverty_2015, pov2015, us_poverty_2015, usmx_poverty_2015)

border_data <- border_data %>% transmute(
  NAME_1, NAME_2,
  "area_sqkm_2010"=area_sqkm,
  "murder_2016"=murder,
  "population_2016"=floor(population),
  "murder_rate_2016"=round(murder_rate, digits=3),
  "pop_dens_sqkm_2016"=round(pop_dens_sqkm, digits=3),
  "gini_2010"=round(gini, digits=3),
  "poverty_rate_2015"=round(poverty_rate, digits=1)
)
save(border_data, file="data/border_data.RData")
