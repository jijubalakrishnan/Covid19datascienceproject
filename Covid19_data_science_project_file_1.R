###############################COVID19 DATA SCIENCE PROJECT 2020 ###############

#### This data science project analyses various factors influencing covid19 ##

#**step1**#
#* Importing basic R packages,libraries required in this analysis
# packages

install.packages("pwr")
install.packages("devtools")
install.packages("tidyverse")
install.packages("sqldf")
install.packages("ggpubr")
install.packages("covid19.analytics")
install.packages("ggeasy")
# need devtools for installing from the github repo
# install bioC.logs
devtools::install_github("mponce0/covid19.analytics")

library(sqldf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(utils)
library("httr")
library("readxl")
library(pwr)
library("ggpubr")
library(RColorBrewer)
library(ggeasy)
# There is a specific covid19 package in R, which is the primary source of data
# Importing the covid19 package in R
# load "covid19.analytics"
library(covid19.analytics)


#**step2**#
#* Importing the datasets 
#all records combined for "confirmed", "deaths" and "recovered" cases
covid19aggcases <- covid19.data("aggregated")
# obtain time series data for "confirmed" cases
covid19.confirmed.cases <- covid19.data("ts-confirmed")
# reads time series data for casualties
covid19.TS.deaths <- covid19.data("ts-deaths")

#* step2B *#
# europen centre for disease prevention dataset
# countrywisedata
country_wise_data <- 
read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
          na.strings = "", fileEncoding = "UTF-8-BOM")

#* step2c *#
# excess deaths dataset from financial times
excess_deaths <- read.csv("https://raw.githubusercontent.com/Financial-Times
         /coronavirus-excess-mortality-data
         /master/data/ft_excess_deaths.csv")

#* step2D *#
# Mobility trends dataset from apple
# Mobility_trends
# source - apple
mobility_trends <- read.csv("mobility_trends.csv")

#* step 2E *#
#* covid19_tests and other details dataset from open data world
#Main_data<- read.csv("https://query.data.world/s/rdyzjiy5kixescvbte7za4svh2exue", header=TRUE, stringsAsFactors=FALSE)
#Main_data <- read.csv("https://query.data.world/s/axrkmpy3d6lrhubrz5ur6d72r7zjjq", header=TRUE, stringsAsFactors=FALSE)
# alterntive
Main_data <- read.csv("maindataaug172020.csv")




#* step 2F.1 *#
# Datasets specifically for united states
# diversity data us source:governing.com
diversity <- read.csv("diversity_data.csv",header = FALSE)
#correcting the first cell and header,backtracked from step5C
diversity[1,1] <- "Alabama"
details <- c("state","population","hispanic","white","black","asian",
             "american_indian")
names(diversity) <- details


#* step 2F.2 *# 
#ethinicity_data_set_us_states_july23
#source: https://covidtracking.com/race
us_states_ethinicity <- read.csv("Race Data Entry - CRDT.csv")
us_states_abb <- read.csv("statesabb.csv")

#* step 2F.3 *#
#Covid19 fatality by age and sex, source:CDC
age_sex_data <- 
read.csv("Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State.csv")

#* step 2G *#
#additional datasets
# govt_policies
policy_data <- read_excel("govt_policy_oxford.xlsx")

#earlier snapshots obtained from worldometer
#This project does not relied on worldometer data,the data from JHU is credible
june6coviddata<- read.csv("coviddata.csv")
details <- c("rank","country","cases","newcases","totaldeaths","newdeaths",
             "recovered","activecases","serious","casepermillion",
             "deathspermillion","totaltests","testspermillion","population")
names(june6coviddata) <- details
june26coviddata <- read.csv("covid19june26.csv")
names(june26coviddata) <- details

#* step 2H *#
# Datasets later added in analysis
#* step 2H.1 *#
#average temperature of world countries
countrytemperature <- read.csv("temperature_world_countries.csv",header = FALSE)
names(countrytemperature) <- c("country","temperature")
# removing the symbols in the data
countrytemperature <- countrytemperature %>% 
mutate(country = as.character(gsub("Â", "", country)))
countrytemperature <- countrytemperature %>% 
mutate(country=sub('.', '', country))

#changing the country temperature to numeric form
countrytemperature$temperature <- as.numeric(countrytemperature$temperature)
# updating russia temperature
index <- which(countrytemperature$country=="Russia")
countrytemperature[index,2] <- -5.2
#updating other name issues
#backtracking from step9
countrytemperature[157,1] <- "US"
countrytemperature[180,1] <- "Switzerland"
countrytemperature[119,1] <- "South Africa"

#* step 2I *#
#Bcg dataset
Bcg <- read.csv("BCG.csv")
names(Bcg)[2] <- "bcgpercentage"


