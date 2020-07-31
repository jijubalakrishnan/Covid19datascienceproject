###############################COVID19 DATA SCIENCE PROJECT 2020 ###############

#### This data science project analyses various factors influencing covid19 ##

#**step1**#
#* Importing basic R packages and libraries required in this analysis
# packages

install.packages("tidyverse")
install.packages("sqldf")
library(sqldf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(utils)
library("httr")
library("readxl")

#**step2**#
#* Importing the datasets 

#* step2A *#
# There is a specific covid19 package in R, which is the primary source of data
# Importing the covid19 package in R

install.packages("covid19.analytics")
# need devtools for installing from the github repo
install.packages("devtools")
# install bioC.logs
devtools::install_github("mponce0/covid19.analytics")
# load "covid19.analytics"
library(covid19.analytics)

#all records combined for "confirmed", "deaths" and "recovered" cases
covid19.ALL.agg.cases <- covid19.data("aggregated")
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
covid19tests <- 
read.csv("https://query.data.world/s/nech7dicznitbq7upcujuck6cmuh2d", 
                         header=TRUE, stringsAsFactors=FALSE)

#* step 2F.1 *#
# Datasets specifically for united states
# diversity data us source:governing.com
diversity <- read.csv("diversity_data.csv")
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

# datasets other than those from online, has to be kept in working directory