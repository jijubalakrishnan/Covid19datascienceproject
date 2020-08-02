###############################COVID19 DATA SCIENCE PROJECT 2020 ###############
###############################DATA PREPROCESSING###############################

# This file has to be run in order after running the first file
#**step3**#
#creating the first dataframe

#* step3A *#
# filtering the main dataset datewise from march to june
startdate <- as.Date("2020-03-01")
enddate <- as.Date("2020-06-30")
df1A <- Main_data[Main_data$date >= startdate & Main_data$date <= enddate,]
df1A

#* step3B *#
# Preparing the data to merge with other dataframes
include_list <- names(Main_data) %in% c("location","stringency_index",
                                        "population","population_density",
                                        "median_age","aged_65_older",
                                        "aged_70_older","gdp_per_capita",
                                        "cardiovasc_death_rate",
                                        "diabetes_prevalence",
                                        "female_smokers","male_smokers",
                                        "hand_washing_facilities",
                                        "life_expectancy",
                                        "hospital_beds_per_thousand")
df1B <- df1A[include_list]

# in the second dataframe, united states is given as us. Thus it is replaced.
index1 <- which(df1B$location=="United States")
df1B[index1,"location"] <- "US"
df1A[index1,"location"] <- "US"

# This dataset has only constant values. Thus duplicate rows are removed
# Remove duplicates rows
df1C<- df1B[!duplicated(df1B$location), ]

#* step3C *#
# reforming covid19aggcases dataframe which has to be merged.
names(covid19aggcases)[4] <- "Country"
#classifying countries by latitude groups
covid19aggcases$Lat <- round(covid19aggcases$Lat/25)
#classifying the countries by longitude groups
covid19aggcases$Long_ <- round(covid19aggcases$Long/60)

#* step3D *#
# selecting the countrywises cases,deaths,latitude and longitude from aggrecases
country_wise_cases <- sqldf("select Country,sum(confirmed) as cases, 
                            sum(deaths) as deaths, sum(recovered) as recovered, 
                            sum(active) as active,lat as lattitude, 
                            long_ as longitude from covid19aggcases 
                            group by Country")

#* step3E *#
# Forking total tests data from df1A
#maximum value of the tests should be taken countrywise
df1D <- sqldf("select location, max(total_tests) as totaltests 
              from df1A group by location")

#* step3F *#
# Merging the output dataframes obtained in the last two steps
df1E <- merge(country_wise_cases,stage2, by.x = "Country",by.y="location")