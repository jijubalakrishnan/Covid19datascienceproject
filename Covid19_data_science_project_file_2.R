###############################COVID19 DATA SCIENCE PROJECT 2020 ###############
###############################DATA PREPROCESSING###############################

# This file has to be run in order after running the first file
#**step3**#
#creating the first dataframe

#* step3A *#
#filtering the main dataset datewise from march to june
startdate <- as.Date("2020-03-01")
enddate <- as.Date("2020-08-11")
df1A <- Main_data[Main_data$date >= startdate & Main_data$date <= enddate,]
#df1A <- Main_data

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
df1E <- merge(country_wise_cases,df1D, by.x = "Country",by.y="location")

#* step3G *#
# filtering excess deaths dataset
excess_deaths_country_wise <- 
sqldf("select country, sum(deaths) as totaldeaths,
         sum(expected_deaths) as expecteddeaths,
      sum(excess_deaths) as excessdeaths 
      from excess_deaths where year =2020 and month between 3 and 7 
      group by country")

#* step3H *#
# Merging the excess deaths dataset to the above dataset.
# Since this dataset is not available for all countries,all.x is given as TRUE
df1F<- merge(df1E,excess_deaths_country_wise,by.x="Country",
             by.y="country",all.x = TRUE)
#* step3I *#
# Combining the above dataframe to the filtered dataframe from the main data
df1G <- merge(df1F,df1C,by.x = "Country",by.y="location")

# The first dataframe is stored into a dataframe D1
# In case of new additions or modifications, it can be impliemented from df1G
D1 <- df1G


#**step4**#
#Creating the second dataframe

#*step4A*#
#Filtering the mobility trends dataset
# keeping only countrywise data
filter1 <- mobility_trends$geo_type=="city"
df2A <- mobility_trends[!filter,]

#*step4B*#
#This dataset has to be merged with timeseries confirmed cases data
#The second dataset is updated daily.
#Thus dates common to both datasets are only used.
ncol(df2A)
ncol(covid19.confirmed.cases)
n <- ncol(covid19.confirmed.cases)
df2B <- subset(covid19.confirmed.cases, select = -c(1,3:4,89:n) )
df2C <- subset(df2A,select = -c(1,4:12))
ncol(df2C)

#*step4C*#
#Modifying column names
names(df2C)[2] <- "transporttype_casenumbers"
df2B <- add_column(df2B,transportationtype_casenumbers="cases", .before = 2)
ncol(df2B)
names(df2C)[1] <- "country"
names(df2B)[1] <- "country"

#*step4D*#
# making the names of the date columns equal
startdate <- as.Date("2020/01/22")
datevalue <- seq(startdate,by="day",length.out=84)
datevalue <- as.character(datevalue)
names(df2B)[3:86]<- datevalue
names(df2B)[2] <- "transporttype_casenumbers"
names(df2C)[3:86] <- datevalue

#*step4E*#
# merging the dataframes
df2D <- rbind(df2C,df2B)
# future modifications can be done on df2D or prior dataframes
D2 <- df2D

#**step5**#
# Creating the third dataframe
# refer step 2F.1 on file1

#*step5A*#
# Filtering the value for the latest date.
# When data is modified, this part should be updated
attach(us_states_ethinicity)
us_states_ethinicity <- subset(us_states_ethinicity,Date==20200722)
us_states_ethinicity

df3A <- merge(diversity,us_states_abb,by.x="state",by.y="State",all.x = TRUE)
df3B <- us_states_ethinicity[c(-1)]
names(df3B)[1] <- "Abbreviation"
df3C <- merge(df3A,df3B,by.x="Abbreviation",by.y="Abbreviation")

#*step5B
# Adding latitude and longitude data of us states
df3D <- sqldf("select Province_State,Lat,Long_ from covid19aggcases")
# merging this with us the above dataset
df3E <- merge(df3C,df3D,by.x="state",by.y="Province_State")
# Remove duplicates rows
df3F <- df3E[!duplicated(df3E$state), ]

#*step5C
# Adding annual temperature data of US states
# source: https://www.currentresults.com
df3G <- read.csv("usstaatesavgtemperature.csv")
df3G <- df3G[-c(2,4)]
names(df3G)[2] <- "avgtemperature"
#merging
df3H <- merge(df3F,df3G,by.x="state",by.y="State")
# Two states are missing
df3I <- merge(df3F,df3G,by.x="state",by.y="State",all.x = TRUE)
# which are Wyoming and district of columbia
# Wyoming has an average annual tempearure 7.55 c
# It is also found that alabama was missing in the dataframe
# This was because first row was read as header in R by default
# It is changed in session 2F.1
# Since district of columiba is not a state, its rows is removed
# Wyoming tempearature data is updated.
index2 <- which(df3I$state=="District of Columbia")
df3J <- df3I[-c(index2),]
df3J[50,37] <- 7.5
# storing second dataframe
D3 <- df3J

#**Step6**
#Creating the fourth dataframe
#Refer step 2F.3
#The dataset contains information about all states
#Coviddata based on age and sex from all states are filtered.
filter2 <- age_sex_data$State=="United States" 
df4A <- age_sex_data[filter2,]
df4B <- df4A[-c(1:3,13)]
D4 <- df4B
