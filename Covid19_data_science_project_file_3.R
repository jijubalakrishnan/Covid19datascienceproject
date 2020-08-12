###############################COVID19 DATA SCIENCE PROJECT 2020 ###############
#**Step7**
# Dealing with NAs
# As the dataframe is made by combining values from variuos sources,
# There exists NA values for many fields for different countries.
str(D1)
# finding the total number of missing values
sum(is.na(D1))
# eliminating the NAs and retriving countires which has data for all fields
P1 <- na.omit(D1)
# This reduces the sample size significanlty, however we get full data of 21
# This was because excess deaths dataset has only values for 21 countries
# Analysis on other variables can be done excluding excess deaths related values

#**step8**
# Finding the summary and structure of P1
summary(P1)
str(P1)
# All variables except country name are numeric and integers

#**step9**
#adding country temperature to the dataframe
P2 <- merge(P1,countrytemperature, by.x = "Country",by.y="country")
# united states, southafrica and switzerland are missing
# which is corrected in the source 2H.1
# The country temperature taken here is the average temperature

#**step10**
#adding case per million,test per million and death per million columns
#The same can be applied for D1 as  well

P3 <- add_column(P2, casepermillion = round((P2$cases/P2$population)*1000000), 
                 .after = 5)
P3 <- add_column(P3, deathpermillion = round((P3$deaths/P3$population)*1000000), 
                 .after = 5)
P3 <- add_column(P3,testpermillion=round((P3$totaltests/P3$population)*1000000), 
                 .after = 7)

#**step11**
#adding test postivity,death rate and excess death rate

P3 <- add_column(P3, testpositivity =(P3$cases/P3$totaltests)*100, 
                 .after = 7)
P3 <- add_column(P3, deathrate = (P3$deaths/P3$cases)*100, 
                 .after = 7)
P3 <- add_column(P3,excessdeath_million=(P3$excessdeaths/P3$population)*1000000, 
                 .after = 7)

# #removingcolumns if necessary
# P3 <- select(P3,-c("casepermillion","deathpermillion"))

#**step12**
#correlation between temperature and casepermillion
#Checking the normality of the data
library(lattice)
histogram(P3$casepermillion~P3$temperature,col="red",xlab="temperature",
          ylab="casepermillion")
# from the histogram, it looks like the variables are normally distributed

#**step13**
#checking normality of casepermillion,deathpermillion with normality test
normality_test1 <- shapiro.test(P3$casepermillion)
normality_test1$p.value
normality_test2 <- shapiro.test(P3$deathpermillion)
normality_test2$p.value
#checking normality of deathrate and excessdeathrate
normality_test3 <- shapiro.test(P3$excessdeath_million)
normality_test3$p.value
normality_test4 <- shapiro.test(P3$deathrate)
normality_test4$p.value
# All these values are normally distributed since p value less than 0.05
#with(P3, tapply(cases , shapiro.test))
# Applying shapiro test for all values
normalitytest <- lapply(P3[2:31],shapiro.test)
shapirolist <- sapply(normalitytest, `[`, c("p.value"))
shapirolist
# By observing the list 
# temperature,cardio-vascular death rate,stringency index
# excess deaths,expected deaths,total deaths,tests positivity,recovered
# active, cases are not normally distributed.
# All other variables are normally distributed.




#plot(italy_data$deaths~capture_date,col="red",type='l',main="Italy",col.main="red",xlab="date", ylab="deaths",sub="UK",col.sub='blue',ylim=c(0,2000))