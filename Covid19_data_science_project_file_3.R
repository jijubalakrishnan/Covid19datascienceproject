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

#**step14**
#Checking the temperature correlation with death rate,death per million
#Death rate is normally distributed, but tempearture is not
#Thus Spearman correlation is used
T1 <- cor.test(P3$temperature,P3$deathrate,method="spearman")
T1
T2 <- cor.test(P3$temperature,P3$deathpermillion,method="spearman")
T2
# The straight results does not show any correlation.
# more analysis

#*step14B*#
# Plotting temperature analysis related findings
ggscatter(P3, x = "temperature", y = "deathpermillion", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "temperature",main="Temperature analysis 1", 
          ylab = "deathpermillion",col="red")
# This indicates that the relationship between temperature vs covid is nonlinear

#*step14C*#
# Simply plotting the graph and fitting a third degree polynomial
y <- P3$deathpermillion
x <- P3$temperature
xx <- data.frame(x,y)
plot(y~x,col="red",main="Temperature analysis 2",
     col.main="darkRed",xlab="Temperature",ylab="deaths per million",
     ylim=c(0,1000),pch=19)

#fitting a third degree polynomial
fit1 <- lm(y~poly(x,3,raw=TRUE))
predict1 <- predict(fit1)
#plotting third degree polynomial
plot(x,predict1,type="p", col="darkgreen",pch=19,main="Temperature analysis 3",
     xlab="temperature",ylab="trend")

#plotting the same thing with ggscatter
ggscatter(P3, x = "temperature", y = "deathpermillion", 
          add = "loess", conf.int = TRUE, 
          xlab = "temperature",main="Temperature analysis 4", 
          ylab = "deathpermillion",col="red")

#*14D*#
#result interpretation
#From these observations it is clear that there is no linear relationship between
#temperature and covid19. However, the trend predicted clearly indicates that the
#covid19 deaths is increased in countries where average temperature is between 10
#to 15. It is lesser in other temperature regions. That is eithere greather than 
#or less than this range. 
#This analysis has to be extended on US datasets and whole countries datasets
#For better understanding.

#*14E*#
#Doing temperature analysis on US datasets
#The dataframe for this one is D3,refer step5
#creating another dataframe P4 for operations
P4 <- D3
#adding death per million,case per million,death rate columns to this dataframe

#*14E.1 *#
#population is of character type
#there exist commas in population, which has to be removed
#changing its type
#removing the commas in population
#changing the type of cases total to numeric

P4$population <- as.numeric(P4$population)
P4$Cases_Total <- as.numeric(P4$Cases_Total)
P4$avgtemperature <- as.numeric(P4$avgtemperature)
P4$avgtemperature <- round(P4$avgtemperature)
P4 <- add_column(P4, casepermillion = 
                   round((P4$Cases_Total/P4$population)*1000000), 
                 .after = 3)
P4 <- add_column(P4, deathpermillion = 
                   round((P4$Deaths_Total/P4$population)*1000000), 
                 .after = 3)
P4 <- add_column(P4, deathrate = (P4$Deaths_Total/P4$Cases_Total)*100, 
                 .after = 3)

#*14E.2 *#
# A linear relation is not expected
# Thus fitting a curve on average temperature vs death per million in us states
y <- P4$deathpermillion
x <- P4$avgtemperature
xx <- data.frame(x,y)
plot(y~x,col="blue",main="Temperature analysis 5",
     col.main="darkgreen",xlab="Temperature",ylab="deaths per million",pch=19,
     sub="US",col.sub='blue',ylim=c(0,1000))


#*14E.3*#
#fitting a third degree polynomial
fit2 <- lm(y~poly(x,3,raw=TRUE))
predict2 <- predict(fit2)
#plotting third degree polynomial
plot(x,predict2,type="p", col="darkblue",pch=19,main="Temperature analysis 6",
     xlab="temperature",ylab="trend",sub="US",col.sub='blue')

#*14E.4*#
#plotting using ggplot
temperature <- P4$avgtemperature
deathpermillion <- predict2
df <- data.frame(temperature,deathpermillion)
df%>%
ggplot(aes(x=temperature,y=deathpermillion,main="US temperature trend"))+
  geom_line(col="blue")+
  ggtitle("United states\ntemperature trend")






lines(uk_data$deaths~capture_date,type='l',col="blue")




#plot(italy_data$deaths~capture_date,col="red",type='l',main="Italy",col.main="red",xlab="date", ylab="deaths",sub="UK",col.sub='blue',ylim=c(0,2000))