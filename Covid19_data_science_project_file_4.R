###############################COVID19 DATA SCIENCE PROJECT 2020 ###############
###################DATA VISUALISATIONS AND REMAINING ANALYSIS###################

#**step21 **
#Visualizing the worst affected countries based on number of cases
#The latest values are used, this will be updated on the Re-run of whole program

#sorting the countries from the first dataframe based on number of cases
index3 <- order(D1$cases)
V1 <- D1[index3,]
#selecting the worst affected countries
#This is indexed in ascending order
# Thus forking from the tail
V1 <- tail(V1,5)

# Visualising this using Barplot
myColors <- brewer.pal(5, "Set1")
names(myColors) <- V1$Country
# Barplot
# preventing the scientific notation in plot
options(scipen=999)
ggplot(V1, aes(x=Country, y=cases,fill=Country)) + 
  geom_bar(stat = "identity") +
  scale_colour_manual(values=myColors)+ ggtitle("Worst affected countries ") + 
  theme(plot.title=element_text(face="bold"))
#reverting the scientific notation
options(scipen=0)

#**step22**
# Visualizing the cumulative deaths of the worst affected countries
V2Us <- country_wise_data%>%
        filter(geoId == "US")
V2Us <- V2Us[c(-1),]
V2UK <- country_wise_data%>%
  filter(geoId == "UK")
V2Brazil <- country_wise_data%>%
  filter(geoId == "BR")
#finding the number of rows in the v2us to capture date
n <-nrow(V2Us)
tm <- seq(1,n,by=1)
tm
today <- Sys.Date()
capture_date <- today-tm
capture_date
V2Us$Months <- capture_date
V2UK$Months <- capture_date
V2Brazil$Months <- capture_date
#plotting the pandemic situation in various countries
ggplot(data = V2Us) + 
  geom_smooth(mapping = aes(x = Months,y = deaths,color="US"))+
  geom_smooth(data=V2UK,aes(x=Months,y=deaths,color="UK"))+
  geom_smooth(data=V2Brazil,aes(x=Months,y=deaths,color="Brazil"))+
  scale_colour_manual("", 
                      values = c("Brazil"="green", "US"="red", 
                                 "UK"="blue"))
  ggtitle("Pandemic progression US UK Brazil")
  
#**step23 **
#logarithamtic plot of number of cases in US
ggplot(data = V2Us) + 
geom_smooth(mapping = aes(x = Months,y = log(cases),color="US"),ylab="Cases")+
  labs(y="cases")+
  geom_smooth(data=V2UK,aes(x=Months,y=log(cases),color="UK"))+
  geom_smooth(data=V2Brazil,aes(x=Months,y=log(cases),color="Brazil"))+
  scale_colour_manual("", 
                      values = c("Brazil"="green", "US"="red", 
                                 "UK"="blue"))+
ggtitle("Logarithamic increase of cases")  
  
  
# Group plots by deaths per million
# arranging countries by death per million
# adding death per million to D1
D1 <- add_column(D1, deathpermillion = round((D1$deaths/D1$population)*1000000), 
                 .after = 5)
V3 <- D1 %>% 
      filter(Country !="San Marino") %>%
      filter(Country !="Andorra") %>%
      arrange(desc(deathpermillion))

# plotting the data
V3 <-head(V3,10)
V3 %>%
  ggplot(aes(x=Country, y=deathpermillion,main="Deaths per million people")) + 
  geom_bar(stat = "identity",fill = "darkred" ) +
  coord_flip()

#**step23**
#Latitude analysis
#dataframe from the covid19 library is used for this purpose

#*step24**
#classifying countries based on thier geographical position
#This was earlier classified in the first dataframe
# However it is best to cataogerise countries into 6 groups
# Far north
# North
# North equator
# South equator
# South
# Far south

# rounding based on latitude
covid19.TS.deaths$Lat <- round(covid19.TS.deaths$Lat/25)
range(covid19.TS.deaths$Lat)

# Categorsing latitude variables
Farnorth <- covid19.TS.deaths %>% filter(Lat==2)
Farsouth <- covid19.TS.deaths %>% filter(Lat==-2)
South    <- covid19.TS.deaths %>% filter(Lat==-1)
North    <- covid19.TS.deaths %>% filter(Lat==1)
Equator  <- covid19.TS.deaths %>% filter(Lat==0)

#finding the number of columns, this will change during each day
n <- ncol(covid19.TS.deaths)

#calculating the time series deaths regionwise
FarnorthTS <- sapply(Farnorth[5:n],sum)
FarsouthTS <- sapply(Farsouth[5:n],sum)
SouthTS    <- sapply(South[5:n],sum)
NorthTS    <- sapply(North[5:n],sum)
EquatorTS   <- sapply(Equator[5:n],sum)

# plotting these data for comparison
# preventing the scientific notation in plot

#**step25**
# storing all values into a dataframe
latitude <- data.frame(FarnorthTS,NorthTS,EquatorTS,SouthTS,FarsouthTS)


#**step26**
#plotting
options(scipen=999)

#**step27**
#capturing the date values for plotting
nrow(latitude)
tm <- seq(1,214,by=1)
tm
today <- Sys.Date()
capture_date <- today-tm
capture_date


nrow(capture_date)
#**step28**
#plotting
# flipping the latitude dataframe
latitude <- latitude[order(nrow(latitude):1),]
latitude$date <- caputre_date
options(scipen=999)

#**step29**
#plotting using ggplot
ggplot(latitude,aes(x=capture_date,y=FarnorthTS,group=1,color="Farnorth"))+
  geom_smooth()+
  
  ggtitle("Covid19 total deaths latitude regionwise")+
  xlab("Date")+
  ylab("total deahts")+
  geom_smooth(df=latitude,aes(x=capture_date,y=NorthTS, color="North",group=1))+
  geom_smooth(df=latitude,aes(x=capture_date,y=EquatorTS, color="Equator",group=1))+
  geom_smooth(df=latitude,aes(x=capture_date,y=SouthTS, color="South",group=1))+
  geom_smooth(df=latitude,aes(x=capture_date,y=FarsouthTS, color="Farsouth",group=1))+
  easy_remove_legend_title()

#**step30**
#Stringency index
# correlation between stringency index and covid19 is tested.
shapiro.test(D1$stringency_index)
# it is normally distriubted

# pearson correlation test
stringency_test <- cor.test(P3$temperature,P3$deathrate,method="pearson")
stringency_test
# plotting the correlation
ggscatter(D1, x = "stringency_index", y = "deathpermillion", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "stringency index",main="Lockdown Vs covid", 
          ylab = "death per million",col="lightblue")

#**step31**#
#prior corona diseases and vaccine
# The mean death per million of the world from covid19
mean(D1$deathpermillion)
# mean deathrate covid19
mean(D1$deathrate)

#filtering countries which were exposed to prior coronavirus
countries <- c("China","United Arab Emirates","Canada","Saudi Arabia",
               "Singapore")

#displaying the filtered dataframe
priorcoronacountries <- D1[D1$Country %in% countries,]
p6 <- priorcoronacountries[,c(1,7,8)]
p6$deathrate <- as.numeric(p6$deathrate)
p6$deathrate <- round(p6$deathrate,digits=2)
p6 <- rbind(p6,c("World",101.16,2.97))


#**step31**#
#Bcg correlation
Bcg <- Bcg[c(1,2)]
P7 <- merge(D1,Bcg,by.x="Country",by.y="Country",all.x=TRUE)

#normalitytest
shapiro.test(P7$bcgpercentage)

#Checking number of missing values
sum(is.na(P7$bcgpercentage))
# There are 50 countries which do not have bcg data in this dataset
# mean death per million of these countries
index7 <- is.na(P7$bcgpercentage)
index8 <- P7$bcgpercentage>75
P8 <- P7[index7,]
countries <- c("Australia","Newzealand","Ecuador","US",
               "Canada","United Kingdom","France","Spain","Sweden","Finland","Norway",
               "Italy","Czhech republic","Austria","Netherlands","Belgium","Germany",
               "Switzerland","Slovakia")
countries2 <- c("US","Netherlands","Belgium","Italy","Canada")

P9 <- P8[P8$Country %in% countries,]
# These 10 countries have no active bcg vaccine policy
mean(P9$deathpermillion)

#countries having no present or past vaccine policy
P10 <- P8[P8$Country %in% countries2,]
#calculating the mean
mean(P10$deathpermillion)

#correlation analysis
P11 <- P7[index8,]
ggscatter(P11, x = "bcgpercentage", y = "deathpermillion", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Bcg percentage",main="Bcg vs covid", 
          ylab = "death per million",col="lightgreen")


#**step32**#
#Etnicity analysis

# converting values to numeric
P12 <- sapply(D3,as.character(gsub(",","",population)))

#removing commas in columns
P12 <-D3 %>% 
  mutate(population = as.character(gsub(",", "", population)))%>%
  mutate(hispanic = as.character(gsub(",", "", hispanic)))%>%
  mutate(white = as.character(gsub(",", "", white)))%>%
  mutate(black = as.character(gsub(",", "", black)))%>%
  mutate(asian = as.character(gsub(",", "", asian)))

# converting to numeric
P13 <- P12[,3:37]
P13 <- apply(P13,2,as.numeric)
P12[,3:37] <- P13
P13 <- P12

#finding the total in US
P14 <-
sqldf("select sum(Deaths_Total) as Totaldeaths,sum(Deaths_white)
      as white,sum(Deaths_black) as black,sum(Deaths_Asian) as Asian, 
      sum(Deaths_Ethnicity_Hispanic) as hispanic,sum(population) as population, 
      sum(white)
      as whitepop,sum(black) as blackpop,sum(hispanic) as hispanicpop,sum(asian) 
      as asianpop from P13")
ethnicity <- c("white","black","asian","hispanic","total")
fatalityrate <- c(P14$white/P14$whitepop,P14$black/P14$blackpop,
                  P14$Asian/P14$asianpop,P14$hispanic/P14$hispanicpop,
                  P14$Totaldeaths/P14$population
                  )
P15 <- data.frame(ethnicity,fatalityrate)
P15$fatalityrate <- P15$fatalityrate*1000000


#**step32**#
# Australia infection
P16 <- covid19.confirmed.cases %>% filter(Country.Region=="Australia")

P16 <- P16 %>% filter(Province.State=="Victoria")
P16 <- subset(P16, select = -c(1:4) )
P17 <- colSums(P16)
# calculating the cumulative difference

# plotting the active cases
P18 <- apply(P16,1,diff)

nrow(P18)
tm <- seq(1,207,by=1)
tm
today <- Sys.Date()
capture_date <- today-tm
capture_date

# flipping and plotting the australia graph
P18 <- data.frame(P18)

P18 <- P18[order(nrow(P18):1),]
P18 <- add_column(P18,capture_date)
options(scipen=999)

#**step29**
#plotting using ggplot
ggplot(P18,aes(x=capture_date,y=P18,group=1,color="Victoria"))+
  geom_smooth()+
  xlab("Months")+
  ylab("Active cases")+
  easy_remove_legend_title()









                                        
