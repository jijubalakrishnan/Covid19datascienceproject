###############################COVID19 DATA SCIENCE PROJECT 2020 ###############
###############################DATA VISUALISATIONS##############################

#**step **
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

#**step **
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
  


