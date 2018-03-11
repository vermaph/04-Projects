#Data Source:
#https://catalog.data.gov/dataset/annual-coal-consumption-by-country

#Data Cleaning
library("tidyr")
coal_consumption<-read.csv("coal_consumption.csv")
coal_consumption2<-gather(coal_consumption,X)
colnames(coal_consumption2)<-c("Countries","Year","Value")
coal_consumption2$Year<-gsub("X","",coal_consumption2$Year)
coal_consumption2$Year<-as.numeric(coal_consumption2$Year)
coal_consumption2$Value<-as.numeric(coal_consumption2$Value)

coal_consumption2$Value<-ifelse(is.na(coal_consumption2$Value),0,coal_consumption2$Value)
coal_consumption2$Value<-ifelse(coal_consumption2$Value<0,0,coal_consumption2$Value)

coal_agg_all_years<-aggregate(coal_consumption2$Value,by=list(coal_consumption2$Countries),FUN=sum)
write.csv(coal_agg_all_years,"all.csv")

#Data Visualization
library("plotly")
world<-coal_consumption2[coal_consumption2$Countries=="World",]
plot_ly(world,x = ~Year,y = ~Value, type = "scatter",mode="lines") %>% layout(title="World Coal Consumption", yaxis=list(title="Consumption in Quadrillion BTU"))

continents<-c("Africa","Antarctica","Asia & Oceania","Australia","Central & South America","Europe","North America")
continents<-coal_consumption2[coal_consumption2$Countries %in% continents,]
plot_ly(continents,x = ~Year,y = ~Value, color = ~Countries, type = "scatter",mode="lines") %>% layout(title="Consumption by continents", yaxis=list(title="Consumption in Quadrillion BTU"))


countries<-c("China","United States","India","Former U.S.S.R.","Europe")
countries<-coal_consumption2[coal_consumption2$Countries %in% countries,]
plot_ly(countries,x = ~Year,y = ~Value, color = ~Countries, type = "scatter",mode="lines") %>% layout(title="Consumption by major countries and EU", yaxis=list(title="Consumption in Quadrillion BTU"))


#Reason:

# https://www.eia.gov/todayinenergy/detail.php?id=9751