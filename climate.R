library(maps)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Q1
#a
load("hurdat2_tidy.RData")
hurrs %>% filter(name == "KATRINA", year == 2005)
katrina = hurrs %>% filter(id == "AL122005")

#b
#Hurdat2_summary = function(hurrs){
#return the total number of storms (count distant id)
#return the range of years covered (max year - min year?)
#return a vector containing the number of track points for each storm (for loop to vector count the number of appearance in each id)

count(unique(hurrs[c("id")]))
max(hurrs["year"])-min(hurrs["year"])
list(print(stormstotal),max(hurrs["year"])-min(hurrs["year"]))
abc = hurrs %>% group_by(id) %>% summarise(amount = n())

Hurdat2_summary = function(x){
  stormstotal = count(unique(x[c("id")]))
  yearcoverd = max(x["year"])-min(x["year"])
  trackpoint = x %>% group_by(id) %>% summarise(amount = n())
  
  notrack = trackpoint[1, ]
  for (i in 2:dim(trackpoint)[1]){
    notrack = c(notrack,trackpoint[i,])
  }
  
  hrlist=list("Number of Storm"= stormstotal, "Year Covered"= yearcoverd, "Number of Track Point" = notrack)
  return(hrlist)
}
######################################
Hurdat2_summary_year = function(x,y){
  b = x %>% filter(year == y)
  stormstotal = count(unique(b[c("id")]))
  yearcoverd = max(b["year"])-min(b["year"])
  trackpoint = b %>% group_by(id) %>% summarise(amount = n())
  notrack = trackpoint[1, ]
  for (i in 2:dim(trackpoint)[1]){
    notrack = c(notrack,trackpoint[i,])
  }
  
  hrlist=list("Number of Storm"= stormstotal, "Year Covered"= yearcoverd, "Number of Track Point" = notrack)
  return(hrlist)
}
hurrs %>% group_by(id) %>% summarise(amount = n())
Hurdat2_summary(hurrs)
Hurdat2_summary_year(hurrs,2010)
Hurdat2_summary(katrina)

#c
#Make a histogram of the number of track points, using the output of your function for the hurrs case.
#number of track points by year? by each hurr? 
hr = Hurdat2_summary(hurrs)
trackpoint = hurrs %>% group_by(id) 
ggplot(trackpoint,aes(id)) + geom_bar()

#d What year did they begin naming hurricanes and other storms? (Answer this with R code and the hurrs dataframe–you can check if you’re right using Google).
hurrs %>% filter(name != "UNNAMED") %>% arrange(year)
#They started to name hurricanes since Year 1950

#Q2
#a
install.packages("maps")

#first hurricaneBasemap would be the name of the new function
hurricaneBasemap = function() {
  #within the function open the package "maps" which is using to draw maps as it provides map outlines or city points
  library(maps)
  #set variable "world" and using map_data function from ggplot and plot in the "world" data from "maps"package
  world = map_data("world")
  #starting to plot with ggplot and name the plot as basemap using to release the plot later
  basemap = ggplot() +
    #use coord_fixed to make sure the aspect ratio is correct and use xlim and ylim to chop off unwanted part from the mapand zoom in
    coord_fixed(xlim=c(-130,30),ylim=c(0,90)) +
    #use geom_polygon to draw the basemap layer, data is the data from the "world" dataset we set before to be display, aes x, y and group is data from dataset, and edge color and filling color can also be specified
    geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
  #the layer of the world map without aplot is finished
  return(basemap)
}
#use the function set above
hurricaneBasemap() +
  #base on the map from the hurricaneBasemap function, we on top of their plot another layer on the map with the "hurrs" data set and use the data from "hurrs" to display data on the new layer
  geom_path(data=hurrs, aes(x=longitude,y=latitude,group=id,color=windspeed)) +
  #provide color setting to the new layer of the "windspeed" factor, as it is a continuous data, we can specifed the color palette, display mode and color tone pattern
  scale_color_distiller(type="seq",direction=1,palette="YlOrRd")

#b
#write function to input year
hurricanes_year= function(x){
  yearset= hurrs %>% filter( year == x)
  hurricaneBasemap() +
    geom_path(data=yearset, aes(x=longitude,y=latitude,group=id,color=windspeed)) +
    scale_color_distiller(type="seq",direction=1,palette="YlOrRd")
}

hurricanes_year(1992)
#great,it works!

#Q3
library(maps)
cities = world.cities

#a
#x is longitude and y latitude

findD = function(xx,yy,x,y){
  dy = yy-y
  dx = (xx-x)*cos((pi/180)*((y+yy)/2))
  distance = 111.325*(sqrt((dx**2)+(dy**2)))
  print("The Distance between two cities is")
  return(distance)
}
findD(-14.32,-178.12,-13.65,-172.12)
#worked

#b
x = Taipei
city = quote(x)
long = cities %>% filter(name == "Taipei") %>% select(long)
lat = cities %>% filter(name == "Taipei") %>% select(lat)
as.numeric(long)

findD_city = function(x,y,city){
  cx = cities %>% filter(name == city ) %>% select(long)
  cx1=as.numeric(cx)
  cy = cities %>% filter(name == city ) %>% select(lat)
  cy1=as.numeric(cy)
  dy = cy1-y
  dx = (cx1-x)*cos((pi/180)*((y+cy1)/2))
  distance = 111.325*(sqrt((dx**2)+(dy**2)))
  print("The Distance between two cities is")
  return(distance)
}

findD_city(29,40,"Taipei")
#seems worked

#c
findD_city_v2 = function(x,y,city){
  cx = cities %>% filter(name == city ) %>% select(long)
  cx1=as.numeric(cx)
  cy = cities %>% filter(name == city ) %>% select(lat)
  cy1=as.numeric(cy)
  dy = cy1-y
  dx = (cx1-x)*cos((pi/180)*((y+cy1)/2))
  distance = 111.325*(sqrt((dx**2)+(dy**2)))
  
  if (y <-90 & y> 90){
    warning("Latitude should not over 90 or less than -90 for planet Earth, please check 
         if latitude and longitude is swapped")
    print("The Distance between two cities is")
    return(distance)
  } else {
    print("The Distance between two cities is")
    return(distance)
  }}

findD_city_v2(180,90,"Taipei")

#d plot a Map like in Q2 apply function to check if any outcome < 100 to find target
#"Port-au-Prince" closer than 100km
#filter findD_city_v2 < 100km 
findD_city_v2(100,31.0,"Port-au-Prince")
close = hurrs %>% mutate("distance_to_Prince" = findD_city_v2(hurrs$longitude,hurrs$latitude,"Port-au-Prince"))
real_close = close %>% filter(distance_to_Prince<=100)
hurrs_prince = hurrs %>% filter(id == real_close$id )
Port = cities %>% filter(name == "Port-au-Prince" )
#tracl point is found but what to plot in Q2
world = map_data("world")
basemap = ggplot() +
  coord_fixed(xlim=c(-100,-25),ylim=c(0,75)) +
  geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
landed = hurrs %>% filter(isLandfall==TRUE)
basemap +
  geom_path(data=hurrs_prince, aes(x=longitude,y=latitude,color=id)) +
  geom_point(data = Port, aes(x = long, y = lat), size = 3, shape = 23, fill = "darkred")+
  geom_text(data = Port, aes(x = long, y = lat+2, label = name), size = 2)+
  theme(legend.position = "none")

#Q4
#a
number_by_year = hurrs %>% group_by(year) %>% summarise(count = n_distinct(id))
number_of_hurrs_year = hurrs %>% group_by(year) %>% distinct(id)
number_of_hurrs_year
ggplot(number_of_hurrs_year,aes(year)) + geom_histogram(binwidth = 1)

#b
library(maps) 
#find max and min of latitude and longtitue in the dataset of landed hurricane which mean is the area of the map
#rather than using the whole Atlantic area i chose only focus on the landed area to give necessary data
#if needed the whole Atlantic area can been found by replace "landed" with the "hurrs" dataset
atlantic = map_data("world")
landed = hurrs %>% filter(isLandfall==TRUE)
x1=max(landed$longitude)
x2=min(landed$longitude)
y1=max(landed$latitude)
y2=min(landed$latitude)
#plot out the map
#have to add those hurricane which landed
basemap = ggplot() +
coord_fixed(xlim=c(x1,x2),ylim=c(y1,y2)) +
geom_polygon(data=atlantic, aes(x=long,y=lat,group=group), color="gray50", fill="white")
landed = hurrs %>% filter(isLandfall==TRUE)
basemap +
geom_path(data=landed, aes(x=longitude,y=latitude,group=id, color=year)) +
scale_color_distiller(type="div",direction=0,palette="RdYlBu")
#It can be seen that the landing zone of the hurricane has been expanded over years and even
#striking those area that never recorded hurricane landing before. The landing area was more 
#focused to south part of this map before 1950 but after 1950 seems the whole landing zone has shifted and expanded
#they landing points was focusing between latitude 30 to 49 before but now the range is expanded to 10-40
#c
#Is peak wind speed increasing over time?
#try boxplot
boxplot(windspeed~year, data=hurrs)
#it can answer the question but seems a little too much information
#would linechart better?
peak_windspeed = hurrs %>% group_by(year) %>% drop_na()%>%summarise(peak = max(windspeed))
linechart= ggplot(peak_windspeed, aes(x=year, y=peak)) + geom_line()
linechart + coord_fixed()
#a lot better 
#the trend we can see from this line chart allow us to see there is actually a increasing trend for the maximum wind speed of hurricane by year
#but the sample size seems to small to judge.

#d
#hist of storm by month to show season (group by month, count number)
hurrs %>% group_by(month) %>% summarise(count = n_distinct(id))
number_of_hurrs_month = hurrs %>% group_by(month) %>% distinct(id)
number_of_hurrs_month
ggplot(number_of_hurrs_month) + geom_histogram(aes(month),binwidth = 1) + scale_x_continuous(breaks = seq(1, 12, by=1))
#According to the histogram hurricane season seems to be September

#e
#decades add to huurs (using mutate and decade and apply)
decade <- function(year)
{
  stopifnot(is.numeric(year))
  
  10 * (year %/% 10)  
}

hurrs = hurrs %>% mutate("decade" = decade(year))
hurrs
#f
hurrs_year_month = hurrs %>% group_by(year,month) %>% distinct(id)
ggplot(data=hurrs_year_month, aes(x=year, fill = factor(month)))+ geom_histogram(binwidth = 1,alpha=1,show.legend=TRUE) + scale_fill_brewer(type="div",direction=1,palette="PRGn")
#very interesting finding has been shown by this plot, at the beginning of hurricane were only recorded at June-October, and very rarely at May, hurricanes was mainly happens in September and October
#then the hurricanes happen more and more in number, the coverage of the hurricane is started expanding to November and December along with the year movement we can see there is more and more hurricanes
#happened in November and December and even started to happen in January which is never happened before. And we can also see there is more and more hurricanes happening in June and May, also getting more often in April and starting to happen in March which never happen before!
#We can definitely see there is a trend that the hurricanes is starting to happen in the later and earlier month in the year, not only growing in amount of hurricanes, but something never happens is starting to happens.