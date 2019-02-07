#Packages Used
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(devtools)
library(stringr)
library(ggmap)
library(maps)
library(mapdata)

#Problem One, Exploring the Dataset 
apartments <- readRDS("cl_apartments.RDS")
nrow(apartments)
ncol(apartments)
typeof(apartments$text)
typeof(apartments$title)

dates <- order(apartments$date_posted)
dates.chrono <- apartments[dates,]
head(dates.chrono)

#Density Plot Overview
ggplot(apartments, aes(x = price, y = craigslist)) +
  xlim(0, 8000) +
  geom_density_ridges() + 
  labs(title = "Prices of Homes in California Areas") +
  xlab("Rent per Month") +
  ylab ("Area")

#Creating a unique dataframe

unique.apts <- unique(apartments[])
unique.apts <- subset(unique.apts, price < 8000)



#Problem 2

#Creating list for listings considered "in major cities" and "suburbs"
apt.cities <- subset(unique.apts, city == "Los Angeles" | city == "San Diego" | city == "San Francisco" | city == "San Jose" | city == "Sacramento" | city == "Fresno" | city == "Long Beach" | city == "Oakland" | city == "Anaheim" | city == "Bakersfield" )
apt.suburbs <- subset(unique.apts, city != "Los Angeles" & city != "San Diego" & city != "San Francisco" & city != "San Jose" & city != "Sacramento" & city != "Fresno" & city != "Long Beach" & city != "Oakland" & city != "Anaheim" & city != "Bakersfield" )

In.City <- unique.apts$city == "Los Angeles" | unique.apts$city ==  "San Diego" | unique.apts$city ==  "San Francisco" | unique.apts$city == "San Jose" | unique.apts$city ==  "Sacramento" | unique.apts$city ==  "Fresno" | unique.apts$city ==  "Long Beach" | unique.apts$city ==  "Oakland" | unique.apts$city ==  "Anaheim" | unique.apts$city ==  "Bakersfield" 
unique.apts$In.City <- In.City

#Plotting Bedrooms by Cities
ggplot(data = unique.apts, aes(x = bedrooms, fill = In.City)) +
  geom_bar(position = "fill") +
  xlim(0,5) +
  labs(title = "Bedrooms in Suburbs vs. Cities", x = "Number of Bedrooms", y = "Number of Listings") +
  guides(fill=guide_legend(title="In City?"))


#Computing Means
mean.bedrooms.cities <- mean(apt.cities$bedrooms, na.rm = TRUE)
mean.bedrooms.suburbs <- mean(apt.suburbs$bedrooms, na.rm = TRUE)
number.bedrooms.comp <- c("bedrooms.cities", "bedrooms.suburbs")
mean.bedrooms.two <- c(mean.bedrooms.cities, mean.bedrooms.suburbs)
bedrooms.comparison <- data.frame(number.bedrooms.comp, mean.bedrooms.two)


#Allow Pets?
ggplot(data = unique.apts, aes(x = pets, fill = In.City)) +
  geom_bar(position = "fill") +
  labs(title = "Apartments Allow Pets?", x = "Dog or Cat", y = "Count") +
  guides(fill=guide_legend(title="Post Updated?"))


# Average number of Bedrooms in Cities vs Suburbs
ggplot(bedrooms.comparison) +
  geom_bar(aes(x = number.bedrooms.comp, y = mean.bedrooms.two), stat = "identity") +
  ylim(0,1.8) + 
  labs(title = "Average Number of Bedrooms", x = "Location", y = "Number of Bedrooms")


#Subsetting 2 bedrooms 1 bathrooms, 2x2, 3x1
two.bed_one.bath <- subset(unique.apts, bedrooms == 2 & bathrooms == 1.0 & price < 8000)
two.bed_two.bath <- subset(unique.apts, bedrooms == 2 & bathrooms == 2.0 & price < 8000)
three.bed_one.bath <- subset(unique.apts, bedrooms == 3 & bathrooms == 1.0 & price <8000)

price.two.by.one <- mean(two.bed_one.bath$price, na.rm = TRUE)
price.two.by.two <- mean(two.bed_two.bath$price, na.rm = TRUE)
price.three.by.one <- mean(three.bed_one.bath$price, na.rm = TRUE)



prices <- c(price.two.by.one, price.two.by.two, price.three.by.one) 
bed.bath <- c("2x1", "2x2", "3x1")
prices.bed.bath <- data.frame(bed.bath, prices)

#Average Prices for Different Apartments by Dimensions
ggplot(prices.bed.bath) + 
  geom_bar(aes(x = bed.bath, y = prices), stat = "identity") +
  labs(title = "Average Prices for Different Apartments (Bathrooms vs. Bedrooms)", x = "Bedroom by Bathroom", y = "Price")

#Correlation
cor(unique.apts$price, unique.apts$bedrooms, use = "complete.obs")
cor(unique.apts$price, unique.apts$bathrooms, use = "complete.obs")

benchmark.cor <- subset(unique.apts, bedrooms >= 4 & bathrooms >= 2)
cor(benchmark.cor$bedrooms, benchmark.cor$price)
cor(benchmark.cor$price, benchmark.cor$bathrooms)

#Apartments in areas similar? 

#Subsetting San Francisco
apts.sanfrancisco <- subset(unique.apts, city == "San Francisco")
apts.sanfrancisco <- subset(apts.sanfrancisco, bedrooms >= 1)
apts.sanfrancisco$bathrooms <- as.factor(apts.sanfrancisco$bathrooms)


unique.apts$bathrooms <- as.integer(unique.apts$bathrooms)

#Plotting San Francisco Bedrooms by Bathrooms
ggplot(apts.sanfrancisco) +
  geom_bar (aes(x = bedrooms, fill = bathrooms), position = "fill") +
  labs(title = "San Francisco Apartments", x = "Bedroom", y = "Count") +
  scale_fill_discrete(name = "Number of Bathrooms")

ggplot(data = unique.apts, aes(x = bedrooms, fill = In.City)) +
  geom_bar(position = "fill") +
  xlim(0,5) +
  labs(title = "Bedrooms in Suburbs vs. Cities", x = "Number of Bedrooms", y = "Number of Listings") +
  guides(fill=guide_legend(title="In City?"))


apts.sanfrancisco <- subset(unique.apts, city == "San Francisco")

ggplot((apts.sanfrancisco)) +
  geom_point(aes(x = sqft, y = price)) + 
  xlim(0,2500) + 
  ylim(1000, 8500) +
  labs(title = "San Francisco Apartments", x = "Square Ft", y = "Rent per Month") 

#Looking at Palo Alto
palo.alto <- subset(unique.apts, city == "Palo Alto")

names(palo.alto)[names(palo.alto) == 'pets'] <- 'Dog_or_Cat'

#Scatterplot Palo Alto
ggplot(palo.alto, aes(x = sqft, y = price)) +
  geom_point(aes(color = Dog_or_Cat)) + 
  labs(title = "Palo Alto Apartments", x = "Square Ft", y = "Rent per Month") +
  scale_fill_manual("Allow Dog or Cat?")

#Looking at San Diego
san.diego <- subset(unique.apts, city == "San Diego")
names(san.diego)[names(san.diego) == 'laundry'] <- 'Laundry'


ggplot(san.diego) +
  geom_bar(aes(x = parking, fill = Laundry), position = "fill") +
  labs(title = "San Diego Parking and Laundry", x = "Parking", y = "Laundry") 

ggplot(san.diego) +
  geom_bar(aes(x = Laundry, fill = parking), position = "fill") +
  labs(title = "San Diego Parking and Laundry", x = "Laundry", y = "Parking") +
  guides(fill=guide_legend(title="Type of Parking"))



#Question 4
#Surburb parking vs. City Parking

no.na.apts <- na.omit(unique.apts)
ggplot(data = no.na.apts) +
  geom_bar(aes(x = parking, fill = In.City), position = "fill") +
  labs(title = "Parking in Suburbs vs. Cities", x = "Avaliable Parking", y = "Suburb vs. City Proportion?")

table(unique.apts$In.City, unique.apts$parking)

num.apts <- sum(unique.apts$In.City, na.rm)
parking.total <- aggregate(In.City ~ parking, unique.apts, sum)
apts.incity <- subset(unique.apts, In.City == TRUE)
num.city.parking <- aggregate(In.City ~ parking, apts.incity, sum)

                              

# Desirable Locations
usa <- map_data("usa") 
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
states <- map_data("state")
ca_df <- subset(states, region == "california")
cat.price <- cut(unique.apts$price, breaks = c(100, 1000, 1500, 2300, 3000, 9000), label = c('100-1000', '1000-1500', '1500-2300', '2000-3000', '3000+'))
unique.apts$Price_Category <- cat.price


ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "white") +
  geom_point(data = unique.apts, aes(x=longitude, y=latitude, color = Price_Category), size=0.3, alpha = 0.3, inherit.aes = FALSE) +
  xlim(-125, -114) + ylim(32, 42.5) +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  labs(title = "Price of Home Listings in California", x = "Longitude", y = "Latitude")



#Large apartments and pets

cat.size <- cut(no.na.apts$price, breaks = c(3, 800, 2000, 9000), label = c('Small', 'Medium', 'Large'), na.rm = TRUE)
no.na.apts$Size_Category <- cat.size

no.na.apts <- na.omit(no.na.apts)
ggplot(data = no.na.apts, aes(x = Size_Category, y = stat(prop), fill = pets, group = pets)) +
  geom_bar(postion = "dodge") +
  labs(title = "Parking in Suburbs vs. Cities", x = "Avaliable Parking", y = "Suburb vs. City Proportion?")

ggplot(data = no.na.apts, aes(x = Size_Category, fill = pets)) +
  geom_bar(position = "fill") +
  labs(title = "Apartment Sizes and Pet Options", x = "Apartment Size", y = "Proportion of Pets Avaliabe") +
  guides(fill=guide_legend(title="Dog or Cat"))

ggplot(data = no.na.apts, aes(x = sqft, y = price, color = pets)) +
  geom_point(size=2, alpha = 0.1) + 
  xlim(0, 3000) +
  labs(title = "Price and Size of Apartments and Pet Options", x = "Apartment Size", y = "Price per Month") +
  guides(fill=guide_legend(title="Dogs or Cats"))

#Luxury Living
luxury.living <- subset(unique.apts, (bedrooms <= bathrooms & laundry == "in-unit" & sqft > 1300) & (parking == "covered" | parking =="garage"))

ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "white") +
  geom_point(data = luxury.living, aes(x=longitude, y=latitude, color = pets), size=1, alpha=0.3, inherit.aes = FALSE) +
  xlim(-125, -114) + ylim(32, 42.5) +
  guides(colour = guide_legend(title = "Dogs or Cats", override.aes = list(size=4))) +
  labs(title = "Locations of Luxurious Apartments", x = "Longitude", y = "Latitude")


#Active Craigslist Pages:
table(unique.apts$date_updated, unique.apts$craigslist)
table(unique.apts$date_updated)
  
unique.apts$updated[is.na(unique.apts$date_updated)] <- "NO"
unique.apts$updated[!(is.na(unique.apts$date_updated))] <- "YES"



ggplot(data = unique.apts, aes(x = craigslist, fill = updated)) +
  geom_bar(position = "identity") +
  labs(title = "Activity on Different Craigslist Sites", x = "Craiglist Website", y = "Total Number of Posts") +
  guides(fill=guide_legend(title="Post Updated?"))


# Limitations to Dataset
table(is.na(unique.apts))
summary(unique.apts)
ncol(unique.apts)
