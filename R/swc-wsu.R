x<-8
y <- x/2
ls()
ls() # list objects in environment

#a vector of weights
weights<- c(50,60,65,82)
weights
animals<-c("mouse", "rat", "dog")
animals

#functions are followed by parenthesis. the information in the () is an argument.
#to find out more about the function, enter ?function
length(weights)
length(animals)
class(weights)
class(animals)
str(weights)
str(animals)

#add elements to a vector
weights <- c(weights,90)
weights
weights <- c(30,weights)
weights
#6 data types: numeric, character, logical, integer, complex, raw
#data structures: factor

z<-c(x,y,weights)
mean(z)

getwd()
list.files()
#data<-read.csv("C:\\Users\\dpcollins\\Documents\\SMALLFARMS\\Research\\Stats\\SoftwareCarpentry\\gapminder.csv")
gapminder<-read.csv("gapminder.csv")
#setwd("~/Desktop")  "~" will 
head(gapminder,50)
class(gapminder)
str(gapminder)
dim(gapminder)
weights[1]
weights [1:3]


#firstrow, first column
gapminder[1,1]
#first row, third column
gapminder[1,3]
#500th row,

gapminder$pop
#equivalent to:
gapminder[,5]
gapminder[,"pop"]

gapminder[gapminder$country=="Finland",]
gapminder[gapminder$pop<=100000, c("country", "year")]
gapminder[gapminder$pop<=100000, c(1,3)]

head(gapminder)

gapminder[50,4]
gapminder$lifeExp[50]

#which countries have life expectancies greater than 80
le<-gapminder[gapminder$lifeExp>80,]
le<-droplevels(le)
levels(le$country)

#country with lowest life expectancy
min(gapminder$lifeExp)
lowest<-gapminder[min(gapminder$lifeExp),"country"]
require(dplyr)
library(dplyr)
library(ggplot2)

#What is dplyr? used for common data frame manipulations and subsets.  
#Ability to work with sebsets of data that are stored externally.

#select keeps columns and filter keeps rows
select(gapminder, country,year,pop)
filter(gapminder,country=="Finland")

#PIPE DATA
gapminder_sml <- gapminder %>%
  filter(pop<=100000)%>%
  select (country,year)

Challenge:  Using pipes subset the gapminder data to include rows where gdpPercap was greater than or equal to 35000. 
Retain columns country, year, and gdpPercap

gap_35<-gapminder%>%
        filter(gdpPercap>=35000) %>%
        select(country,year,gdpPercap)

gapminder %>%
    mutate(totalgdp=gdpPercap*pop)%>%
    head

gapminder %>%
    mutate(totalgdp=gdpPercap*pop)%>%
    group_by (continent)%>%
    summarize(meangdp=mean)
    (totalgdp)

gapminder%>%
    mutate(totalgdp=gdpPercap*pop)%>%
    group_by(continent,year)%>%
    summarize(meangdp=mean(totalgdp))

gapminder %>%
    mutate(totalgdp=gdpPercap*pop)%>%
    group_by(continent,year)%>%
    summarize(meangdp=mean(totalgdp),
              mingdp=min(totalgdp))

#max life expectancy for each continent
gapminder %>%
    group_by(continent)%>%
    summarize(max_le=max(lifeExp))

gapminder %>%
    group_by(year)%>%
    summarize(mean_le=mean(lifeExp),
    min_le=min(lifeExp),
    max_le=max(lifeExp))


#Pick a country and find the population for each year in the data prior to 1982. Return a data frame with the
#columns country, year, and pop.
gapminder %>%
    group_by(year)%>%
    filter(country=="Afghanistan")%>%
    filter(year<1982)%>%
    select(country,year,pop)

#tidy data = observation in rows and variables in columns.
    
library(ggplot2)






require(lattice)
xyplot(lifeExp~year|country, data=which(gapminder[gapminder$country=="Afghanistan",]))
xyplot(lifeExp~year|country, data=gapminder[gapminder$country=="Afghanistan"|gapminder$country=="United States",])
xyplot(lifeExp~year, group=country, data=gapminder[gapminder$country=="Afghanistan"|gapminder$country=="United States",])
xyplot(lifeExp~year, group=country, auto.key=T,data=gapminder[gapminder$continent=="Americas",])

dat<-gapminder%>%filter(continent=="Americas")
xyplot(lifeExp~year, group=country, auto.key=T,data=dat, drop.unused.levels=T)

xyplot(lifeExp~year, group=country, auto.key=T,data=(gapminder%>%filter(continent=="Americas")))


gapminder[gapminder$country=="Afghanistan",]


