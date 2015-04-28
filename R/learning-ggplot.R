ls()
rm(list=ls())
library("ggplot2")
library(dplyr)
getwd()
gapminder<-read.csv("gapminder.csv")

#scatterplot of lifeExp vs gdpPercap
ggplot(gapminder,
       aes(x=gdpPercap,y=lifeExp))+
        geom_point()

p<-ggplot(gapminder,
          aes(x=gdpPercap,y=lifeExp))

p+geom_point()

p3<-p+geom_point()+scale_x_log10()
p3

#scatterplot of lifeExp vs gdpPercap with only data for China
China<-gapminder %>%
  filter(country=="China")
p<-ggplot(China,
          aes(x=gdpPercap,y=lifeExp))
p+geom_point()


#New R trick: put parenthesis around the entire call and you both 
#create the object and run the plot.
(p<-ggplot(gapminder%>%
          filter(country=="China"),
          aes(x=gdpPercap,y=lifeExp))+
          geom_point())

p+geom_point(size=4)+scale_x_log10()

p<-ggplot(gapminder%>%
            filter(country=="China"),
          aes(x=year,y=lifeExp))
p+geom_point()

p<-ggplot(gapminder,
          aes(x=gdpPercap,y=lifeExp, color=continent))
p+geom_point()+scale_x_log10()


p<-ggplot(gapminder,
          aes(x=gdpPercap,y=lifeExp,size=pop))
p+geom_point()+scale_x_log10()

p<-ggplot(gapminder,
          aes(x=gdpPercap,y=lifeExp,color=as.factor(year)))
p+geom_point()+scale_x_log10()

#Plot of china but with lines
gm_china<-gapminder %>%
    filter(country=="China")
    ggplot(gm_china, aes(x=gdpPercap,y=lifeExp))+
    geom_line(color="violetred")+
    geom_point(color="lightblue", size=4)
##Layering:  If the color is put in geom, it will affect all of the points
#If color is put in the aesthetic, it will vary.

ggplot(gm_china, aes(x=gdpPercap,y=lifeExp))+
  geom_line(color="violetred")+
  geom_point(aes(color=year), size=4)

#make a plot of life Exp vs gdpPercap for china and india
#with lines in black but points colored by country
dat<-gapminder%>%
  filter(country=="China"|country=="India")
dat<-gapminder%>%
  filter(country %in% c("China","India"))
ggplot(dat,aes(x=gdpPercap,y=lifeExp,group=country))+
  geom_line(color="black")+
  geom_point(aes(color=country),size=5)


gapminder %>%
  filter(year==2007) %>% 
  ggplot(aes(y=lifeExp,x=continent)) +
  geom_boxplot()

gapminder %>%
  filter(year==2007) %>% 
  ggplot(aes(y=lifeExp,x=continent)) +
  geom_boxplot()+coord_flip()

gapminder %>%
  filter(year==2007) %>% 
  ggplot(aes(y=lifeExp,x=continent)) +
  geom_boxplot()+
  geom_point(position=position_jitter(width=0.1,height=0))


#FACETing
ggplot(gapminder,
       aes(x=gdpPercap,y=lifeExp))+
  geom_point()+scale_x_log10()+
  facet_grid(~continent)

ggplot(gapminder,
       aes(x=gdpPercap,y=lifeExp))+
  geom_point()+scale_x_log10()+
  facet_grid(continent~.)

ggplot(gapminder,
       aes(x=gdpPercap,y=lifeExp))+
  geom_point()+scale_x_log10()+
  facet_grid(continent~year)

ggplot(gapminder,
       aes(x=gdpPercap,y=lifeExp))+
  geom_point()+scale_x_log10()+
  facet_wrap(~continent)

ggplot(gapminder,
       aes(x=gdpPercap,y=lifeExp,color=continent))+
  geom_point()+scale_x_log10()+
  facet_wrap(~year)


dat<-gapminder%>%
  filter(country %in% c("China","India","United States","France","Nigeria"))
ggplot(dat,(aes(x=gdpPercap,y=lifeExp)))+
  geom_line()+
  facet_wrap(~country)

#Another way to pipe the data
 p<- gapminder%>%
  filter(country %in% c("China","India","United States","France","Nigeria")) %>%
  ggplot((aes(x=gdpPercap,y=lifeExp)))+
  geom_line()+
  facet_wrap(~country)

p+theme_bw()
ggsave("five_countries.png",dpi=300)


require(lattice)
xyplot(lifeExp~year|country, data=gapminder[gapminder$country=="Afghanistan"|gapminder$country=="United States",])
xyplot(lifeExp~year, group=country, data=gapminder[gapminder$country=="Afghanistan"|gapminder$country=="United States",])
xyplot(lifeExp~year, group=country, auto.key=T,data=droplevels(gapminder[gapminder$continent=="Americas",])

dat<-gapminder%>%filter(continent=="Americas")
xyplot(lifeExp~year, group=country, auto.key=T,data=dat, drop.unused.levels=T)

xyplot(lifeExp~year, group=country, auto.key=T,data=(gapminder%>%filter(continent=="Americas")))

