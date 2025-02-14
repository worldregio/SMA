library(dplyr)
library(data.table)
library(mapsf)
library(sf)

x1<-readRDS("gawc_2000/cities.RDS")  %>% mutate(year="2000") 
x2<-readRDS("gawc_2016/cities.RDS") %>% mutate(year="2016")
x<-rbind(x1,x2)
saveRDS(x,"gawc/cities.RDS")

x1<-readRDS("gawc_2000/cities_links.RDS")  %>% mutate(year="2000") 
x2<-readRDS("gawc_2016/cities_links.RDS")   %>% mutate(year="2016") 
x<-rbind(x1,x2)
saveRDS(x,"gawc/cities_links.RDS")
