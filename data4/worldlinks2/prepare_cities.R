library(dplyr)
library(data.table)
library(mapsf)
library(sf)

x<-readRDS(file = "gawc_2000/links_gawc_2000.RDS")
cit1<-x %>% group_by(CITi, CITj, STAi, STAj) %>%
            summarise(Lij= sum(Lijk)) %>%
            filter(Lij>0) %>% 
            mutate(INT=as.numeric(STAi!=STAj)) %>%
            filter(CITi !=CITj) %>%
            group_by(CITi) %>%
            summarise(V=n(),
                      VI=sum(INT),
                      VN=sum(1-INT)) %>%
            select(City=CITi, V, VI,VN)
             
                      
 

cit2<-x %>% filter(i!=j)%>%
  mutate(Country=STAi,
         City=CITi, 
         ID_ref=i,
         INT=as.numeric(STAi!=STAj)) %>%
  mutate( Lijk_INT=Lijk*INT,
          Lijk_NAT=Lijk*(1-INT))%>%
  group_by(Country,City,ID_ref) %>%
  summarise(  D=sum(Lijk),
             DI=sum(Lijk_INT),
             DN=sum(Lijk_NAT))

cit <-left_join(cit2,cit1)
cit$V[is.na(cit$V)]<-0
cit$VI[is.na(cit$VI)]<-0
cit$VN[is.na(cit$VN)]<-0
map<-readRDS("gawc_2000/map_cities_gawc_2000.RDS")
map<-map %>% select(ID_ref=city_code, Lon, Lat, geometry)
res<-inner_join(cit,map)
res<-as.data.frame(res)
res<-st_as_sf(res)
class(res)
saveRDS(res,"gawc_2000/cities.RDS")
