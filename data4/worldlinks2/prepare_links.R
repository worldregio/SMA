library(data.table)
library(sf)
library(dplyr)


#model<-readRDS("openflight/cities_links.RDS")


x<-readRDS("gawc_2000/map_links_gawc_2000.RDS") %>% select(-Lij)

l<-readRDS("gawc_2000/links_gawc_2000.RDS")

ll<-l %>% group_by(i,j,CITi, CITj, STAi, STAj) %>%
  summarise(Lij= sum(Lijk)) %>%
  filter(Lij>49) %>% 
  mutate(INT=as.numeric(STAi!=STAj)) %>%
  filter(CITi !=CITj) %>%
  select(i,j, Cit_i=CITi, Cit_j=CITj, Sta_iso3_i=STAi, Sta_iso3_j=STAj,Lij) 

y<-left_join(x,ll) %>% filter(is.na(Lij)==F)
saveRDS(y, "gawc_2000/cities_links.RDS")
