library(data.table)
library(dplyr)
library(reshape2)
x <- readRDS("data2/MYCHELEM.RDS")
class(x)
x$t<-as.numeric(x$t)
x$per<-cut(x$t,breaks=c(1965, 1970, 1975, 1980, 1985, 1990, 1995,2000, 2005, 2010, 2015, 2020))
levels(x$per)<- c( "1966-70", "1971-75", "1976-80", "1981-85",
"1986-90", "1991-95", "1996-00", "2001-05", "2006-10",
"2011-15", "2016-20")
x<-data.table(x)

tab<-x[,.(Fikt=sum(Fijkt, na.rm=T)),.(i,k, t=per)] 
tot <-tab %>% filter(k=="TOT") %>% select(i,t, Fit=Fikt)
tab <- tab %>% filter(k !="TOT") %>% left_join(tot) %>% mutate(Xikt = round(100*Fikt/Fit,0))


# Pivot
don<- dcast(tab,formula = i+t~k)
don$it<-paste(don$i,don$t, sep="_")
don <- don[,c(1,2,12,3:11)]
# ACP 
library(FactoMineR)
acp <-PCA(don[,4:11])
don$F1<-acp$ind$coord[,1]
