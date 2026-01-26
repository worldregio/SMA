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

# Add size
don <- don[,c(1,2,12,3:11)]
tot2<-tot[,.(i=i, pct=100*Fit/sum(Fit)),.(t)]
don<-left_join(don,tot2)



# ACP 
library(FactoMineR)
acp <-PCA(don[,4:11],col.w = 13,scale.unit = T)
plot(acp,choix = "var")
don$F1<-acp$ind$coord[,1]
don$F2<-acp$ind$coord[,2]
# CAH
cah<-HCPC(acp,nb.clust = 6)
don$typ<-cah$data.clust$clust
typ<-cah$data.clust
levels(typ$clust)<-c("A.1-Energie","A.2-Agriculture","A.3-Mines","B.1-Textile","B.3-High Tech","B.2-Electronique")
typ$clust<-as.character(as.factor(typ$clust))
par(mfrow=c(3,3))
plot(catdes(typ, num.var = 9,proba = 2),barplot = T,level = 2)

# Add Names
map<-readRDS("data2/World_Chelem.RDS") %>% select(i=code, name) %>% st_drop_geometry()
don<-don %>%left_join(map)
don<-don[,c(1,2,17,13,4:12,14,15,16)]
levels(don$typ)<-c("A.1-Energie","A.2-Agriculture","A.3-Mines","B.1-Textile","B.3-High Tech","B.2-Electronique")
don$typ<-as.character(as.factor(don$typ))

saveRDS(don,"data2/CHELEM_typo.RDS")
write.table(don,"data2/CHELEM_typo.csv",sep=";",dec=",",row.names = F)

typo<- don %>% select(5:13,14,16) %>% group_by(typ) %>% 
           summarise_all(mean)
kable(typo, digits=1)


library(ggplot2)
library(ggrepel)

sel <- don %>% filter(t=="2016-20") %>% filter(pct>0.1)
sel$size <- sqrt(sel$pct)

g<- ggplot(sel) + aes(x=F1,y=F2) + 
  geom_point(aes(col=typ,size=size)) + 
  geom_label_repel(aes(label=i, size=size)) + 
  theme_light()
g

