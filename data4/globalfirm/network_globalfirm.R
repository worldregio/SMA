## ----cache.comments=TRUE,warning=FALSE-----------------------------------
rm(list=ls())
library(reshape2)
library(igraph)
setwd("/Users/claudegrasland1/Documents/cg/cours/ASPS2016/NETWORK/globalfirm")

## ----warning=F,comment=F,message=F---------------------------------------
tab<-read.csv("globalfirm.csv",
                 header=TRUE,
                 sep=";",
                 na.strings="NA",
                 encoding="UTF-8")
head(tab)
dim(tab)
# Accountancy
acc<-as.matrix(tab[,2:6]) 
rownames(acc)<-tab$City
# Advertisment
adv<-as.matrix(tab[,7:17]) 
rownames(adv)<-tab$City
# Banking
bnk<-as.matrix(tab[,18:31]) 
rownames(bnk)<-tab$City
#Lawyer
law<-as.matrix(tab[,32:47]) 
rownames(law)<-tab$City
# Total
tot<-as.matrix(tab[,2:47]) 
rownames(tot)<-tab$City
# Ech
x1<-c(2,0,0,3,3,1,3,1,0,1)
x2<-c(0,3,1,1,0,1,2,0,1,1)
x3<-c(3,0,0,3,3,0,3,0,1,1)
ech<-cbind(x1,x2,x3)
row.names(ech)<-c("Chica","Frank","HongK","Londo","LosAn","Milan","NewYo","Paris","Singa","Tokyo")
is.matrix(ech)

## ------------------------------------------------------------------------
mat_lieu_indiv<-ech
mat_lieu_indiv

## ------------------------------------------------------------------------
totcol<-apply(mat_lieu_indiv,FUN="sum",2)
totcol

## ------------------------------------------------------------------------
totligne<-apply(mat_lieu_indiv,FUN="sum",1)
totligne

## ------------------------------------------------------------------------
# elemental relational matrix
matrel<-(mat_lieu_indiv)%*%t(mat_lieu_indiv)
matrel
#matrel

## ------------------------------------------------------------------------
#proportional relation matrix
maxcol<-apply(mat_lieu_indiv,FUN="max",2)
max<-sum(maxcol*maxcol)
matrelnorm<-round(matrel/max,2)
matrelnorm

## ------------------------------------------------------------------------
#social distance matrix
matsocdist<-1-matrelnorm
dim(matsocdist)[1]
for (i in 1:dim(matsocdist)[1]) { matsocdist[i,i]<-0}
matsocdist

## ------------------------------------------------------------------------
#asymmetrical relational matrix
maxlinkcity<-apply(mat_lieu_indiv*apply(mat_lieu_indiv,FUN="max",2),FUN="sum",1)
maxlinkcity
matrelasym<-round(matrel/maxlinkcity,2)
matrelasym

## ------------------------------------------------------------------------
mat<-matrelnorm
# create boolean matrix
seuil<-0.25
mat[mat<seuil]<-0
mat[mat>=seuil]<-1
# eliminate empty rows or columns
totlig<-apply(mat,FUN="sum",1)
totcol<-apply(mat,FUN="sum",2)
mat<-mat[totlig>0,totcol>0]
#plot graph
net<-graph_from_adjacency_matrix(mat,diag=FALSE,mode="undirected")
size<-sqrt(degree(net,normalized=T))
plot.igraph(net,
            vertex.color="red",
            vertex.label.dist=0.6,
            vertex.label.cex=0.5+0.5*size,
            vertex.size=size*10
           )
# compute centrality indexes
tabres<-as_data_frame(net, what="vertices")
tabres$C_DEG_std<-round(degree(net,normalized=T),2)
tabres$C_BET_std<-round(betweenness(net,normalized=T),2)
tabres$C_CLO_std<-round(closeness(net,normalized=T),2)
tabres[order(tabres$C_DEG_std, decreasing=T),]

## ----cache.comments=TRUE,message=FALSE,warning=FALSE---------------------
# SAUVEGARDER LE PROGRAMME ? 
library(knitr)
purl("network_globalfirm_v2.Rmd")

