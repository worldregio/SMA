library(data.table)
library(dplyr)
library(reshape2)
library(visNetwork)




Col_Mat<-function(col=col){
  
  names(col)<-c("i","j","Fij")
  don2<-reshape2::dcast(col, formula = i~j, 
                        value.var="Fij",fill=0)
  mat<-as.matrix(don2[,-1])
  row.names(mat)<-don2$i
  return(mat)
}



Mat_Col<-function(mat=mat){
  tab<-reshape2::melt(mat)
  names(tab)<-c("i","j","Fij")
  tab$i<-as.character(tab$i)
  tab$j<-as.character(tab$j)
  tab<-as.data.frame(tab)
  return(tab)
}


Mat_Norm<-function(mat=mat,
                   norm=1000000,
                   dig=0){
  tot<-sum(mat,na.rm=T)
  mat<-round(mat*norm/tot,dig)
}


Col_Norm<-function(col=col,
                   norm=1000000,
                   dig=0){
  mat<-Col_Mat(col)
  mat<-Mat_Norm(mat,norm,dig)
  col<-Mat_Col(mat)
}



Mat_Sel<-function(mat=mat,
                  pcti = 1,
                  pctj = 1,
                  rest = "_rest_"){
  
  Oi<-100*apply(mat,1,sum)/sum(mat)
  Dj<-100*apply(mat,2,sum)/sum(mat)
  seli<-row.names(mat)[Oi>pcti]
  selj<-colnames(mat)[Dj>pctj]
  sel<-unique(c(seli,selj))
  col<-Mat_Col(mat)
  col$i[!col$i %in% sel]<-rest
  col$j[!col$j %in% sel]<-rest
  col2<-col %>% group_by(i,j) %>% 
    summarise(Fij=sum(Fij,na.rm=T))
  mat2<-Col_Mat(col2)
  diag(mat2)<-0
  return(mat2)
}




Col_Sel<-function(mat=mat,
                  pcti = 1,
                  pctj = 1,
                  rest = "_rest_"){
  mat2<-Col_Mat(col)
  mat2<-Mat_Sel(mat2,pcti,pctj,rest)
  col<-Mat_Col(mat2)
}


Col_Mar<-function(col=col){
  Fij<-col
  names(Fij)<-c("i","j","Fij")
  Fji<-Fij
  names(Fji)<-c("j","i","Fji")
  res<-left_join(Fij,Fji) %>% mutate(Vij=Fij+Fji, Sij=Fji-Fij)
  
  mar_i<-res %>% group_by(i) %>% summarise(Oi=sum(Fij, na.rm=T),
                                           Di = sum(Fji, na.rm=T)) %>%
    mutate(Vi=Oi+Di,
           Si=Di-Oi)
  
  res<-left_join(res,mar_i)                      
  mar_j<-res %>% group_by(j) %>% summarise(Oj=sum(Fji, na.rm=T),
                                           Dj =sum(Fij, na.rm=T))%>%
    mutate(Vj=Oj+Dj,
           Sj=Dj-Oj)
  res<-left_join(res,mar_j) 
  res<-as.data.frame(res)
  return(res)
}



Col_Dom<-function(col=col,
                  minPCT=0)
  
{
  
  # Compute parameters
  Oi<-col %>% group_by(i) %>% summarise(Oi=sum(Fij, na.rm=T))
  Wi<-col %>% group_by(j) %>% summarize(size=sum(Fij, na.rm=T)) %>% select(i=j, Wi=size)
  Wj<-col %>% group_by(j) %>% summarize(size=sum(Fij, na.rm=T)) %>% select(j=j, Wj=size)
  col<- col %>% left_join(Oi) %>% left_join(Wi) %>% left_join(Wj) %>% mutate(PCTij=round(100*Fij/Oi,2))
  # Select first flows
  dom <- col %>% group_by(i) %>%
    arrange(-Fij) %>% 
    slice(1) %>% ungroup()  %>%
    mutate(dominated = as.numeric(Wi<Wj & PCTij>minPCT),
           dominant = as.numeric(i %in% unique(j)),
           isolated = as.numeric((dominated+dominant)==0),
           type=(dominant-dominated+2)*(1-isolated))
  return(dom)
  
}







Viz_Dom<-function(tabdom=tabdom,
                  mytitle = "Titre",
                  mysub =" (c) Grasland, C."
) {
  
  
  res<-tabdom
  Ftot<-sum(res$Fij)
  
  # A. CREATE NODES
  nodes<-res %>% filter(type !=0) %>% 
    select( label=i,
            type=type,
            Wi = Wi)
  # A.1 Cration of Id
  nodes$id<-1:length(nodes$label)
  # A.1 Add color for types
  nodes$color<-as.factor(nodes$type)
  levels(nodes$color)<-c("yellow","orange","red")
  nodes$color<-as.character(nodes$color)
  
  # A.1 Add shape for types
  nodes$shape<-as.factor(nodes$type)
  #levels(nodes$shape)<-c("triangle","square","circle")
  #nodes$shape<-as.character(nodes$shape)
  
  
  # A.2 Add Size based on % of Wi
  nodes$pct <-100*nodes$Wi/sum(nodes$Wi)
  nodes$value<-10*sqrt(nodes$pct)
  
  
  # B. CREATE EDGES
  edges<-tabdom %>% filter(dominated!=0)%>%
    select(i,j,Fij, PCTij) %>%
    left_join(nodes %>% select(i=label, from = id)) %>%  
    left_join(nodes %>% select(j=label, to = id )) 
  edges$color<-"gray40"
  #edges$color.border<-"gray40"
  # A.2 Add Size based on % of Fij
  edges$pct <-100*edges$Fij/Ftot
  edges$width<-2*edges$pct
  #edges$arrows<-"middle"
  
  
  
  net<- visNetwork(nodes, 
                   edges, 
                   main = mytitle,
                   submain = mysub,
                   #  main = title,
                   height = "700px", 
                   width = "100%")   %>%   
    visNodes(scaling =list(border="black",
                           min =10, max=80, 
                           label=list(min=10,max=80)))%>%
    visEdges(scaling = list(min=20,max=90))%>%
    visOptions(highlightNearest = TRUE,
               #               selectedBy = "group", 
               #               manipulation = TRUE,
               nodesIdSelection = TRUE) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visLegend() %>%
    visIgraphLayout(layout ="layout.fruchterman.reingold",smooth = TRUE)
  
  return(net)
  
}