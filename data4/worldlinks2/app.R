library(shinydashboard)
#library(ggplot2)
library(dplyr)
#library(xtable)
library(knitr)
#library(plotly)
library(sf)
library(data.table)
#library(geosphere)

# Set directory
#setwd("/data/user/g/cgrasland/zPublish/shiny/worldlinks")

# Load data ------
map<-readRDS("gawc/states.RDS")
#x<-data.frame(Country=map$Country, ISO3=map$ISO3)
cities<-readRDS("gawc/cities.RDS")
#nodes <- merge(nodes,x,by="ISO3",all.x=T,all.y=F)
cities$City<-paste(cities$City," (",cities$Country,")", sep="")
cities<-st_set_crs(cities,4326)
cities<-cities[order(cities$year,-cities$D),]

links2<-readRDS("gawc/cities_links.RDS")
links2$Cit_i<-paste(links2$Cit_i," (",links2$Sta_iso3_i,")", sep="")
links2$Cit_j<-paste(links2$Cit_j," (",links2$Sta_iso3_j,")", sep="")
links2<-st_set_crs(links2,4326)
names <- cities$City[cities$year=="2016"]
vars <- names






#### Test

####

# Load map ----



# -----------

ui <- dashboardPage(
    
    
    
    dashboardHeader(title = "World Cities Network (2000-2016)"),
    
    #------
    dashboardSidebar(
        
  #      selectInput('size', 'Size criteria', vars, "TOT") ,
  #      selectInput('size', 'Select criteria', 
  #                  c("Land area"= "SRF",
  #                    "Agricultural Area" = "ARB",
  #                    "Total Population" = "POP",
  #                    "Urban Population" = "URB",
  #                    "Gross Domestic Product (ppp)" = "GDP",
  #                    "CO2 emissions" = "CO2",
  #                    "SYNTHETIC POWER INDEX" = "TOT"),
  #                  selected = "POP"),
        
       selectInput('time', 'Select year', c("2000","2016"), "2000"),
        selectInput('city', 'Select city', names, "Paris**"),
        
        checkboxInput(inputId = "polar",label = "Polar projection",value = TRUE),
        
       sliderInput('maxlink',"Top links",min = 10,max=1000, value=100),
        
        paste("Auteur : C. Grasland (2023) --- "),
  
        
        paste("Source : GaWC (2000-2020)")
        
        
    ),
    #-----
    dashboardBody(
        
#-----   first line of body --------------   
    fluidRow(

         box(title = "Global Cities network according to GaWC",
            plotOutput('map'),width=8, height=700),
         
         box(title = "Top links ",
             tableOutput('topinter'),
             width = 4,
             height = 700),
   #      valueBoxOutput("nbflights0"),
   #      valueBoxOutput("nbflights1"),
   #      valueBoxOutput("nbflights3"),
   #      valueBoxOutput("nbflights2"),
   #      valueBoxOutput("nbflights4")
         
         
          

        ),

        
# --------------------------       
    )
)

server <- function(input, output) {
 #   set.seed(122)
 #   histdata <- rnorm(500)
 #   
 #   output$plot1 <- renderPlot({
 #       data <- histdata[seq_len(input$slider)]
 #       hist(data)
    
  # Combine the selected variables into a new data frame

#  tab<- reactive({
#    tab<-links2[links2$year==input$time,]
#    tab<-tab[tab$Cit_i==input$city,]
#    tab<-head(tab,input$maxlink)
#    return(tab)
#  })

  tab1<- reactive({
    tab1<-links2[links2$year==input$time,]
    tab1<-tab1[tab1$Cit_i==input$city,]
    tab1<-st_drop_geometry(tab1)
    tab1<-tab1[,c("Cit_j","Lij")]
    tab1$pct<-round(100*tab1$Lij/sum(tab1$Lij),2)
    tab1<-tab1[order(-tab1$Lij),]
    tab1<-head(tab1,20)
    return(tab1)
  })
  

  
    
    
    output$topinter <- ({
        
        renderTable(tab1()[1:20,])
    })
    

    
 
    
    output$map <- renderPlot({ 
      nodes<-cities[cities$year==input$time,]
      if(input$polar == FALSE){ 
        xref<-round(st_coordinates(nodes[nodes$City==input$city,])[1],0)
        yref<-round(st_coordinates(nodes[nodes$City==input$city,])[2],0)
      } else {
        xref=0
        yref=90
      }
      mycrs<-paste("+proj=laea +x_0=0 +y_0=0 +lon_0=",xref," +lat_0=",yref,sep="")
      
      par(mar=c(0,0,0,0),bg="black")
      
      #create basemap
      map<-st_transform(map,crs=mycrs)
      plot(map$geometry, border="white",  lwd=0.5)
      
      # Add connected states
      tab<-links2[links2$year==input$time,]
      tab<-tab[tab$Cit_i==input$city,]
      tab<-head(tab,input$maxlink)
      stalist<-unique(tab$Sta_iso3_j)
     map2<-map[map$ISO3 %in% stalist,]
 #     mycol<- add.alpha("red", alpha=0.4)
      plot(map2$geometry,col="brown4", border=NA,  lwd=0.5,add=T,alpha=0.3)
      
      #Add basemap again
      map<-st_transform(map,crs=mycrs)
      plot(map$geometry, border="gray50",  lwd=0.5, add=T)
      
      # Add host states
      tab<-links2[links2$year==input$time,]
      tab<-tab[tab$Cit_i==input$city,]
      tab<-head(tab,input$maxlink)
      stalist<-unique(tab$Sta_iso3_i)
      map3<-map[map$ISO3 %in% stalist,]
      #     mycol<- add.alpha("red", alpha=0.4)
      plot(map3$geometry,col="blue", border=NA,  lwd=0.5,add=T,alpha=0.3)
      
      #overlay airports
      nodes<-cities[cities$year==input$time,]
      nodes<-st_transform(nodes,crs=mycrs)
      plot(nodes$geometry, pch=3, cex=0.15, col="gray70",add=T)
      #points(nodes$Longitude,nodes$Latitude, pch=3, cex=0.05, col="chocolate1")
      
      #overlay connected airports
      nodes<-cities[cities$year==input$time,]
      nodes<-st_transform(nodes,crs=mycrs)
      tab<-links2[links2$year==input$time,]
      tab<-tab[tab$Cit_i==input$city,]
      tab<-head(tab,input$maxlink)
      citlist<-unique(tab$Cit_j)
      sel<-nodes[nodes$City %in% citlist,]
      sel$size<-sel$D/max(sel$D,na.rm=T)*3
      plot(sel$geometry, pch=20, cex=sel$size, col="orange", add=T)
      #points(nodes$Longitude,nodes$Latitude, pch=3, cex=0.05, col="chocolate1")
      
      # overlay links
      tab<-links2[links2$year==input$time,]
      tab<-tab[tab$Cit_i==input$city,]
      sel<-head(tab,input$maxlink)
      sel<-st_transform(sel,crs=mycrs)
      plot(sel$geometry,col="lightyellow",add=T,lwd=0.7)
      
      
    }, height = 600)
    

    
    output$nbflights1 <- renderValueBox({
      nodes<-cities[cities$year==input$time,]
      x<-round(nodes[nodes$City==input$city,]$VI/2,0)[1]
      nbf<-dim(tab())[1]
      valueBox(
        x,"TRANSNATIONAL (nb. of cities)", icon = icon("plane", lib = "glyphicon"),
        color = "yellow"
      ) 
    })  
    
    output$nbflights2 <- renderValueBox({
      nodes<-cities[cities$year==input$time,]
      x<-round(nodes[nodes$City==input$city,]$VN/2,0)[1]
      nbf<-dim(tab())[1]
      valueBox(
        x,"NATIONAL (nb. of cities)", icon = icon("plane", lib = "glyphicon"),
        color = "blue"
      ) 
    })  
    
    output$nbflights3 <- renderValueBox({
      nodes<-cities[cities$year==input$time,]
      x<-round(nodes[nodes$City==input$city,]$DI/2,0)[1]
      nbf<-dim(tab())[1]
      valueBox(
        x,"TRANSNATIONAL (nb. of km)", icon = icon("globe", lib = "glyphicon"),
        color = "yellow"
      ) 
    })  
    
    output$nbflights4 <- renderValueBox({
      nodes<-cities[cities$year==input$time,]
      x<-round(nodes[nodes$City==input$city,]$DN/2,0)[1]
      nbf<-dim(tab())[1]
      valueBox(
        x,"NATIONAL (nb. of km)", icon = icon("globe", lib = "glyphicon"),
        color = "blue"
      ) 
    })  
  
    output$nbflights0 <- renderValueBox({
      x<-length(unique(tab()$Sta_j))
      valueBox(
        x,"INTERNATIONAL (nb. of states)", icon = icon("globe", lib = "glyphicon"),
        color = "red"
      ) 
    })  
    
    
      
}

shinyApp(ui, server)
