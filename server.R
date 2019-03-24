library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
  set.seed(100)
#  alldata <- final_new[sample.int(nrow(final_new), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]
  earth.dist <- function (long1, lat1, long2, lat2)
  {
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- round(R * c,digits = 1)
    return(d)
  }
 
function(input, output, session) {

  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng =10, lat = 51, zoom = 6.2)
  })


  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({

    final_f<-filter(final,Km_Hersteller==input$Km_Hersteller)
     # output$secondSelection <- renderUI({
     #   selectInput("Km_Werk", "Km_Werk", levels(factor(final_f$Km_Werk)))
     # })
     # final_f<-filter(final_f,Km_Werk==as.character(input$Km_Hersteller))
    # 

     All_Info<-final_f%>% 
       unite(INFO, Km_Hersteller,Km_Werk,ET_Hersteller,ET_Werk, sep=",")
     All_Info<- as.data.frame(table(All_Info$INFO))%>%
       separate(Var1, c("Km_Hersteller","Km_Werk","ET_Hersteller","ET_Werk"))%>%
      left_join(Tier1_geo,by=c("Km_Werk"))%>%
      left_join(Tier2_geo,by=c("ET_Werk"))
     All_Info$Entfern <- earth.dist(All_Info$ET_L.ngengrad,All_Info$ET_Breitengrad,All_Info$Km_L.ngengrad,All_Info$Km_Breitengrad)
     All_Info2<-All_Info %>% select(ET_L.ngengrad,ET_Breitengrad,ET_Werk,ET_Hersteller,Km_Werk,Entfern)%>%
       spread(Km_Werk,Entfern)
    All_Info2 <- unite(All_Info2,Entfern,5:(4+as.numeric(length(levels(factor(ET_Werk_Info2$Km_Werk))))),sep = " km ,")
    
    ET_Werk_Info<-final_f%>%
      unite(INFO, ET_Hersteller,ET_Werk, sep=",")
    ET_Werk_Info<- as.data.frame(table(ET_Werk_Info$INFO))%>%
      separate(Var1, c("ET_Hersteller","ET_Werk"))%>%
      left_join(Tier2_geo,by=c("ET_Werk"))%>%merge(All_Info2)
    
    
    ET_Werk_Info$ET_Breitengrad <- jitter(ET_Werk_Info$ET_Breitengrad , factor = 4.0001)
    ET_Werk_Info$ET_L.ngengrad <- jitter(ET_Werk_Info$ET_L.ngengrad, factor = 4.0001)

    Km_Werk_Info<-left_join(as.data.frame(table(final_f$Km_Werk)),Tier1_geo,by=c("Var1"="Km_Werk"))

    
    colorData <- min(ET_Werk_Info$Freq):max(ET_Werk_Info$Freq)
    pal <- colorNumeric("viridis", colorData)
    
    output$histCentile <- renderPlot({
      ggplot(All_Info, aes(x=ET_Werk, y=Freq,fill = Km_Werk)) +
        geom_bar(stat="identity")+
        coord_flip()+ylab("Volumenstroeme")+theme(legend.position="top")
      })

    

    leafletProxy("map") %>%
      clearShapes() %>%
      addCircles(lat = Km_Werk_Info$Km_Breitengrad, 
                 lng=Km_Werk_Info$Km_L.ngengrad,
                 radius=15000,stroke=TRUE, 
                 color = "red", 
                 weight = 5,
                 opacity = 0.2, 
                 fillOpacity= 0.5, 
                 fillColor=pal(Km_Werk_Info$Freq),   
                 popup = paste("TIER1", "<br>",
                               "Hersteller",input$Km_Hersteller, "<br>",
                               "Werk", Km_Werk_Info$Var1, "<br>",
                               "Gesamtzahl des Eingangs", Km_Werk_Info$Freq, "<br>" ))%>%
      addCircles(lat = ET_Werk_Info$ET_Breitengrad, 
                 lng=ET_Werk_Info$ET_L.ngengrad,
                 radius=10000,
                 stroke=FALSE, 
                 fillOpacity= 0.5, 
                 fillColor=pal(ET_Werk_Info$Freq),
                 popup = paste("TIER2", "<br>",
                               "Hersteller",ET_Werk_Info$ET_Hersteller, "<br>",
                               "Werk", ET_Werk_Info$ET_Werk, "<br>",
                               "Gesamtzahl gelieferte Einzelteile", ET_Werk_Info$Freq, "<br>",
                               "Zulieferwerk-Entfernung zur Km_Werke", list(levels(factor(All_Info$Km_Werk))),
                               ET_Werk_Info$Entfern,"km"))%>%
      addLegend("bottomleft", pal=pal, values=colorData, title="Volumenstroeme",layerId="colorLegend")
      # addPolylines(lat =unite(All_Info,N_B,Km_Breitengrad, ET_Breitengrad,sep=" ")$n_B, lng=unite(All_Info,N_L, Km_L.ngengrad,ET_L.ngengrad,sep=" ")$n_L)



    
     # draw_line(alldata_f)
      # addPolylines(lat =c(alldata_f$Km_Breitengrad[],alldata_f$ET_Breitengrad[]), lng=c(alldata_f$Km_L.ngengrad[],alldata_f$ET_L.ngengrad[]))
      # addCircles(lat = ~ET_Breitengrad, lng=~ET_L.ngengrad,radius=10000,stroke=FALSE, fillOpacity=0.4, fillColor="green")
    #   # addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
    #   #   stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    #   # addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
    #   #   layerId="colorLegend")

   
  })

  
  
  

  ## Data Explorer ###########################################

  output$ziptable <- DT::renderDataTable({filter(final, Km_Hersteller==input$Km_Hersteller)
   
  })
}
