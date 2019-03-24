library(dplyr)
library(tidyr)

#TODO: set workspace to shiny app
setwd("D:/MA/TU Berlin/R-Projekt/Gruppe03/Shiny_Case_study")

#TODO: read Final workspace
load("data/Final.RData")

# change data form for filter()
final$Km_Hersteller <- factor(final$Km_Hersteller)
final$ET_Hersteller <- factor(final$ET_Hersteller)

# read GEO data and cleaning
Tier1_geo<-read.csv("Data/Tier1_Werke_2017-07-11_v1.2_TrR.csv",sep = ";")%>%
  na.omit()%>%
  select(Werk,Breitengrad,L.ngengrad)
names(Tier1_geo) <- c("Km_Werk","Km_Breitengrad","Km_L.ngengrad")
Tier1_geo$Km_Werk <- as.character(Tier1_geo$Km_Werk)

Tier2_geo<-read.csv("Data/Tier2_Werke_2017-07-11_v1.2_TrR.csv",sep = ";")
Tier2_geo<- Tier2_geo[,c("Werk","Breitengrad","L.ngengrad")]%>% 
  na.omit()
names(Tier2_geo) <- c("ET_Werk","ET_Breitengrad","ET_L.ngengrad")
Tier2_geo$ET_Werk <- as.character(Tier2_geo$ET_Werk)

