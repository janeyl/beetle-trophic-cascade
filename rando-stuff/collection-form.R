##invetebrate counts for collection form
library(dplyr)

invets <- read.csv("/Users/JaneyLienau/Desktop/Inverts.csv")
head(invets)
str(invets)

invets$Individuals <- as.numeric(invets$Individuals)
invets <- filter(invets, !Individuals == "NA")


test <- invets%>%
  group_by(X)%>%
  summarise(Totals = sum(Individuals))


dates <- invets%>%
  distinct(Date)

#GB


GB <- read.csv("/Users/JaneyLienau/Desktop/GB.csv")
head(GB)
str(GB)

GB$ScientificName <- paste(GB$Genus, GB$Species, sep=" ")


testGB <- GB%>%
  group_by(ScientificName)%>%
  summarise(Totals = sum(SpeciesCount))



