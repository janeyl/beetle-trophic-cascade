#Termite paper

library(dplyr)
library(tidyr)

termites <- read.csv("/Users/JaneyLienau/Desktop/termite_methane.csv")

test <- termites%>%
  filter(Family == "Rhinotermitidae")


test2 <- termites%>%
  filter(Family == "Archotermopsidae")

test3 <- termites%>%
  filter(Family == "Kalotermitidae")


test4 <- bind_rows(test, test2, test3)
rm(rate)
rate <- filter(test4, Species == "Reticulitermes flavipes")
rate <-   mutate(rate, Methane.production.rate = as.numeric(Methane.production.rate)

testikng <- mean(rate$Methane.production.rate)

                 