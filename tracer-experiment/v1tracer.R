#_______________________________________________________
#Import data----
#_______________________________________________________

library(dplyr)
library(ggplot2)
library(lme4)
library(ggrepel)

#_______________________________________________________
#Load data and clean
#_______________________________________________________
invert <- read.csv("/Users/JaneyLienau/Desktop/data/inverts.csv")
tins <- read.csv("/Users/JaneyLienau/Desktop/data/tins.csv")
isotope <- read.csv("/Users/JaneyLienau/Desktop/data/data.csv")
field <- read.csv("/Users/JaneyLienau/Desktop/data/field.csv")
tinweights <- read.csv("/Users/JaneyLienau/Desktop/data/tins.csv")
gb <- read.csv("/Users/JaneyLienau/Desktop/data/GB.csv")
soil <- read.csv("/Users/JaneyLienau/Desktop/data/soil.csv")

#_______________________________________________________
#Make and clean df----
#_______________________________________________________
#inverte df
tinweights <- tinweights%>%
  mutate(., TinNum = as.character(TinNum))

invert <- invert  %>% 
  mutate(Date=recode(Date, '44699'='May', '7/13/2022'='July', '10/18/2022'='Oct'),
         Plot=recode(Plot, '1'='Plot_01', '2'='Plot_02', '3'='Plot_03','4'='Plot_04', '5'='Plot_05',
                     '6'='Plot_06', '7'='Plot_07', '8'='Plot_08','9'='Plot_09', '10'='Plot_10',
                     '11'='Plot_11','12'='Plot_12','13'='Plot_13','14'='Plot_14', '15'='Plot_15',
                     '16'='Plot_16','17'='Plot_17','18'='Plot_18','19'='Plot_19','20'='Plot_20'))%>%
  filter(., Date == c("May", "July", "Oct"))%>%
  mutate(., TinNum = as.character(TinNum),
         DryMass = as.numeric(DryMass))%>%
  left_join(., tinweights, by = 'TinNum')%>%
  mutate(Biomass = TinWSpec-TinWeight)%>%                   #calculate biomass of species from the tines
  mutate(Biomass = coalesce(Biomass, 0))%>%                    #replace NA with 0 
  mutate(DryMass = coalesce(DryMass, 0))%>%
  mutate(Biomass = DryMass+Biomass)%>%                         #combind two different methods of weighing biomass
  filter(!IsotopeNote == "Not enough Ground sample to run")%>%
  select(-TinNum, -DryMass, -TinWSpec, -TinWeight, -TinWeight2,-IsotopeNote)%>%  #remove unnecessary columns
  filter(!Individuals ==  "1(sleg)",
         !Individuals < 1,
         !Code == "?????",
         !Code == "COB",
         !Code == "PRO")%>%
  rename(., ID = "Vial")  #remove orders that were not sampled at a plot

#combind isotope with invetebrate data to calculate %Recovery
tray <- filter(isotope, !Tray == "GB",
               !Tray == "Soil")
invertisotope <-   left_join(invert, tray, by = "ID")
invertisotope <- left_join(invertisotope, field, by = "Plot")

invertNrecovery <- invertisotope
invertNrecovery <- invertNrecovery%>%
  mutate(., FractionRec = (Nratio - Air)/(AtomN-Air))%>%
  mutate(., MassRecovered = (FractionRec*Biomass*(PerN/100)))%>%
  mutate(., PercentRecovered = ((MassRecovered/gNappliedPlot)*100))


#combind isotope with GB data to calculate %Recovery
gb <- rename(gb, TinNum = "Tin")%>%
  mutate(., TinNum = as.character(TinNum))%>%
  left_join(., tinweights, by = 'TinNum')%>%
  mutate(., Biomass = SamWeightWTinWeight-TinWeight2)%>%
  select(-TinNum, -SampleBout, -IsotopeWeight, -SamWeightWTinWeight, -Note, -TinWeight, -TinWeight2, -ScientificName)%>%
  mutate(Date=recode(Date, '5/18/22'='May', '7/13/22'='July', '10/18/22'='Oct'))%>%
  rename(ID = "Isotope.lable",
         Plot = "Location")%>%
  mutate(., ID = as.character(ID))

trayGB <- filter(isotope, Tray == "GB")%>%
  mutate(., ID = as.character(ID))

GBisotope <-   left_join(gb, trayGB, by = "ID")
GBisotope <- left_join(GBisotope, field, by = "Plot")

GBNrecovery <- GBisotope
GBNrecovery <- GBNrecovery%>%
  mutate(., FractionRec = (Nratio - Air)/(AtomN-Air))%>%
  mutate(., MassRecovered = (FractionRec*Biomass*(PerN/100)))%>%
  mutate(., PercentRecovered = ((MassRecovered/gNappliedPlot)*100))


#make a df with soil and isotope to cal % recovery in the soil

#DF DOESN'T WORK!!
SoilIsotope <-   left_join(soil, soiltray, by = "ID")
SoilIsotope <- left_join(SoilIsotope, field, by = "Plot")
names(SoilNrecovery)
SoilNrecovery <- SoilIsotope
SoilNrecovery <- SoilNrecovery%>%
  mutate(., FractionRec = (Nratio - Air)/(AtomN-Air))%>%
  mutate(., MassRecovered = (FractionRec*BulkDensity*(PerN/100)))%>%
  mutate(., PercentRecovered = ((MassRecovered/gNappliedPlot)*100))

#estimate Biomass for May samples of GB


#_______________________________________________________
#Figure - %Recovery by Treatment
#_______________________________________________________
names(GBNrecovery)
p1 <- ggplot(GBNrecovery, aes(x=Treatment, y=PercentRecovered, fill = Treatment), na.action(na.omit))+
  geom_boxplot()
p1

m1 <- glm(PercentRecovered~Treatment, data = GBNrecovery)
summary(m1)

p2 <- ggplot(invertNrecovery, aes(x=Treatment, y=PercentRecovered, fill = Treatment), na.action(na.omit))+
  geom_boxplot()
p2
m2 <- glm(PercentRecovered~Treatment, data = invertNrecovery)
summary(m2)


#_______________________________________________________
#Figure - %Recovery by Forest
#_______________________________________________________
names(GBNrecovery)
p3 <- ggplot(GBNrecovery, aes(x=ForestType, y=PercentRecovered, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p3

m3 <- glm(PercentRecovered~ForestType, data = GBNrecovery)
summary(m3)

p4 <- ggplot(invertNrecovery, aes(x=ForestType, y=PercentRecovered, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p4
m4 <- glm(PercentRecovered~ForestType, data = invertNrecovery)
summary(m4)

#_______________________________________________________
#Figure - %Recovery  through time
#_______________________________________________________
names(GBNrecovery)
p5 <- ggplot(GBNrecovery, aes(x=Date, y=PercentRecovered, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p5

m5 <- glm(PercentRecovered~Date, data = GBNrecovery)
summary(m5) #there is a difference through time (more in october, small franction)

p6 <- ggplot(invertNrecovery, aes(x=Date, y=PercentRecovered, fill = Date), na.action(na.omit))+
  geom_boxplot()
p6
m6 <- glm(PercentRecovered~Date, data = invertNrecovery)
summary(m6)

#_______________________________________________________
#Figure - %Recovery by Order and species
#_______________________________________________________

p7 <- ggplot(GBNrecovery, aes(x=SpeciesCode, y=PercentRecovered, fill = SpeciesCode), na.action(na.omit))+
  geom_boxplot()
p7
m6 <- glm(PercentRecovered~SpeciesCode, data = GBNrecovery)
summary(m6)


p8 <- ggplot(invertNrecovery, aes(x=Code, y=PercentRecovered, fill = Code), na.action(na.omit))+
  geom_boxplot()
p8
m8 <- glm(PercentRecovered~Code, data = invertNrecovery)
summary(m8) #yes differences 


#_______________________________________________________
#Figure - %Recovery soil
#_______________________________________________________

p9 <- ggplot(SoilNrecovery, aes(x=ForestType, y=PercentRecovered, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p9
m9 <- glm(PercentRecovered~ForestType, data = SoilNrecovery)
summary(m9)


p10 <- ggplot(SoilNrecovery, aes(x=Treatment, y=PercentRecovered, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p10
m10 <- glm(PercentRecovered~Treatment, data = SoilNrecovery)
summary(m10) #yes differences 0.0633


p11 <- ggplot(SoilNrecovery, aes(x=Date, y=PercentRecovered, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p11

m11 <- glm(PercentRecovered~Date, data = SoilNrecovery)
summary(m11)

#_______________________________________________________
#Figure - Delta 15 N and 13C both forests 
#_______________________________________________________
names(GBNrecovery)
testing <- GBNrecovery%>%
  filter(Date == "Oct")
p12 <- ggplot(testing, aes(x=SpeciesCode, y=d15N, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p12


testing2 <- GBNrecovery%>%
  filter(Date == "July")
p13 <- ggplot(testing2, aes(x=SpeciesCode, y=d15N, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p13


testing3 <- GBNrecovery%>%
  filter(Date == "May")
p14 <- ggplot(testing3, aes(x=SpeciesCode, y=d15N, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p14

#all species delt 15N
p15 <- ggplot(GBNrecovery, aes(x=SpeciesCode, y=d15N, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p15

#all species delt 13C
p15 <- ggplot(GBNrecovery, aes(x=SpeciesCode, y=d13C, fill = ForestType), na.action(na.omit))+
  geom_boxplot()
p15

##

p15 <- ggplot(GBNrecovery, aes(x=d15N, y=d13C), na.action(na.omit))+
 
  geom_text(label=GBNrecovery$SpeciesCode)
p15

#take the mean of each species for D15N and D13C
meandelta <- GBNrecovery%>%
  select(SpeciesCode,
         d15N,
         d13C)
meandelta <- meandelta%>%
  group_by(., SpeciesCode)%>%
  mutate(., meand15N = mean(d15N))%>%
  mutate(., meand13C = mean(d13C))

meandelta <- meandelta%>%
  select(SpeciesCode,
    meand15N,
         meand13C)%>%
  distinct()


pd15 <- ggplot(meandelta, aes(x=meand15N, y=meand13C), na.action(na.omit))+
  geom_text(label=meandelta$SpeciesCode)
pd15

#group by genus

#take the mean of each species for D15N and D13C
meandeltaGenus <- GBNrecovery%>%
  select(Genus,
         d15N,
         d13C)
meandeltaGenus <- meandeltaGenus%>%
  group_by(., Genus)%>%
  mutate(., meand15N = mean(d15N))%>%
  mutate(., meand13C = mean(d13C))

meandeltaGenus <- meandeltaGenus%>%
  select(Genus,
         meand15N,
         meand13C)%>%
  distinct()

write.csv(meandeltaGenus, "/Users/JaneyLienau/Desktop/GitHubRepository/beetle-trophic-cascade/meandeltaGenus.csv", row.names=FALSE)

#--------------------------------------------------------------------
# d13C~d15N plot of genus to show tropic position for Trophic cascade paper
#------------------------------------------------------------------------
pGd15 <- ggplot(meandeltaGenus, aes(x=meand15N, y=meand13C), na.action(na.omit))+
  geom_text_repel(label=meandeltaGenus$Genus)+
  labs(x = 'Mean Delta N-15', 
    y = 'Mean Delta C-13')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme_minimal()
pGd15

pdf("/Users/JaneyLienau/Desktop/MeanDeltaN-C.pdf", width = 6, height = 5)
plot(pGd15)
dev.off()


