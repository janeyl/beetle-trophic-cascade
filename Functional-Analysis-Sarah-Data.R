#CHECK~ move to git hub if goes into pub
beetleFunction <- read.csv("/Users/JaneyLienau/Desktop/WorkingData/beetle-functional-table.csv")
sarahSpecies <- read.csv("/Users/JaneyLienau/Desktop/WorkingData/species_data.csv")
speciesCode <- read.csv("/Users/JaneyLienau/Desktop/WorkingData/speciesCode.csv")


#_______________________________________________________
##Functional analysis 
#_______________________________________________________

test.df <- sarahSpecies%>%
  select(-Location, -Shannon, -Simpson, -invSimpson, -Richness, -Abundance, -Evenness)%>%
  tidyr::pivot_longer(., CYPL:LAPA, 
                      names_to = "Species", 
                      values_to = "Count")
sum(test.df$Count)#1077
sum(sarahSpecies[,c(4:73)]) #1077


speciesCode <- speciesCode%>%
  distinct()

function.df <- test.df%>%
  left_join(., beetleFunction, by = "Species")%>%
  na.omit()%>%
  filter(Count!=0)

functionTest <- test.df%>%
  left_join(., beetleFunction, by = "Species")%>%
  filter(!is.na(ScientificName))%>%
  filter(Count!=0)%>%
  filter(AgeClassTxt!="24-29 years")%>%#testing to see if removing this weird intermediate age class helps
  filter(AgeClassTxt!="14-18 years")%>%na.omit()

sum(functionTest$Count)#486 lost a half with cutting out the age groups

#combind age class 1-2, and 2-3
functionTest <- functionTest%>%
  mutate(AgeClass = sub("1", "2", AgeClass))#%>%
#  mutate(AgeClass = sub("3", "4", AgeClass))
rm(functionTest2)
functionTest2 <- functionTest %>%
  group_by(AgeClassTxt, Species, Function) %>%
  summarise(Count = sum(Count, na.rm = TRUE))
sum(functionTest2$Count)#479

functionTest3 <- functionTest %>%
  group_by(AgeClassTxt, Function) %>%
  summarise(Count = sum(Count, na.rm = TRUE))
sum(functionTest3$Count)#479
#_______________________________________________________
##ground beetle density for species of interest
#_______________________________________________________
# 6 pitfall traps per stand, four stands within each age class, =48 pitfall traps
#for just these two age classes 48 ave species of interest total 32.77381
#functionTest
averageCounts <- functionTest %>%
  filter(Species == "HAPE" | Species == "PTTR"|Species=="PTRO"|Species=="PTPE") %>%
  group_by(Species) %>%
  summarise(AverageCount = mean(Count, na.rm = TRUE))
sum(averageCounts$AverageCount)#=32.77381
#_______________________________________________________
##plot functional groups across age classes
#_______________________________________________________
functionalgroups <- ggplot(functionTest, aes(x=AgeClassTxt, y=Count, fill = Function))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Function), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  #  scale_fill_manual(values = c("salmon", "paleturquoise3","gold1")) +
  theme_classic()+
  labs(x = 'Forest Type', 
       y = 'Species Count',
       fill='Functional Group')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="Forest Type", 
                   limits = c("Control","1-8 years"), 
                   labels = c("Old Forest", "Young Forest"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
functionalgroups
pdf("/Users/JaneyLienau/Desktop/functionalgroups.pdf", width = 6, height = 5)
plot(functionalgroups)
dev.off()

functionalgroups3 <- ggplot(functionTest3, aes(x=AgeClassTxt, y=Count, fill = Function))+
  geom_bar(stat = "identity") +
  theme_classic()+
  labs(x = 'Forest Type', 
       y = 'Species Count',
       fill='Functional Group')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="Forest Type", 
                   limits = c("Control","1-8 years"), 
                   labels = c("Old Forest", "Young Forest"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
functionalgroups3

m <- lm(Count ~ Function, data=functionTest2);summary(m);Anova(m);lsmeans(m, pairwise~Function, adjust="tukey")
anova(m)

