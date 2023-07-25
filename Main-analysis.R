
#_______________________________________________________
#Import data----
#_______________________________________________________
if(TRUE){
library(dplyr)
library(ggplot2)
library(corrplot)
library(lme4)
library(car)
library(nlme)
library(emmeans)
library(ggpol)
library(RColorBrewer)
library(cowplot)
library(ggrepel)
library(gghalves)
}
#when you're ready: put model estimates in a nice table:
#https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
#_______________________________________________________
#Load data----
#_______________________________________________________
if(TRUE){
rawdata <- read.csv("/Users/JaneyLienau/Desktop/GitHubRepository/beetle-trophic-cascade/rawdata.csv")

season <- read.csv("/Users/JaneyLienau/Desktop/GitHubRepository/beetle-trophic-cascade/Season.csv")

plotinfo <- read.csv("/Users/JaneyLienau/Desktop/GitHubRepository/beetle-trophic-cascade/Cascade_PlotInfo.csv")

plotinfo <- rename(plotinfo, Treatment = Treatment..PT..HT..C.)

meandeltaGenus <- read.csv("/Users/JaneyLienau/Desktop/GitHubRepository/beetle-trophic-cascade/meandeltaGenus.csv")
}
#_______________________________________________________
#Make and clean df----
#_______________________________________________________
if(TRUE){
nmin <- left_join(rawdata, plotinfo, by = "Plot")

#calculate delta for all variables
nmin <- mutate(nmin, 
               NminDelta = Nmin_Sept-Nmin_June,
               NnitDelta = Nnit_Sept-Nnit_June,
               NamDelta = Nam_Sept-Nam_June,
               TNDelta = TN_Sept-TN_June,
               TCDelta = TC_Sept-TC_June,
               pHDelta = pH_Sept-pH_June,
               WHCDelta = WHC_Sept-WHC_June)

#remove other variables
nmin <- select(nmin, -c(Nmin_June:TC_Sept))
nmin <- select(nmin, -c(Initial_Stock:Notes))

#forest 
oldforest <- filter(nmin, Plot == c(1:15))
youngforest <- filter(nmin, Plot == c(16:30))
}

#_______________________________________________________
#Both Forests Nmin ----
#_______________________________________________________

p1 <- ggplot(nmin, aes(x=Treatment, y=NminDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Change in Net Nitrogen Mineralization\n(ug N g-1 day-1)',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="Ground Beetle Treatment", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Detritivore","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))
p1 

if(TRUE){
pdf("/Users/JaneyLienau/Desktop/NminTreatment.pdf", width = 7, height = 5)
plot(p1)
dev.off()}
#_______________________________________________________
#Both Forests Nmin ---- ESA Poster version
#_______________________________________________________

p <- ggplot(nmin, aes(x=Treatment, y=NminDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 2, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_classic()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Change in net nitrogen mineralization\n(ug N g-1 day-1)',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=18), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Detritivore","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
p

if(TRUE){
  pdf("/Users/JaneyLienau/Desktop/NminTreatmentESA.pdf", width = 7, height = 5)
  plot(p)
  dev.off()}
#_______________________________________________________
##Delta N ---- young forest more availab n
#_______________________________________________________
#nmin
m <- lme(NminDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Forest, adjust="tukey")
#estimate 0.6061286, pvalue- 0.0302
#nnit
m <- lme(NnitDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Forest, adjust="tukey")
#nam
m <- lme(NamDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Forest, adjust="tukey")
#estyoung   0.4427553 p-value 0.0056

#_______________________________________________________
##Delta TC TN----
#_______________________________________________________
#ns
hist(nmin$TNDelta)#leftish
hist(log(nmin$TNDelta+4))#normal with adding constant
hist(nmin$TCDelta)#normal


p2 <- ggplot(nmin, aes(x=Treatment, y=TCDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Change in Total Carbon (%)',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="Ground Beetle Treatment", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Detritivore","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))
p2



pdf("/Users/JaneyLienau/Desktop/totalC.pdf", width = 7, height = 5)
plot(p2)
dev.off()

m <- lme(TNDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Forest, adjust="tukey")
m <- lme(TCDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Forest, adjust="tukey")

#_______________________________________________________
#Old vs new* forest----
#_______________________________________________________

p4 <- ggplot(nmin, aes(x=Forest, y=NminDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Change in Net Nitrogen Mineralization\n(ug N g-1 day-1)',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="Forest Type", 
                   limits = c("Old","Young"), 
                   labels = c("Old Forest", "Young Forest"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))
p4


pdf("/Users/JaneyLienau/Desktop/NminForest.pdf", width = 6, height = 5)
plot(p4)
dev.off()

#--------------------------------------------------------------------
# d13C~d15N plot of genus to show tropic position for Trophic cascade paper
#------------------------------------------------------------------------

p5 <- ggplot(meandeltaGenus, aes(x=meand15N, y=meand13C), na.action(na.omit))+
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
p5

pdf("/Users/JaneyLienau/Desktop/MeanDeltaN-C.pdf", width = 6, height = 5)
plot(p5)
dev.off()

#_______________________________________________________
## *Delta N min Young---- 
#_______________________________________________________
m <- lme(NminDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey")#HT estimate -0.22587237, p 0.0069

m <- lme(NnitDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey") #HT estimate -0.21302869, p 0.0696

m <- lme(NamDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey")   

#check normality
plot(rstudent(m) ~ m$fitted.values, pch = 19, col = 'red', xlab = "Fitted Values", ylab = "Studentized Residuals",
     main = paste("Fits vs. Studentized Residuals,", label = m$terms))
abline(h = 0, lwd = 3)
abline(h = c(2,-2), lty = 2, lwd = 2, col="blue")
abline(h = c(3,-3), lty = 2, lwd = 2, col="green")

boxplot(rstudent(m) ~ youngforest$Treatment)#not even
boxplot(rstudent(m) ~ youngforest$pHDelta)#not even
boxplot(rstudent(m) ~ youngforest$WHCDelta)#not even
summary(m)



#_______________________________________________________
## Delta N min Old---- 
#_______________________________________________________

m <- lme(NminDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey")

m <- lme(NnitDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey") 

m <- lme(NamDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey")   

#_______________________________________________________
#### *Delta *TC *TN Young----
#_______________________________________________________
m <- lme(TNDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey")   
m <- lme(TCDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey")   
#_______________________________________________________
##Delta TC TN Old----
#_______________________________________________________

m <- lme(TNDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey")   
m <- lme(TCDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey")   
#_______________________________________________________
##End-
#_______________________________________________________



