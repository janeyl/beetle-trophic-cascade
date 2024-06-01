#___________________________________________________________________________________________________________
## Janey Lienau
## Title: Ground beetle trophic interactions alter available nitrogen in temperate forest soil
## Created: Fall 2022
## Last Modified: May 31 2024
#___________________________________________________________________________________________________________

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
library(kableExtra) #
library(broom) 
library(spdep)
}
#citation("nlme")
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
               WHCDelta = WHC_Sept-WHC_June,
              CtoN_June = TC_June/TN_June,
              CtoN_Sept = TC_Sept/TN_Sept,
              CtoN = CtoN_Sept-CtoN_June)

#forest 
oldforest <- filter(nmin, Plot == c(1:15))
youngforest <- filter(nmin, Plot == c(16:30))
}
#___________________________________________________________________________________________________________
##Figures
#___________________________________________________________________________________________________________

#_______________________________________________________
#Figure 3: Both Forests Nmin change and real numbers
#_______________________________________________________
#Figure 3 (A)
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
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
p1 

if(TRUE){
pdf("/Users/JaneyLienau/Desktop/NminTreatment.pdf", width = 7, height = 5)
plot(p1)
dev.off()}

##Figure 3 (B & C)
p1.1 <- ggplot(nmin, aes(x=Treatment, y=Nmin_June, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Initial Net Nitrogen Mineralization\n(ug N g-1 day-1)',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "none")
p1.1 
p1.2 <- ggplot(nmin, aes(x=Treatment, y=Nmin_Sept, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Final Net Nitrogen Mineralization\n(ug N g-1 day-1)',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "none")
p1.2

prow2 <- plot_grid(p1.1, p1.2, align = 'h',
  hjust = -1
)
prow2
if(TRUE){
  pdf("/Users/JaneyLienau/Desktop/p1.3.pdf", width = 10, height = 5)
  plot(prow2)
  dev.off()}
#composite Figure 3 parts A,B,C in illistrator

#_______________________________________________________
## Figure 4: Delta TC, TN
#_______________________________________________________

p2.1 <- ggplot(nmin, aes(x=Treatment, y=TCDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(x = 'Ground Beetle Treatment', 
    y = 'Change in Total Carbon (%)',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "right")
p2.1
p2.2 <- ggplot(nmin, aes(x=Treatment, y=TCDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(x = 'Ground Beetle Treatment', 
    y = 'Change in Total Nitrogen (%)',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "none")
p2.2

prow3 <- plot_grid(p2.1 + theme(legend.position="none"),
                   p2.2, align = 'h')
prow3
# extract the legend from one of the plots
legend <- get_legend(p2.1)

if(TRUE){
  pdf("/Users/JaneyLienau/Desktop/Figure-4-test.pdf", width = 7, height = 5)
  plot(prow3)
  dev.off()}

test <- plot_grid(p2.3, legend, rel_widths = c(2,.4))
test
if(TRUE){
  pdf("/Users/JaneyLienau/Desktop/figre5-legend.pdf", width = 7, height = 5)
  plot(test)
  dev.off()}
#combind in illistrator
#_______________________________________________________
## Figure 5: C:N ratio
#_______________________________________________________
p2.3 <- ggplot(oldforest, aes(x=Treatment, y=CtoN))+
  geom_half_boxplot(side = "l",  outlier.shape = 17, aes(fill = "FixedColor")) +
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("FixedColor" = "salmon")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Change in C:N Ratio')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "none")
p2.3
p2.4 <- ggplot(nmin, aes(x=Treatment, y=CtoN_June, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Initial C:N Ratio',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
p2.4
p2.5 <- ggplot(nmin, aes(x=Treatment, y=CtoN_Sept, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Final C:N Ratio',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
p2.5
p2.6 <- ggplot(nmin, aes(x=Treatment, y=CtoN, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Change in C:N Ratio',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
p2.6

if(TRUE){
  pdf("/Users/JaneyLienau/Desktop/Figure-5.pdf", width = 5, height = 5)
  plot(p2.3)
  dev.off()}
#___________________________________________________________________________________________________________
##Statistics
#___________________________________________________________________________________________________________
#_______________________________________________________
#Moran's I test
#_______________________________________________________

nb2listw(nmin, style = "W")
lm.morantest(m1, nb2listw(nmin, style="W"))
# Fit your linear mixed-effects model
m1 <- lme(NminDelta ~ Treatment*Forest, random = ~1|Block, data = nmin)
# Compute residuals
residuals_m1 <- resid(m1)
# Ensure coords_m1 represents spatial coordinates, e.g., cbind(longitude, latitude)
coords_m1 <- cbind(nmin$Plot, nmin$Forest)  # Adjust this to your actual spatial coordinates
# Create spatial weights matrix (example using knn neighbors)
nearby_m1 <- knn2nb(knearneigh(coords_m1, k = 3))  # Adjust k as needed
listw_m1 <- nb2listw(nearby_m1, style="W")  # Convert to weights list and row-standardize
# Test for spatial autocorrelation
moran_res_m1 <- moran.test(residuals_m1, listw = listw_m1)
# Print Moran's I test results
print(moran_res_m1)
#_______________________________________________________
## Stats Comparing both forests
#_______________________________________________________
#nmin
hist(nmin$NminDelta)
m1 <- lme(NminDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m1);shapiro.test(resid(m1));Anova(m1);lsmeans(m1, pairwise~Forest, adjust="tukey");anova(m1)
#estimate 0.6061286, pvalue- 0.0302
#nnit
hist(nmin$NnitDelta)
m2 <- lme(NnitDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m2);shapiro.test(resid(m2));Anova(m2);lsmeans(m2, pairwise~Forest, adjust="tukey");anova(m2)
#nam
hist(nmin$NamDelta)
m3 <- lme(NamDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m3);shapiro.test(resid(m3));Anova(m3);lsmeans(m3, pairwise~Forest, adjust="tukey");anova(m3)
#estyoung   0.4427553 p-value 0.0056
#ns
hist(nmin$TNDelta)#leftish
hist(log(nmin$TNDelta+4))#normal with adding constant
m4 <- lme(log(TNDelta+4) ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m4);shapiro.test(resid(m4));Anova(m4);lsmeans(m4, pairwise~Forest, adjust="tukey")

hist(nmin$TCDelta)#normal
m5 <- lme(TCDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m5);shapiro.test(resid(m5));Anova(m5);lsmeans(m5, pairwise~Forest, adjust="tukey")

#_______________________________________________________
## *Delta N min Young---- 
#_______________________________________________________
hist(youngforest$NminDelta)#normal
m6 <- lme(NminDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m6);shapiro.test(resid(m6));Anova(m6);lsmeans(m6, pairwise~Treatment, adjust="tukey");anova(m6)#HT estimate -0.22587237, p 0.0069
hist(youngforest$NnitDelta)#normal
m7 <- lme(NnitDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m7);shapiro.test(resid(m7));Anova(m7);lsmeans(m7, pairwise~Treatment, adjust="tukey") #HT estimate -0.21302869, p 0.0696
hist(youngforest$NamDelta)#normal
m8 <- lme(NamDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m8);shapiro.test(resid(m8));Anova(m8);lsmeans(m8, pairwise~Treatment, adjust="tukey")   

#_______________________________________________________
## Delta N min Old---- 
#_______________________________________________________
hist(oldforest$NminDelta)#normal
m9 <- lme(NminDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m9);shapiro.test(resid(m9));Anova(m9);lsmeans(m9, pairwise~Treatment, adjust="tukey")

hist(oldforest$NnitDelta)#normal
m10 <- lme(NnitDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m10);shapiro.test(resid(m10));Anova(m10);lsmeans(m10, pairwise~Treatment, adjust="tukey") 

hist(oldforest$NamDelta)#normal
m11 <- lme(NamDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m11);shapiro.test(resid(m11));Anova(m11);lsmeans(m11, pairwise~Treatment, adjust="tukey")   

#_______________________________________________________
#### Delta TC TN Young----
#_______________________________________________________
hist(youngforest$TNDelta)#normal
m12 <- lme(TNDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m12);shapiro.test(resid(m12));Anova(m12);lsmeans(m12, pairwise~Treatment, adjust="tukey")   
hist(youngforest$TCDelta)#normal
m13 <- lme(TCDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m13);shapiro.test(resid(m13));Anova(m13);lsmeans(m13, pairwise~Treatment, adjust="tukey")   
summary(m13)
#_______________________________________________________
##Delta TC TN Old----
#_______________________________________________________
hist(oldforest$TNDelta)#normal
m14 <- lme(TNDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m14);shapiro.test(resid(m14));Anova(m14);lsmeans(m14, pairwise~Treatment, adjust="tukey")   
hist(oldforest$TCDelta)#normal
m15 <- lme(TCDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m15);shapiro.test(resid(m15));Anova(m15);lsmeans(m15, pairwise~Treatment, adjust="tukey")   
#_______________________________________________________
##Delta and raw values C:N Young 
#_______________________________________________________
m16 <- lme(CtoN ~ Treatment,random = ~1| Block, data=youngforest);summary(m16);shapiro.test(resid(m16));Anova(m16);lsmeans(m16, pairwise~Treatment, adjust="tukey");anova(m16)
#m <- lme(CtoN_Sept ~ Treatment,random = ~1| Block, data=youngforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey");anova(m)
#m <- lme(CtoN_June ~ Treatment,random = ~1| Block, data=youngforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey");anova(m)
#_______________________________________________________
##Delta and raw values C:N Old 
#_______________________________________________________
m17 <- lme(CtoN ~ Treatment,random = ~1| Block, data=oldforest);summary(m17);shapiro.test(resid(m17));Anova(m17);lsmeans(m17, pairwise~Treatment, adjust="tukey");anova(m17)
#sig
#m <- lme(CtoN_June ~ Treatment,random = ~1| Block, data=oldforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey");anova(m)
#m <- lme(CtoN_Sept ~ Treatment,random = ~1| Block, data=oldforest);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Treatment, adjust="tukey");anova(m)
#sig
m18 <- lme(CtoN ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m18);shapiro.test(resid(m18));Anova(m18);lsmeans(m18, pairwise~Forest, adjust="tukey")

#--------------------testing WCH and Ph
m19 <- lme(WHCDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m19);shapiro.test(resid(m19));Anova(m19);lsmeans(m19, pairwise~Treatment, adjust="tukey");anova(m19)#
m20 <- lme(pHDelta ~ Treatment,random = ~1| Block, data=youngforest);summary(m20);shapiro.test(resid(m20));Anova(m20);lsmeans(m20, pairwise~Treatment, adjust="tukey");anova(m20)#

m21 <- lme(WHCDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m21);shapiro.test(resid(m21));Anova(m21);lsmeans(m21, pairwise~Treatment, adjust="tukey");anova(m21)#
m22 <- lme(pHDelta ~ Treatment,random = ~1| Block, data=oldforest);summary(m22);shapiro.test(resid(m22));Anova(m22);lsmeans(m22, pairwise~Treatment, adjust="tukey");anova(m22)#

#_______________________________________________________
## Stats Table
#_______________________________________________________

# Create a list to store ANOVA results
anova_results_list <- list()

# Loop through the models (adjust the range as needed)
for (i in 1:22) {
  # Calculate ANOVA for each model (a1 through a49)
  anova_result <- anova(get(paste0("m", i)))
  anova_result$Response <- as.character(formula(get(paste0("m", i)))[[2]])
  
  # Add the ANOVA result to the list
  anova_results_list[[i]] <- data.frame(anova_result)
}
#rename columns to be more specific 


# Combine ANOVA results into a single data frame
combined_df <- do.call(rbind, anova_results_list)

# Format the F value and p-value
format_p_value <- function(f_value, df1, df2, p_value) {
  formatted_f <- sprintf("%.3f", f_value)
  formatted_p <- sprintf("%.3f", p_value)
  return(paste0("F(", df1, ",", df2, ")=", formatted_f, ", p=", formatted_p))
}

combined_df$F_value <- format_p_value(combined_df$`F.value`, combined_df$`numDF`, combined_df$`denDF`, combined_df$`p.value`)

# Create a data frame with terms from row names without incrementing the numbers
combined_df$term <- rownames(combined_df)

new_data <- combined_df %>%
  mutate(term = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "(Intercept)1" ~ "Intercept",
    # Add more conditions as needed
    TRUE ~ term  # Keep other values unchanged
  ))

combined_df <- combined_df %>%
  mutate(term = sub("\\(Intercept\\)\\d+", "(Intercept)", term))%>%
  mutate(term = sub("Forest\\d+", "Forest", term))%>%
  mutate(term = sub("Treatment\\d+", "Treatment", term))%>%
  mutate(term = sub("	Treatment:Forest\\d+", "Treatment:Forest", term))%>%
  mutate(Order = 1:56)
#rm(table_df)
table_df <- combined_df %>%
  select(Response, term, F_value, Order) %>%
  tidyr::pivot_wider(names_from = "term", values_from = "F_value")
#----------------------------------Stopped here->need to adapt for adding m16 and m17, and m18-TABLE NOT WORKING!
#I extracted C:N manually
table_df <- table_df%>%
  select(-"(Intercept)")%>%
  mutate(SiteID = "test")%>%
  mutate(SiteID = ifelse(row_number() <= 12, "Treatment*Forest", SiteID))%>%
  mutate(SiteID = ifelse(row_number() >= 13 & row_number() <= 20, "TNTC*Forest", SiteID))%>%
  mutate(SiteID = ifelse(row_number() >= 21 & row_number() <= 32, "NMin*YoungForest", SiteID))%>%
  mutate(SiteID = ifelse(row_number() >= 33 & row_number() <= 40, "TNTC*YoungForest", SiteID))
  
Vars_a <- colnames(table_df)

### table test
table_test <- table_df %>% select(Response, Vars_a[4], SiteID) %>% na.omit()%>%
  left_join(table_df %>% select(Response, Vars_a[5], SiteID) %>% na.omit()) %>%
  left_join(table_df %>% select(Response, Vars_a[6], SiteID) %>% na.omit())->summary_test


allmodels <- summary_test%>%
  select("Response", "SiteID", "Forest", "Treatment:Forest")

table <- allmodels%>%
  kbl() %>%
  kable_styling()

# Print the table
table
#_______________________________________________________
#ls means on significant models
lm6 <- lsmeans(m6, pairwise~Treatment, adjust="tukey")
lm7 <- lsmeans(m7, pairwise~Treatment, adjust="tukey")
lm17 <- lsmeans(m17, pairwise~Treatment, adjust="tukey")

lm6 <- as.data.frame(lm6$contrasts)
lm7 <- as.data.frame(lm7$contrasts)
lm17 <- as.data.frame(lm17$contrasts)

table2 <- lm6%>%
  kbl() %>%
  kable_styling()
table2
table3 <- lm7%>%
  kbl() %>%
  kable_styling()
table3
table4 <- lm17%>%
  kbl() %>%
  kable_styling()
table4

#_______________________________________________________
## Table 2: average soil variables table
#_______________________________________________________
#pH
round(mean(oldforest$pH_June), 2)
round(mean(oldforest$pH_Sept), 2)

round(mean(youngforest$pH_June), 2)
round(mean(youngforest$pH_Sept), 2)
#WHC
round(mean(oldforest$WHC_June), 2)
round(mean(oldforest$WHC_Sept), 2)

round(mean(youngforest$WHC_June), 2)
round(mean(youngforest$WHC_Sept), 2)
#Total Carbon
round(mean(oldforest$TC_June), 2)
round(mean(oldforest$TC_Sept), 2)

round(mean(youngforest$TC_June), 2)
round(mean(youngforest$TC_Sept), 2)
#Total Nitrogen
round(mean(oldforest$TN_June), 2)
round(mean(oldforest$TN_Sept), 2)

round(mean(youngforest$TN_June), 2)
round(mean(youngforest$TN_Sept), 2)
# N min
round(mean(oldforest$Nmin_June), 2)
round(mean(oldforest$Nmin_Sept), 2)

round(mean(youngforest$Nmin_June), 2)
round(mean(youngforest$Nmin_Sept), 2)
# N nit
round(mean(oldforest$Nnit_June), 2)
round(mean(oldforest$Nnit_Sept), 2)

round(mean(youngforest$Nnit_June), 2)
round(mean(youngforest$Nnit_Sept), 2)

# N amon
round(mean(oldforest$Nam_June), 2)
round(mean(oldforest$Nam_Sept), 2)

round(mean(youngforest$Nam_June), 2)
round(mean(youngforest$Nam_Sept), 2)

# C:N
round(mean(oldforest$CtoN_June), 2)#13.77
round(mean(oldforest$CtoN_Sept), 2)#13.45

round(mean(youngforest$CtoN_June), 2)#18.4
round(mean(youngforest$CtoN_Sept), 2)#17.62

#___________________________________________________________________________________________________________
##Supplemental Material
#___________________________________________________________________________________________________________

#_______________________________________________________
#Supplemental Figure 1: N min in old vs new forest----
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
        axis.text.x=element_text(size=18), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("Old","Young"), 
                   labels = c("Old Forest", "Young Forest"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
p4


pdf("/Users/JaneyLienau/Desktop/NminForest.pdf", width = 6, height = 5)
plot(p4)
dev.off()


#_______________________________________________________
# Supplemental Figure 2: other soil variables
#_______________________________________________________

px <- ggplot(nmin, aes(x=Treatment, y=pHDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Change in Soil pH',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
px 

pw <- ggplot(nmin, aes(x=Treatment, y=WHCDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_minimal()+
  labs(x = 'Ground Beetle Treatment', 
    y = 'Change in WHC',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Omnivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "none")
pw
pxw <- plot_grid(px,pw, ncol = 1)
pxw


if(TRUE){
  pdf("/Users/JaneyLienau/Desktop/Supp-figX.pdf", width = 5, height = 10)
  plot(pxw)
  dev.off()}

#_______________________________________________________
##End-
#_______________________________________________________
