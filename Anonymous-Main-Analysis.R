
#_______________________________________________________
#Load Library----
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
}
#citation("nlme")
#_______________________________________________________
#Load data----
#_______________________________________________________
if(TRUE){
  rawdata <- read.csv("rawdata.csv")
  
  season <- read.csv("Season.csv")
  
  plotinfo <- read.csv("Cascade_PlotInfo.csv")
  
}
#_______________________________________________________
#Make and clean df----
#_______________________________________________________
if(TRUE){
  plotinfo <- rename(plotinfo, Treatment = Treatment..PT..HT..C.)
  
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
  
  #forest 
  oldforest <- filter(nmin, Plot == c(1:15))
  youngforest <- filter(nmin, Plot == c(16:30))
}
#check predictor variables 
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
                   labels = c("Control", "Detritivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))
p1 

if(TRUE){
  pdf("NminTreatment.pdf", width = 7, height = 5)
  plot(p1)
  dev.off()}
#_______________________________________________________
#Both Forests Nmin ----  Poster version
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
                   labels = c("Control", "Detritivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
p

if(TRUE){
  pdf("NminTreatment.pdf", width = 7, height = 5)
  plot(p)
  dev.off()}
#_______________________________________________________
##Delta N ---- young forest more availab n
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

#_______________________________________________________
##Delta TC TN----
#_______________________________________________________

p2 <- ggplot(nmin, aes(x=Treatment, y=TCDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 2, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_classic()+
  labs(#x = 'Ground Beetle Treatment', 
    y = 'Change in Total Carbon (%)',
    fill='Forest Type')+
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        axis.text.x=element_text(size=18), 
        axis.text.y=element_text(size=12))+
  theme(title=element_text(size=rel(1.2)))+
  scale_x_discrete(name ="", 
                   limits = c("CT","HT","PT"), 
                   labels = c("Control", "Detritivore\n+Predator","Predator"))+
  theme(axis.title.x = element_text(margin = margin(t = 5, b=5)), 
        axis.title.y = element_text(margin = margin(l = 5, r=5)), 
        axis.text.x=element_text(margin = margin(t=10)), 
        axis.text.y=element_text(margin = margin(r = 10)))+
  theme(legend.position = "top")
p2



pdf("totalC.pdf", width = 7, height = 5)
plot(p2)
dev.off()
#ns
hist(nmin$TNDelta)#leftish
hist(log(nmin$TNDelta+4))#normal with adding constant
m4 <- lme(log(TNDelta+4) ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m);shapiro.test(resid(m));Anova(m);lsmeans(m, pairwise~Forest, adjust="tukey")

hist(nmin$TCDelta)#normal
m5 <- lme(TCDelta ~ Treatment*Forest,random = ~1| Block, data=nmin);summary(m5);shapiro.test(resid(m5));Anova(m5);lsmeans(m5, pairwise~Forest, adjust="tukey")

#_______________________________________________________
#Old vs new* forest----
#_______________________________________________________

p4 <- ggplot(nmin, aes(x=Forest, y=NminDelta, fill = Forest))+
  geom_half_boxplot(side = "l",  outlier.shape = 17)+
  geom_half_point(aes(color = Forest), 
                  side = "r", show.legend = F,
                  size = 1, alpha = .5)+
  scale_fill_manual(values = c("salmon", "paleturquoise3")) +
  theme_classic()+
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


pdf("NminForest.pdf", width = 6, height = 5)
plot(p4)
dev.off()

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
#### *Delta *TC *TN Young----
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
##Stats Table
#_______________________________________________________

# Create a list to store ANOVA results
anova_results_list <- list()

# Loop through the models (adjust the range as needed)
for (i in 1:15) {
  # Calculate ANOVA for each model (a1 through a49)
  anova_result <- anova(get(paste0("m", i)))
  anova_result$Response <- as.character(formula(get(paste0("m", i)))[[2]])
  
  # Add the ANOVA result to the list
  anova_results_list[[i]] <- data.frame(anova_result)
}

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
  mutate(Order = 1:40)
rm(table_df)
table_df <- combined_df %>%
  select(Response, term, F_value, Order) %>%
  tidyr::pivot_wider(names_from = "term", values_from = "F_value")

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

lm6 <- as.data.frame(lm6$contrasts)
lm7 <- as.data.frame(lm7$contrasts)

table2 <- lm6%>%
  kbl() %>%
  kable_styling()
table2
table3 <- lm7%>%
  kbl() %>%
  kable_styling()
table3


#--------------------testing WCH and Ph
m <- lme(NminDelta ~ Treatment+pHDelta+WHCDelta,random = ~1| Block, data=youngforest);summary(m6);shapiro.test(resid(m6));Anova(m6);lsmeans(m6, pairwise~Treatment, adjust="tukey");anova(m6)#HT estimate -0.22587237, p 0.0069

m <- lme(NnitDelta ~ Treatment+pHDelta+WHCDelta,random = ~1| Block, data=youngforest);summary(m7);shapiro.test(resid(m7));Anova(m7);lsmeans(m7, pairwise~Treatment, adjust="tukey") #HT estimate -0.21302869, p 0.0696

m <- lme(NamDelta ~ Treatment+pHDelta+WHCDelta,random = ~1| Block, data=youngforest);summary(m8);shapiro.test(resid(m8));Anova(m8);lsmeans(m8, pairwise~Treatment, adjust="tukey")   

#_______________________________________________________
##End-
#_______________________________________________________
