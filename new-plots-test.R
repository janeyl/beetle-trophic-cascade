#testing new plots out

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

#testing changes
