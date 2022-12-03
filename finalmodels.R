#models to motivate differences in avail N by forest type
m <- glm(NminDelta ~ Treatment*Forest, data=nmin)
summary(m)

m <- glm(NnitDelta ~ Treatment*Forest, data=nmin)
summary(m)

m <- glm(NamDelta ~ Treatment*Forest, data=nmin)
summary(m)


#young
m <- glm(NminDelta ~ Treatment, data=youngforest)
summary(m)

m <- glm(NnitDelta ~ Treatment, data=youngforest)
summary(m)

m <- glm(NamDelta ~ Treatment, data=youngforest)
summary(m)

#Old
m <- glm(NminDelta ~ Treatment, data=oldforest)
summary(m)

m <- glm(NnitDelta ~ Treatment, data=oldforest)
summary(m)

m <- glm(NamDelta ~ Treatment, data=oldforest)
summary(m)

#use AIC to select model with pH and WHC
