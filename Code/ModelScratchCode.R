###Scratch Code for trying out various model structures and such
##Research Derby--The Edge of Significance 3/31/22

library(lme4)
#Include year as a random effect

mod4<-glmer.nb(Length ~ scale(Duration) + scale(Structures) + scale(Hectares) + scale(PopSize) + scale(Income) + (1|Year), data = fires)
summary(mod4)

AIC(mod4)
AIC(mod3)  ###going with simpler model
