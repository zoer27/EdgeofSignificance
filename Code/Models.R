##Research Derby 3/30/2028
###Questions: How has google search interest in wildfires change over time? 
#How does the population size and wealth index affect national interest in wildfires?

#model ideas:
#GLM with length of interest (days) with covariates including population size and wealth

library(tidyverse)
library(MASS)
library(lme4)
library(RLRsim)

# Data Import -------------------------------------------------------------
fires<-read_csv("Data/WildFireData.csv")
fires<-fires[1:25,]

#reformatting data
fires<-fires %>% rename(Length = `Length of Nationwide Google Interest`, County = `Largest County (by pop size)`, Income = `Median Household Income`, Peak = `Peak Interest`, PeakCA = `Peak Interest CA`, PopSize = `Pop Size`)

str(fires)

# Population Size and Wealth Models For NATIONAL ---------------------------------------------------------------------
#pairs plot for covariates
pairs(fires[,c(3,6:13,15)],na.action = stats::na.pass)
#cor values

cors<-cor(fires[,c(3,6:13,15)], use = "pairwise.complete.obs")

pairs(fires[,c(6,10)])
cor(fires[,c(6,10)])

#Deaths and Structures and Costs are correlated--only use ONE of these
#counties and hectares are correlated --only use ONE of these
#very few data for Number of Fires DO NOT INCLUDE

pairs(fires[,c(15:16)]) #not correlated
cor(fires[,c(15:16)])

#GLM for length of interest (days)--Poisson distribution
mod1<-glm(Length ~ Duration + Structures + Hectares + PopSize + Income, family = "poisson", data = fires)
summary(mod1)

#Test model fit

#Test for overdispersion
mod2<-glm(Length ~ Duration + Structures + Hectares + PopSize + Income, family = "quasipoisson", data = fires)
summary(mod2)

#negative binomial
mod3<-glm.nb(Length ~ Duration + Structures + Hectares + PopSize + Income, data = fires)
summary(mod3)

#Include year as a random effect

mod4<-glmer.nb(Length ~ scale(Duration) + scale(Structures) + scale(Hectares) + scale(PopSize) + scale(Income) + (1|Year), data = fires)
summary(mod4)

AIC(mod4)
AIC(mod3)  ###going with simpler model

#check covariates:


#GLM for ratio of peak interest


# Models for California ---------------------------------------------------



