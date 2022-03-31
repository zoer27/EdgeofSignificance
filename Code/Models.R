##Research Derby 3/30/2028
###Questions: How has google search interest in wildfires change over time? 
#How does the population size and wealth index affect national interest in wildfires?

#model ideas:
#GLM with length of interest (days) with covariates including population size and wealth

library(tidyverse)


# Data Import -------------------------------------------------------------
fires<-read_csv("Data/WildFireData.csv")
fires<-fires[1:25,]

#reformatting data
fires<-fires %>% rename(Length = `Length of Nationwide Google Interest`)

str(fires)

# Population Size and Wealth Models For NATIONAL ---------------------------------------------------------------------
#pairs plot for covariates
pairs(fires[,c(3,6:13,15)],na.action = stats::na.pass)
#cor values

cors<-cor(fires[,c(3,6:13,15)])

pairs(fires[,c(6,10)])
cor(fires[,c(6,10)])

#Deaths and Structures are correlated--only use ONE of these
#counties and hectares are correlated --only use ONE of these

#GLM for length of interest (days)--Poisson distribution
mod1<-glm(Length ~ )
#Test model fit

#Test for overdispersion

#Include year as a random effect

#Test random effect inclusion


#GLM for ratio of peak interest


# Models for California ---------------------------------------------------



