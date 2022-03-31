##Research Derby 3/30/2028
###Questions: How has google search interest in wildfires change over time? 
#How does the population size and wealth index affect national interest in wildfires?

#model ideas:
#GLM with length of interest (days) with covariates including population size and wealth

library(tidyverse)
library(MASS)
library(faraway)

# Data Import -------------------------------------------------------------
fires<-read_csv("Data/WildFireData.csv")
fires<-fires[1:25,]

#reformatting data
fires<-fires %>% rename(Length = `Length of Nationwide Google Interest`, 
                        County = `Largest County (by pop size)`, Income = `Median Household Income`, 
                        Peak = `Peak Interest`, PeakCA = `Peak Interest CA`, PopSize = `Pop Size`) %>%
  mutate(Pandemic = ifelse(Year %in% c(2020,2021), 1, 0)) #creating categorical variable of whether the fire was in a pandemic year

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

#GLM for length of interest (days)--Poisson distribution--using offset for duration so actually modeling a poisson rate
mod1<-glm(Length ~ Structures + Hectares + PopSize + Income + Pandemic + offset(log(Duration)), family = "poisson", data = fires)
summary(mod1)
AIC(mod1)
#Test for overdispersion
mod2<-glm(Length ~Structures + Hectares + PopSize + Income + Pandemic + offset(log(Duration)), family = "quasipoisson", data = fires)
summary(mod2)
#dispersion parameter is greater than 2 so overdispersed

#negative binomial
mod3<-glm.nb(Length ~ Structures + Hectares + PopSize + Income + Pandemic + offset(log(Duration)), data = fires)
summary(mod3)
AIC(mod3)

#diagnostics:
halfnorm(hatvalues(mod3))
summary(fires[21,]) 
summary(fires[17,]) #don't really see a reason to exclude either of these so keeping them in


#estimates
tab <- data.frame(coefficients(mod3),confint(mod3,type = "profile")[,1],confint(mod3,type = "profile")[,2])
colnames(tab) <- c("Coefficients","Lower 95%","Upper 95%")
tab
exp(tab) #on real scale

#GLM for ratio of peak interest--converting to decimal from percentage--but it's like the offset term is inherent


# Models for California ---------------------------------------------------




# Figures -----------------------------------------------------------------

#for figures want to use predictions and confidence intervals--easier to interpret than estimates

#create new data fixing the rest of the parameters (usually good to fix at their mean), and then predict on this data, 
#I think it would be nice to do this for both pandemic = 1 and pandemic = 0 so you can plot on same plot

#data for prediction across the range of Structures and at mean values of everything else for pandemic and not pandemic years
new.data.1<-data.frame(Structures = rep(seq(0, 20000, by = 1), 2), Hectares = rep(108714,40002), PopSize = rep(900176, 40002), Duration = rep(79.96, 40002),Income = rep(71134, 40002), Pandemic = c(rep(1, 20001), rep(0, 20001))) 

#making predictions
preds1<-cbind(new.data.1, predict(mod3, new.data.1, type = "link", se.fit = TRUE))

preds1 <- within(preds1, {
  Length <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

#plotting predictions

ggplot(preds1) +
  geom_ribbon(aes(x = Structures, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .25) +
  geom_line(aes(x = Structures, y = Length, colour = as.factor(Pandemic)), size = 1) +
  labs(x = "Number of Structures Burned", y = "Length of Interest per day of Fire Duration")
