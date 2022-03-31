##Research Derby 3/30/2028
###Questions: How has google search interest in wildfires change over time? 
#How does the population size and wealth index affect national interest in wildfires?

#model ideas:
#GLM with length of interest (days) with covariates including population size and wealth

library(tidyverse)
library(MASS)
library(faraway)
library(aod)

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

#deivance explained for best model
percdev1 <- (mod3$null.deviance - mod3$deviance) / mod3$null.deviance
percdev1 #30%

#GLM for ratio of peak interest--the offset term is inherent
summary(fires$Peak)
modPeak1<-glm(Peak ~ Duration + Structures + Hectares + PopSize + Income + Pandemic, family = "poisson", data = fires)
summary(modPeak1)

#testing for overdispersion
modPeak2<-glm(Peak ~ Duration + Structures + Hectares + PopSize + Income + Pandemic, family = "quasipoisson", data = fires)
summary(modPeak2) #overdispersion parameter is over 2 so switching to negative binomial

#negative binomial
modPeak3<-glm.nb(Peak ~ Duration + Structures + Hectares + PopSize + Income + Pandemic, data = fires)
summary(modPeak3)

#percent deviance explained
precdev2 <- (modPeak3$null.deviance - modPeak3$deviance) / modPeak3$null.deviance
precdev2 #17%

#testing model fit
halfnorm(hatvalues(modPeak3))

#model estimates
tab2 <- data.frame(coefficients(modPeak3),confint(modPeak3,type = "profile")[,1],confint(modPeak3,type = "profile")[,2])
colnames(tab2) <- c("Coefficients","Lower 95%","Upper 95%")
tab2

#Poisson predictions don't make much sense--trying binomial
PeakModBin<-glm(cbind(Peak, rep(100,nrow(fires))) ~ Duration + Structures + Hectares + PopSize + Income + as.factor(Pandemic), family = "binomial", data = fires)
summary(PeakModBin)

#seeing if betabinomial is a better fit
PeakModBetaBin<-betabin(cbind(Peak, rep(100,nrow(fires))) ~ Duration + Structures + Hectares + PopSize + Income + as.factor(Pandemic), ~ 1,data = fires)
summary(PeakModBetaBin)

# Models for California ---------------------------------------------------
#Peak interest in California
Peakmod1CA<-glm(PeakCA ~ Structures + Hectares + PopSize + Income + Pandemic + Duration, family = "poisson", data = fires)
summary(Peakmod1CA)

#testing for overdispersion
Peakmod2CA<-glm(PeakCA ~ Structures + Hectares + PopSize + Income + Pandemic + Duration, family = "quasipoisson", data = fires)
summary(Peakmod2CA) #overdispersed so switching to 

#negative binomial
Peakmod3CA<-glm(PeakCA ~ Structures + Hectares + PopSize + Income + Pandemic + Duration, data = fires)
summary(Peakmod3CA)


#beta binomial for peak interest in california
PeakModBetaBinCA<-betabin(cbind(PeakCA, rep(100,nrow(fires))) ~ Duration + Structures + Hectares + PopSize + Income + as.factor(Pandemic), ~ 1,data = fires)
summary(PeakModBetaBinCA)


#including california as a factor--is there a difference between national and california? 
fires_peakCA<-fires$PeakCA
fires_peakNA<-fires$Peak
fires2<-fires %>% dplyr::select(-Peak, -PeakCA) %>%
  bind_rows(.,.) %>%
  add_column(PeakALL = c(fires_peakNA, fires_peakCA), Region = c(rep("National", 25), rep("California", 25)))

PeakRegionBetaBin<-betabin(cbind(PeakALL, rep(100,nrow(fires2))) ~ Duration + Structures + Hectares + PopSize + Income + as.factor(Pandemic) + Region, ~ 1,data = fires2)
summary(PeakRegionBetaBin)

#plots comparing California and National peak
peakscomp<-tibble(year = rep(fires$Year, 2), name = rep(fires$Name, 2), Peak = c(fires$Peak, fires$PeakCA), Category = c(rep("National", nrow(fires)), rep("California", nrow(fires))))
peakscomp<-peakscomp %>% arrange(year)
ggplot() + geom_bar(data = peakscomp, aes(x = name, y = Peak, fill = Category), stat = "identity", position = "dodge")

# Figures -----------------------------------------------------------------


#for figures want to use predictions and confidence intervals--easier to interpret than estimates

#create new data fixing the rest of the parameters (usually good to fix at their mean), and then predict on this data, 
#I think it would be nice to do this for both pandemic = 1 and pandemic = 0 so you can plot on same plot

#data for prediction across the range of Structures and at mean values of everything else for pandemic and not pandemic years
new.data.1<-data.frame(Structures = rep(seq(0, 20000, by = 10), 2), 
                       Hectares = rep(108714,4002), 
                       PopSize = rep(900176, 4002), 
                       Duration = rep(79.96, 4002),
                       Income = rep(71134, 4002), 
                       Pandemic = c(rep(1, 2001), rep(0, 2001))) 

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
  geom_point(data = fires, aes(x = Structures, y = Length, color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  labs(x = "Number of Structures Burned", y = "Length of Interest on Google")+
  dark_theme_classic()


summary(fires$Length)
summary(fires$Duration)


#preds for Hectares
new.data.2<-data.frame(Structures = rep(1878, 40602), 
                       Hectares = rep(seq(14000, 420000, by = 20), 2), 
                       PopSize = rep(900176, 40602), 
                       Duration = rep(79.96, 40602),
                       Income = rep(71134, 40602), 
                       Pandemic = c(rep(1, 20301), rep(0, 20301))) 

#making predictions
preds2<-cbind(new.data.2, predict(mod3, new.data.2, type = "link", se.fit = TRUE))

preds2 <- within(preds2, {
  Length <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})


ggplot(preds2) +
  geom_ribbon(aes(x = Hectares, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .25) +
  geom_line(aes(x = Hectares, y = Length, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Hectares, y = Length, color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  labs(x = "Hectares", y = "Length of Interest on Google")
 

#preds for Income
new.data.3<-data.frame(Structures = rep(1878, 17802), 
                       Hectares = rep(108714, 17802), 
                       PopSize = rep(900176, 17802), 
                       Duration = rep(79.96, 17802),
                       Income = rep(seq(40000, 129000, by = 10), 2), 
                       Pandemic = c(rep(1, 8901), rep(0, 8901))) 

#making predictions
preds3<-cbind(new.data.3, predict(mod3, new.data.3, type = "link", se.fit = TRUE))

preds3 <- within(preds3, {
  Length <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})


ggplot(preds3) +
  geom_ribbon(aes(x = Income, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .25) +
  geom_line(aes(x = Income, y = Length, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Income, y = Length, color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  labs(x = "Income", y = "Length of Interest on Google")




#preds for peak National--with structures varying
preds4<-cbind(new.data.1, predict(PeakModBetaBin, new.data.1, type = "link", se.fit = TRUE))

preds4 <- within(preds4, {
  Peak <- plogis(fit)
  LL <- plogis(fit - 1.96 * se.fit)
  UL <- plogis(fit + 1.96 * se.fit)
})


ggplot(preds4) +
  geom_ribbon(aes(x = Structures, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .25) +
  geom_line(aes(x = Structures, y = Peak, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Structures, y = (Peak/100), color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  labs(x = "Number of Structures Burned", y = "Relative peak of interest on google")



#Peak preds with income
preds5<-cbind(new.data.3, predict(PeakModBetaBin, new.data.3, type = "link", se.fit = TRUE))

preds5 <- within(preds5, {
  Peak <- plogis(fit)
  LL <- plogis(fit - 1.96 * se.fit)
  UL <- plogis(fit + 1.96 * se.fit)
})


ggplot(preds5) +
  geom_ribbon(aes(x = Income, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .25) +
  geom_line(aes(x = Income, y = Peak, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Income, y = (Peak/100), color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  labs(x = "Number of Structures Burned", y = "Relative peak of interest on google")


#peak and Hectares
preds6<-cbind(new.data.2, predict(PeakModBetaBin, new.data.2, type = "link", se.fit = TRUE))

preds6 <- within(preds6, {
  Peak <- plogis(fit)
  LL <- plogis(fit - 1.96 * se.fit)
  UL <- plogis(fit + 1.96 * se.fit)
})
