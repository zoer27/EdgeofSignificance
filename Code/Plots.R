library(tidyverse)
library(ggdark)
#Start with some general plots
USFireGen <- read_csv("Data/USFireGen.csv")
CalFireGen <- read_csv("Data/CalFireGen.csv")
CalFireGen
CalFireGen %>% 
  mutate(Month=as.numeric(str_extract(Month, "^\\d+"))) %>%
  group_by(Month) %>%
  summarize(CAFire=sum(CAFire)) ->
  CalFireGen

ggsave("CAFireGen.jpg", plot = last_plot(), path= "Figures", device="jpeg", type="cairo")

CalFireGen %>% 
  filter(Month<2022)%>% 
  ggplot(aes(x= as.numeric(Month), y=CAFire))+
  geom_line()+
  geom_point()+
  dark_theme_classic()+
  theme(text = element_text(size = 20))+
  labs(y= "California Fire Interest (CA)", x="Year")

ggsave("CAFireGen.jpg", plot = last_plot(), path= "Figures", device="jpeg", type="cairo")

USFireGen %>% 
  mutate(Month=as.numeric(str_extract(Month, "^\\d+"))) %>%
  group_by(Month) %>%
  summarize(USFire=sum(USFire)) ->
  USFireGen

USFireGen %>% 
  filter(Month<2022)%>% 
  ggplot(aes(x= as.numeric(Month), y=USFire))+
  geom_line()+
  geom_point()+
  dark_theme_classic()+
  theme(text = element_text(size = 20))+
  labs(y= "California Fire Interest (National)", x="Year")

ggsave("USFireGen.jpg", plot = last_plot(), path= "Figures", device="jpeg", type="cairo")

#Cool now I should move on to plotting specifics from the modeling

#Run the models file to get data loaded

#Plotting Attention and Structures destroyed
fires %>% 
  ggplot(aes(x = Structures, y = Length))+
  geom_point()+
  dark_theme_classic()+
  theme(text = element_text(size = 20))+
  scale_x_log10()+
  labs(x="Structures Destroyed", y="Duration of National Interest (days)")

ggplot(preds1) +
  geom_ribbon(aes(x = Structures, ymin = LL, ymax = UL, alpha = .25)) +
  geom_line(aes(x = Structures, y = Length), size = 1) + 
  dark_theme_classic()+
    geom_point(data = fires, aes(x = Structures, y = Length)) + 
  labs(x = "Number of Structures Burned", y = "Length of Interest on Google")

fires %>% 
  ggplot(aes(x = Structures, y = Peak))+
  dark_theme_classic()+
  geom_point(size=2.5)+
  labs(x="Number of Structures Burned", y="Peak of National Interest")+
  theme(text = element_text(size = 20))+
  scale_x_log10()

ggsave("PeakNStructures.jpg", plot = last_plot(), path= "Figures", device="jpeg", type="cairo")

fires %>% 
  ggplot(aes(x = Structures, y = PeakCA))+
  geom_point(size=2.5)+
  dark_theme_classic()+
  labs(x=element_blank(), y="Peak of CA Interest")+
  theme(text = element_text(size = 20))+
  scale_x_log10()

ggsave("CAStructures.jpg", plot = last_plot(), path= "Figures", device="jpeg", type="cairo")

#Plotting Attention and Acres burned
fires %>% 
  ggplot(aes(x = Hectares, y = Length))+
  geom_point()

fires %>% 
  ggplot(aes(x = Hectares, y = Peak))+
  geom_point()

fires %>% 
  ggplot(aes(x = Hectares, y = PeakCA))+
  geom_point()

#Plotting Attention and Income
fires %>% 
  ggplot(aes(x = Income, y = Length))+
  geom_point()

fires %>% 
  ggplot(aes(x = Income, y = Peak))+
  geom_point()

fires %>% 
  ggplot(aes(x = Income, y = PeakCA))+
  geom_point()

#Plotting Attention and Population
fires %>% 
  ggplot(aes(x = PopSize, y = Length))+
  geom_point()+
  scale_x_log10()

fires %>% 
  ggplot(aes(x = PopSize, y = Peak))+
  geom_point()+
  scale_x_log10()

fires %>% 
  ggplot(aes(x = PopSize, y = PeakCA))+
  geom_point()+
  scale_x_log10()


#Length v Structures
ggplot(preds1) +
  geom_ribbon(aes(x = Structures, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .25) +
  geom_line(aes(x = Structures, y = Length, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Structures, y = Length, color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  labs(x = "Number of Structures Burned", y = "Length of National Interest (days)")+
  dark_theme_classic()+
  theme(legend.position = "none",
        text = element_text(size = 20))

ggsave("PanvsNonPan.jpg", plot = last_plot(), path= "Figures", device="jpeg", type="cairo")

#Length v Hectares
ggplot(preds2) +
  geom_ribbon(aes(x = Hectares, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .25) +
  geom_line(aes(x = Hectares, y = Length, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Hectares, y = Length, color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  dark_theme_classic()+
  labs(x = "Hectares Burned", y = "Length of National Interest (days)")+
  theme(legend.position = "none",
      text = element_text(size = 20))

ggsave("LengthvHectares.jpg", plot = last_plot(), path= "Figures", device="jpeg", type="cairo")

#Length v income
ggplot(preds3) +
  geom_ribbon(aes(x = Income, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .25) +
  geom_line(aes(x = Income, y = Length, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Income, y = Length, color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  dark_theme_classic()+
  labs(x = "Median Houshold County Income", y = "Length of National Interest (days)")+
  theme(legend.position = "none",
        text = element_text(size = 20))

ggsave("LengthvIncome.jpg", plot = last_plot(), path= "Figures", device="jpeg", type="cairo")


#peak plots
#peak and structures
ggplot(preds4) +
  geom_ribbon(aes(x = Structures, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .25) +
  geom_line(aes(x = Structures, y = Peak, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Structures, y = (Peak/100), color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  dark_theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 18)) +
  labs(x = "Number of Structures Burned", y = "Relative peak of interest on Google")

ggsave("PeakStructures.jpg", plot = last_plot(), path= "Figures", device="jpeg")

#peak and income
ggplot(preds5) +
  geom_ribbon(aes(x = Income, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .3) +
  geom_line(aes(x = Income, y = Peak, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Income, y = (Peak/100), color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  labs(x = "Median Household Income", y = "Relative peak of interest on Google") + dark_theme_classic()+
  theme(legend.position = "none",
        text = element_text(size = 18))

ggsave("PeakIncome.jpg", plot = last_plot(), path= "Figures", device="jpeg")

#peak and Hectares
ggplot(preds6) +
  geom_ribbon(aes(x = Hectares, ymin = LL, ymax = UL, fill = as.factor(Pandemic)), alpha = .3) +
  geom_line(aes(x = Hectares, y = Peak, colour = as.factor(Pandemic)), size = 1) + 
  geom_point(data = fires, aes(x = Hectares, y = (Peak/100), color = as.factor(Pandemic))) + 
  scale_fill_manual(values = c("#f7920b", "#b62304"))+
  scale_color_manual(values = c("#f7920b", "#b62304"))+
  labs(x = "Hectares Burned", y = "Relative peak of interest on Google") + dark_theme_classic()+
  theme(legend.position = "none",
        text = element_text(size = 18))

ggsave("PeakHectares.jpg", plot = last_plot(), path= "Figures", device="jpeg")

