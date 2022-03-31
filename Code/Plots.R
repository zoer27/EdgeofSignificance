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

CalFireGen %>% 
  filter(Month<2022)%>% 
  ggplot(aes(x= as.numeric(Month), y=CAFire))+
  geom_line()+
  dark_theme_classic()+
  labs(y= "California Fire Interest (CA)", x="Year")

USFireGen %>% 
  mutate(Month=as.numeric(str_extract(Month, "^\\d+"))) %>%
  group_by(Month) %>%
  summarize(USFire=sum(USFire)) ->
  USFireGen

USFireGen %>% 
  filter(Month<2022)%>% 
  ggplot(aes(x= as.numeric(Month), y=USFire))+
  geom_line()+
  dark_theme_classic()+
  labs(y= "California Fire Interest (US)", x="Year")

#Cool now I should move on to plotting specifics from the modeling

#Run the models file to get data loaded

#Plotting Attention and Structures destroyed
fires %>% 
  ggplot(aes(x = Structures, y = Length))+
  geom_point()+
  scale_x_log10()

fires %>% 
  ggplot(aes(x = Structures, y = Peak))+
  geom_point()+
  scale_x_log10()

fires %>% 
  ggplot(aes(x = Structures, y = PeakCA))+
  geom_point()+
  scale_x_log10()

#Plotting Attention and Acres burned
fires %>% 
  ggplot(aes(x = Hectares, y = Length))+
  geom_point()

#Plotting Attention and Income
fires %>% 
  ggplot(aes(x = Income, y = Length))+
  geom_point()

#Plotting Attention and Population
fires %>% 
  ggplot(aes(x = PopSize, y = Length))+
  geom_point()+
  scale_x_log10()
