
library(vegan)
library(tidyverse)

dat <- read.csv("Data/WildFireData.csv", nrows = 25) %>%
  column_to_rownames("Name") %>%
  mutate(Income=str_replace(Median.Household.Income, ",", "")) %>%
  select(Structures, Hectares, Pop.Size, Income, Duration) %>%
  mutate(across(everything(), as.numeric))

pcavals <- dat %>%
  sweep(2, sapply(dat, max), `/`) %>%
  princomp()
biplot(pcavals)

mds <- dat %>%
  sweep(2, sapply(dat, max), `/`) %>%
  metaMDS()
as.data.frame(mds$points) %>%
  rownames_to_column("rn") %>%
  ggplot(mapping = aes(x=MDS1, y=MDS2, label=rn)) +
  geom_text() +
  geom_text(data = as.data.frame(mds$species) %>% rownames_to_column("rn"), color="red") +
  geom_segment(data = as.data.frame(mds$species) %>% rownames_to_column("rn"), color="red",
               xend=0, yend=0, arrow = arrow(ends = "first")) +
  theme_minimal()
