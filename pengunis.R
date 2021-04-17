# library
library(tidyverse)

# loading data
penguins<-read.csv("penguins_size.csv")

# exploring datasets
glimpse(penguins)
head(penguins)
tail(penguins)
summary(penguins)
names(penguins)

penguins %>%
  filter(island == "Torgersen")

penguins %>%
  arrange(culmen_length_mm)

penguins %>%
  arrange(species) %>%
  head()

# creating a subset, funtion of dplyr

set.seed(406)

penguins_subset<- penguins %>%
  sample_n(12)

# arrange
penguins_subset %>%
  arrange(species)

# filter
penguins_subset %>%
  filter(culmen_depth_mm > 18.5)

penguins_subset %>%
  filter(between(culmen_depth_mm,16.2,18.2))

# select
penguins_subset %>%
  select(species,island, body_mass_g)

penguins_subset %>%
  select(where(is.character))

# muatate

penguins_subset %>%
  mutate(body_mass_weight_pound = body_mass_g / 453.59237) %>%
  select(species, body_mass_g, body_mass_weight_pound, everything())

# summarise

penguins_subset %>%
  summarise(mean_body_mass = mean(body_mass_g))

penguins %>%
  summarise(avg_body_weight = mean(body_mass_g, na.rm= TRUE))

penguins %>%
  group_by(species) %>%
  summarise(avg_body_weight = mean(body_mass_g, na.rm= TRUE))

penguins %>%
  group_by(species, island) %>%
  summarise(avg_body_weight = mean(body_mass_g, na.rm=TRUE))
