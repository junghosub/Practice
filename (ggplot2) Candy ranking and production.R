# library
library(tidyverse)
library(ggthemes)

# loading datasets
candyRankings <- read.csv("candy-data.csv")
candyProduction <- read_csv("candy_production.csv")

# checking datasets
glimpse(candyRankings)
glimpse(candyProduction)

# Making Scatter plot
ggplot(data = candyRankings, aes(x = sugarpercent, y = pricepercent, label = competitorname))+
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_text(check_overlap = T,
            vjust = "bottom",
            nudge_y = 0.01,
            angle = 30,
            size = 2) +
  labs(title = "More sugary candies are more expensive",
       x = "Sugar content(percentile)",
       y = "price(percentile")

# text could be a point!

candy_price<-ggplot(data = candyRankings, aes(x=sugarpercent, 
                                 y = pricepercent, 
                                 label = competitorname)) +
  geom_smooth(method = 'lm') +
  geom_text(check_overlap = T,
            size = 3,
            angle = 30) +
  labs(title = "More sugary candies are more expesive",
       x = "Sugar content (percentile)",
       y = "Price(percentile)")

# save
ggsave("More Sugary candies are more expensive.png",
       plot= candy_price,
       height = 6,
       width = 10,
       units = "in")

# how to visualize categorical data
# Bar chart

candyFeatures <- candyRankings %>% select(2:10)

# convert to logical
candyFeatures[] <- lapply(candyFeatures, as.logical)

# visualize
ggplot(data = candyFeatures, aes(x = chocolate)) +
  geom_bar()

# more informative bar. to use 'caramel'
ggplot(data = candyFeatures, aes(x = chocolate,
                                 fill = caramel)) +
  geom_bar()

ggplot(data=candyFeatures, aes(x=chocolate,
                               fill = caramel)) +
  geom_bar(position = "dodge") +
  facet_wrap(c("caramel")) +
  scale_fill_manual(values = c("#BBBBBB",
                               "#E69F00"))

# plus label and theme
chocolate_caramel<-ggplot(data = candyFeatures, aes(x = chocolate,
                                 fill = caramel))+
  geom_bar(position = "dodge", size = 2)+
  facet_wrap(c("caramel"))+
  scale_fill_manual(values = c("#BBBBBB",
                               "#E69F00")) +
  labs(title = "Chocolate candies are more likey have caramel",
       x = "is the candy chocolate?",
       y = "count of candies") +
  theme(legend.position = c(0.9,0.9),
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggsave("chocolate_caramel.png",
       plot = chocolate_caramel,
       height = 6, width = 10, units = "in")

# Making Line Chart
lineplot <-ggplot(data=candyProduction, aes(x=observation_date, y=IPG3113N))+
  geom_line() +
  geom_smooth()+
  labs(title = "Monthly candy production (US)",
       x = "Date",
       y = "As percent of 2012 Production")

lineplot + theme_economist()

