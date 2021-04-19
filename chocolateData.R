# library
library(tidyverse)

# data loading
chocolateData<-read.csv("flavors_of_cacao.csv")
names(chocolateData) <- make.names(names(chocolateData), unique(TRUE))
names(chocolateData)
head(chocolateData)
tail(chocolateData)
names(chocolateData) <- gsub("[[:space:]+]", "", names(chocolateData))

str(chocolateData)
chocolateData<-type_convert(chocolateData)
str(chocolateData)

# char data of cocoa percent -> numeric
chocolateData$Cocoa.Percent<-sapply(chocolateData$Cocoa.Percent, function(x)gsub("%","",x))
chocolateData<-type_convert(chocolateData)
str(chocolateData)

# data cleaning

summary(chocolateData)
summarise_all(chocolateData,funs(mean))

chocolateData %>%
  summarise(averageRaiting = mean(Rating),
            sdRating = sd(Rating))
# group_by

chocolateData %>%
  group_by(Review.Date) %>%
  summarise(avaerageRating = mean(Rating),
            sdRating = sd(Rating))
# Visualization
chocolateRatingByReviewDate<-ggplot(chocolateData, aes(x=Review.Date, y = Rating, color=Cocoa.Percent))+
  geom_point()+
  geom_jitter()
  geom_smooth(method='lm')
  
  
# plot save
ggsave("chocolateRatingByReviewDate.png",
       plot= chocolateRatingByReviewDate,
       height = 6, width = 10, units = "in")
