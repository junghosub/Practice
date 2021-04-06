# Library
library(tidyverse)
library(corrplot)
library(randomForest)
library(ggthemes)
library(scales)
library(plyr)
library(caret)

# Roading Data

train <- read_csv("train.csv")
test <- read_csv("test.csv")

full <- bind_rows(train,test)

options(warn = -1)  # ingnore Warning

# Check Data

str(full)
summary(full)
glimpse(full)

# Exploratory Analysis

sort(colSums(is.na(full)), decreasing = T)

# Visualization
  ggplot(data = full[1:891,], aes(x = Age, fill = factor(Survived))) +
  geom_histogram(bins = 30, position = "dodge", na.rm=T) +
  labs(title = "Age vs Survived") +
  scale_fill_discrete(name = element_text("Survived")) +
  theme(legend.position = c(0.9,0.9)) # change label position
  
ggplot(data= full[1:819,], aes(x = Sex, fill = factor(Survived))) +
    geom_bar(stat = "count",
             position = "dodge") +
    labs(title = "Sex vs Survied") +
    scale_fill_discrete(name = element_text("Survived")) +
    theme(legend.position = c(0.9,0.9))

ggplot(data =full[1:819,], aes(x = Age, fill = factor(Survived))) +
  geom_histogram(bins = 30) +
  facet_grid(.~Sex) +
  labs(title = "Sex vs Age vs Survived") +
  scale_fill_discrete(name = element_text("Survived")) + # Change Legend Name
  theme(legend.position = c(0.9,0.9))

ggplot(data = full[1:819,], aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat = "count") +
  facet_grid(~Sex) +
  labs(title = "PClass vs Sex vs Survived") +
  scale_fill_discrete(name = element_text("Survived"))

ggplot(data = full[1:891,], aes(x = Fare)) +
  geom_histogram(fill = "sky blue") +
  geom_vline(aes(xintercept = median(Fare)), linetype = "dashed") +
  geom_text(aes(x = median(Fare), y = 0, label = median(Fare), vjust = 2.5)) +
  labs(title = "Distibution of Fare")
  
# family
full$Family <- full$SibSp + full$Parch + 1

ggplot(data = full[1:891,], aes(x = Family, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Family Size vs Survived") +
  scale_fill_discrete(name = element_text("Survived")) +
  scale_x_continuous(breaks = seq(1,11,1)) +
  theme(legend.position = c(0.9,0.9))
    
full$FamD[full$Family == 1] <- "Alone"
full$FamD[full$Family < 5 & full$Family > 1] <- "Small"
full$FamD[full$Family >= 4] <- "Big"

mosaicplot(table(full$FamD, full$Survived), main = "Mosaic Plot of Family Size", xlab = "Family size", ylab = "Survived", col = hcl(c(50,120)),)

# mosaicplot(~full$FamD + full$Survived, main = "Mosaice Plot of Familly Size", xlab = "Family Size", ylab = "Survived", col = hcl(c(50,200)),)         

# missing value
full %>%
  filter(is.na(Age) & Sex == 'male')

full[1:891,] %>%
  filter(!is.na(Age) & Sex == "male") %>%
  ggplot(aes(x = Age, fill = factor(Pclass))) +
  stat_density(position = "identity", alpha = 0.4, na.rm =TRUE) +
  geom_vline(aes(xintercept=median(Age[Pclass == 3],na.rm = T)), color = 'Dark Blue', linetype = 'dashed') +
  geom_vline(aes(xintercept=median(Age[Pclass == 2],na.rm = T)), color = 'Dark Green', linetype = 'dashed') +
  geom_vline(aes(xintercept=median(Age[Pclass == 1],na.rm = T)), color = 'Tomato', linetype = 'dashed') +
  theme(legend.position = c(0.9,0.9),
        legend.title  = element_blank()) +
  labs(title = "Distribution of male Age By Pclass")


for (i in 1:3){
  full[is.na(full$Age) & full$Pclass == i & full$Sex == 'male',]$Age <- median(full[full$Sex == 'male' & full$Pclass == i,]$Age, na.rm = TRUE)
}

full[1:891,] %>%
  filter(!is.na(Age) & Sex == "female") %>%
  ggplot(aes(x = Age, fill = factor(Pclass))) +
  stat_density(position = "identity", alpha = 0.4) +
  geom_vline(aes(xintercept=median(Age[Pclass == 3],na.rm = T)), color = 'Dark Blue', linetype = 'dashed') +
  geom_vline(aes(xintercept=median(Age[Pclass == 2],na.rm = T)), color = 'Dark Green', linetype = 'dashed') +
  geom_vline(aes(xintercept=median(Age[Pclass == 1],na.rm = T)), color = 'Tomato', linetype = 'dashed') +
  theme(legend.position = c(0.9,0.9),
        legend.title  = element_blank()) +
  labs(title = "Distribution of Female Age By Pclass")

for (i in 1:3){
  full[is.na(full$Age) & full$Pclass == i & full$Sex == 'female',]$Age <- median(full[full$Sex == 'female' & full$Pclass == i,]$Age, na.rm = TRUE)
}

sum(is.na(full$Age))

# Embarked

full %>%
  filter(is.na(Embarked))

# check
table(full$Embarked) # S is the most value

full$Embarked[c(62,830)] <- "S"

ggplot(data = full[1:891,], aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(.~Embarked) +
  labs(title = "PClass vs Embarked vs Survived") +
  scale_fill_discrete(name = element_text("Survived"))

# Fare
full %>%
  filter(is.na(Fare))

ggplot(data = full[1:891,], aes(x = Fare, fill = factor(Pclass))) +
  stat_density(position = "identity", alpha = 0.75) +
  labs(title = "Distribution Fare by Pclass") +
  theme(legend.position = c(0.9,0.9),
        legend.title = element_blank())

ggplot(data = full[full$Pclass == 3,], aes(x = Fare)) +
  geom_density(alpha = 0.5, fill = "sky blue") +
  geom_vline(aes(xintercept = median(Fare, na.rm = TRUE)), linetype = "dashed", lwd = 1, colour = "Royal Blue") +
  geom_text(aes(x = median(Fare), y = 0, label = median(Fare), vjust = 2.5)) +
  labs(title = "Density Pclass = 3",
       x = "Fare",
       y = "Density")

full$Fare[1044] <- median(full$Fare[full$Pclass == 3], na.rm =TRUE)

# Child
full$Child[full$Age < 18] <- "Child"
full$Child[full$Age >= 18] <- "Adult"

ggplot(data = full[1:891,], aes(x = Age, fill = factor(Survived))) +
  geom_histogram() +
  facet_wrap(~Child) +
  labs(title = "Child vs Survived") +
  scale_fill_discrete(name = element_text("Survived")) +
  theme(legend.position = c(0.9,0.9))
  
df <- full[1:891,]

ggplot(data = df[df$Child == "Child",], aes(x = Sex, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass) +
  labs(title = "Pclass vs Sex vs Child vs Survived") +
  scale_x_discrete(name = element_text("Survived"))

# name

title <- full$Name
title <- gsub("^.*, (.*?)\\..*$", "\\1", title)

full$title <- title
unique(full$title)
sort(table(full$title), decreasing = T)

full[title %in% c("Capt","Col","Major","Dr","Rev","Don","Sir","the Countess","Jonkheer"),]$title <- "officer"
full[title %in% c("Mlle", "Ms", "Lady","Dona"),]$title <- "Miss"
full[title == "Mme",]$title <- "Mrs"  

full$title <- as.factor(full$title)

ggplot(data = full[1:891,], aes(x = title, fill = factor(Survived))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(name = element_text("Survived")) +
  theme(legend.position = c(0.9,0.9))

# Correlation Heat Map

corr.data <- full[1:891,]

corr.data$Embarked <- revalue(corr.data$Embarked, c("S" = 1, "Q" = 2, "C" = 3))
corr.data$Sex <- revalue(corr.data$Sex, c("male" =1, "female" = 2))
corr.data$FamD <- revalue(corr.data$FamD, c("Alone" = 1, "Small" = 2, "Big" = 3))
corr.data$Child <- revalue(corr.data$Child, c("Child" = 1, "Adult" = 2))
corr.data$title <- revalue(corr.data$title, c("Miss" = 1, "Mrs" = 2, "Master" = 3, "officer" = 4, "Mr" = 5))

# Convert to Numeric

corr.data$Child <- as.numeric(corr.data$Child)
corr.data$Sex <- as.numeric(corr.data$Sex)
corr.data$Embarked <- as.numeric(corr.data$Embarked)
corr.data$FamD <- as.numeric(corr.data$FamD)
corr.data$Pclass <- as.numeric(corr.data$Pclass)
corr.data$Survived <- as.numeric(corr.data$Survived)
corr.data$title <- as.numeric(corr.data$title)

corr.data <- corr.data[,c("Child", "Sex", "Embarked", "FamD","Pclass", "Fare", "Age", "title", "Survived")]
str(corr.data)

corr.data<-cor(corr.data)
sort(corr.data, decreasing = TRUE)

# Correlation Plot
corrplot.mixed(corr.data, tl.pos = "lt", tl.col = 'black')

# Convert to Factor
full$Child  <- factor(full$Child)
full$Sex  <- factor(full$Sex)
full$Embarked  <- factor(full$Embarked)
full$Pclass  <- factor(full$Pclass)
full$FamD  <- factor(full$FamD)
full$title <- factor(full$title)

full1 <- full[,-9]
full_mod <- full1[,-10]

train <- full_mod[1:891,]
test <- full_mod[892:1309,]

# Modeling

set.seed(1234)
model <- randomForest(factor(Survived) ~ Child + Sex + Embarked + FamD + Pclass + Fare + title, data =train, importance = T, ntree = 500)
fitted <- predict(model)

print(model)
importance(model)
varImpPlot(model)

prediction <- predict(model, test)

solution <- data.frame(Survived = prediction, PassengerID = test$PassengerId)

write.csv(solution, file = "model.soution.csv", row.names = F)
