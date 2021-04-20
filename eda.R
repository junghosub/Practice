library(tidyverse)
library(readxl)
library(MASS)
library(lmtest)
library(arm)

# Loading Data

titanic <- read.csv('Titanic train.csv', header = T)
vegetable <- read_excel('1년간 채소가격현황.xlsx', na)

veg <- vegetable %>% column_to_rownames('...1')
veg
health <- read.csv('건강검진표(성별-흡연상태-콜레스테롤-비만도).csv')
View(health)
str(vegetabel)

view(titanic)
view(vegetabel)
view(health)

# 줄기-그림
stem(titanic$Fare, scale = .5)

# 히스토그램
hist(titanic$Age, col = 'skyblue')
abline(v = mean(titanic$Age), col = 'red', lwd = 2)

# 분위수 구하기
quantile(titanic$Age)

# 박스플롯
boxplot(titanic$Age~titanic$Pclass, main = 'Boxplot of Age by PClass', ylab = 'Age', xlab = 'Pclass')
boxplot(titanic$Age~titanic$Survived) # width, reorder
abline(h = mean(titanic$Age), col = 'red')

# 자료의 재표현
hist(titanic$Fare)
hist(log(titanic$Fare))

# 자료의 재표현 - text
data("Animals")

plot(log(Animals$brain)~log(Animals$body))
text(y = log(Animals$brain), x = log(Animals$body), 
     labels = abbreviate(rownames(Animals)),
     adj = 0, cex = 0)

# 회귀분석
par(mfrow = c(1,2))
lm(log(Animals$brain)~log(Animals$body))
m0 <- lm(log(Animals$brain)~log(Animals$body))
plot(log(Animals$brain)~log(Animals$body))
abline(m0$coefficients, lty = 'dotted')

# 로버스트 회귀분석
rlm(log(Animals$brain)~log(Animals$body))
m1 <- rlm(log(Animals$brain)~log(Animals$body))
plot(log(Animals$brain)~log(Animals$body))
abline(m1$coefficients, lty = 'dotted')

# 그림에 텍스트 삽입

# 확률분포의 활용
qqnorm(titanic$Age)

# 도수분포표
addmargins(table(titanic$Survived))


# 이원자료 빈도표 탐색
vegetabel

summary(vegetabel)
View(vegetabel)

world <- read.table('WorldTemperature_Mean.txt', header = T)
mp <- medpolish(world)
mp

# medpolish residual plot
plot(matrix(mp$residuals))
abline(h = 0)

comparison <- matrix(mp$row, ncol = 1) %*% matrix(mp$col, nrow = 1) / mp$overall
plot(x = comparison, y = mp$residuals)
boxplot(mp$row~ mp$col, main = 'Boxplot', ylab = 'residual', xlab = 'Month')


veg
mp <- medpolish(veg)
mp
View(world)

comparison <- matrix(mp$row, ncol = 1) %*% matrix(mp$col, nrow = 1) / mp$overall
comparison

twoway.model <- medpolish(world)
attach(twoway.model)
comparison <- matrix(row, ncol = 1)%*%matrix(col, nrow = 1)/overall
comparison

par(mfrow = c(1,2))
plot(mp$residuals~comparison, xlim = c(-10000,10000), ylim = c(-10000,10000))
boxplot(matrix(mp$residuals))




data("UCBAdmissions")
View(UCBAdmissions)
UCBAdmissions

dim(UCBAdmissions)

Tab1 <- UCBAdmissions[1,,]
Tab1

Tab2 <- UCBAdmissions[2,,]
Tab2

Tab <- Tab1 + Tab2
Tab

addmargins(Tab)

barplot(Tab1, cex.main = 1.5, cex.names = 1.5, cex.axis = 1.5)
barplot(Tab1, cex.main = 1.5, cex.names = 1.5, cex.axis = 1.5, col = c('wheat', 'grey'),
        main = 'Accept')

barplot(Tab1, cex.main = 1.5, cex.names = 1.5, cex.axis = 1.5)

par(mfrow = c(1,1))
barplot(Tab, 
        legend = rownames(Tab),
        cex.main = 1.5, 
        cex.names = 1.5,
        cex.axis = 1.5, beside = FALSE)

Tab
ggplot(aes(x = Tab$Dept, y = Tab$Gender))+
  geom_bar(stat = 'identity', position = 'fill')

# 기존
Tab.col <- apply(Tab, 2, sum)
Tab.c <- Tab%*%diag(1/Tab.col) * 100
rownames(Tab.c) <- c('A', 'B', 'C', 'D', 'E', 'F')
colnames(Tab.c) <- c('Male', 'Female')
barplot(Tab.c, legend = rownames(Tab.c))

par(mfrow = c(1,1))
Tab.col2 <- apply(Tab, 1, sum)
Tab.c2 <- t(Tab) %*% diag(1/Tab.col2) * 100 
colnames(Tab.c2) <- c('Male', 'Female')
barplot(Tab.c2, legend = colnames(Tab.b))

df.tab <- data.frame(Tab)
ggplot(data = df.tab, aes(x = Gender, y = Freq, fill = factor(Dept))) +
  geom_bar(stat ='identity', position = 'fill', col = 'white') +
  theme_classic() +
  scale_fill_discrete(name = 'Dept')

ggplot(data = df.tab, aes(x = Gender, y = Freq, fill = factor(Dept))) +
  geom_bar(stat = 'identity', position = 'fill') +
  theme_classic() +
  scale_fill_discrete(name = 'Dept')
par(mfrow = c(1,2))
barplot(t(Tab), legend = colnames(Tab))
barplot(t(Tab), beside = TRUE, legend = colnames(Tab), col = c('skyblue', 'red', 'Pink'))

mosaicplot(~Dept + Gender, 
           data = UCBAdmissions, 
           color = TRUE, 
           cex.axis = 1.2, 
           main = '',
           xlab = '',
           ylab = '')
mosaicplot(~Gender + Dept, data = UCBAdmissions, color = TRUE,
           cex.axis = 1.2, main = '', xlab = '', ylab = '')

UCBAdmissions
mosaicplot(~Gender + Admit + Dept, data = UCBAdmissions, color = TRUE, off = 1)
mtext(side = 3, quote(bold('UCBAdmissions')),
      line = 1, cex = 1.5)
mtext(side = 1, 'Gender', line = 2, cex = 1.3)
mtext(side = 2, 'Dept', line = 1, cex = 1.3)

mosaicplot(~Dept + Admit+Gender, data = UCBAdmissions, color = TRUE, off = 2)

