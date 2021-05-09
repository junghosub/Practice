library(MASS)

set.seed(1234567)

x <- seq(1,10)
y <- 2.5 + 0.5 *  x + rnorm(10,0,1)
y[10] <- -10

# 최고제곱 회귀모형 적합
m0 <- lm(y~x)

# 후버의 M 추정 회귀모형 적합
m1 <- rlm(y~x)

# LMS 회귀모형 적합
m2 <- lqs(y~x, method = 'lms')

# LTS 회귀모형 적합
m3 <- lqs(y~x, method = 'lts')

# LSE vs M-est
par(mfrow = c(1,2))
plot(y~x, ylim = c(-10,10), main = 'by LSE')
abline(m0$coefficients)

plot(y~x, ylim = c(-10,10), main = 'by M-est')
abline(m1$coefficients)

# work 1
par(mfrow = c(1,1))
plot(y~x, ylim = c(-10,10), main = 'Regression fittings')
abline(m0$coefficients)
abline(m1$coefficients, col = 'blue', lty = 3)
abline(m2$coefficients, col = 'red', lty = 3)
abline(m3$coefficients, col = 'green', lty = 3)

# work 2
x <- seq(1,10)
y <- 2.5 + 0.5 *  x + rnorm(10,0,1)
y[10] <- -100
x[10] <- 100

par(mfrow = c(1,2))
m2a <- lqs(y~x, method = 'lts')
m2b <- lqs(y~x, method = 'lts', quantile = 8)

plot(y~x, xlim = c(0,10), ylim = c(0,10), main = 'LTS')
abline(m2a$coefficients, lty = 'dotted', col = 'blue')

plot(y~x, xlim = c(0,10), ylim = c(0,10), main = 'LTS opt')
abline(m2b$coefficients, col = 'red')

# work 3
data("stackloss")
str(stackloss)
stackloss

# 최소제곱
m1 <- lm(stack.loss~Air.Flow + Water.Temp + Acid.Conc.)

# 로버스트 회귀직선
m2 <- rlm(stack.loss~Air.Flow + Water.Temp + Acid.Conc.)

# LMS
m3 <- lqs(stack.loss~Air.Flow + Water.Temp + Acid.Conc., method = 'lms')

# LTS
m4 <- lqs(stack.loss~Air.Flow + Water.Temp + Acid.Conc., method = 'lts', quantile = 16)

# 회귀계수 비교
m1$coefficients
m2$coefficients
m3$coefficients
m4$coefficients

# 잔차 비교
par(mfrow = c(1,1))
boxplot(m1$residuals, m2$residuals, m3$residuals, m4$residuals, 
        ylab = 'residuals', xlab = 'LS-M-LMS-LTS Regression')

# 잔차 이상치 제거 후 회귀직선 추정
m5 <- lm(stack.loss~Air.Flow + Water.Temp + Acid.Conc., data = stackloss[abs(m4$residuals) < 4,])
summary(m5)
