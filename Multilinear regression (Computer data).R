
View(comp)
attach(comp)

comp1 <- comp[,-c(1,7,8,9)]
View(comp1)
summary(comp1)
attach(comp1)

install.packages("moments")
library(moments)

# Graphical exploration
hist(speed)
summary(speed)
skewness(speed)
kurtosis(speed)
qqnorm(speed)
qqline(speed)

hist(hd)
summary(hd)
skewness(hd)
kurtosis(hd)
qqnorm(hd)
qqline(hd)

hist(ads)
summary(ads)
skewness(ads)
kurtosis(ads)
qqnorm(ads)
qqline(ads)

hist(trend)
summary(trend)
skewness(trend)
kurtosis(trend)
qqnorm(trend)
qqline(trend)

# Explore the data
plot(price,speed) # Plot relation ships between each X with Y
plot(price,hd)

## Or make a combined plot
pairs(comp1)

# Scatter plot for all pairs of variables
cor(price,speed)
cor(comp1)

# The Linear Model of interest
model.str <- lm(price~speed+hd+ram+screen+ads+trend)
summary(model.str)

model.ad<-lm(price~ads)
summary(model.ad)

model.tr <- lm(price~trend)
summary(model.tr)

model.adtr <- lm(price~ads+trend)
summary(model.adtr)

model.shrm <- lm(price~speed+hd+ram+screen)
summary(model.shrm)


### Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor(comp1)

cor2pcor(cor(comp1))
?cor2pcor

install.packages("car")
library(car)

# Deletion Diagnostics for identifying influential variable
influence.measures(model.str)
influenceIndexPlot(model.str) # Index Plots of the influence measures
influencePlot(model.str)# A user friendly representation of the above
?influencePlot

## Regression after deleting the 1441 & 1701st observation
model.str1<-lm(price~speed+hd+ram+screen+ads+trend, data=comp1[-c(1441,1701),])
summary(model.str1)

### Variance Inflation Factors
vif(model.str1)  # VIF is > 10 => collinearity

model.ad<-lm(price~speed+hd+ram+screen+trend)
summary(model.ad)

model.tr<-lm(price~speed+hd+ram+screen+ads)
summary(model.tr)

model.hd<-lm(price~speed+ram+screen+ads+trend)
summary(model.hd)

model.rm <- lm(price~speed+hd+ram+trend)
summary(model.rm)

#### Added Variable Plots ######
avPlots(model.str, id.n=5, id.cex=100, col="red")
?avPlots

install.packages("MASS")
library(MASS)

stepAIC(model.str) # backward

model.final <- lm(price~speed+hd+ram+trend+ads+screen, data=comp1[-c(1441,1701),])
summary(model.final)
confint(model.final,level=0.95)
pred <- predict(model.final,interval="predict")
pred <- as.data.frame(pred)
View(pred)

comp2 <- comp1[-c(1441,1701),]
cor(pred$fit, comp2$price)


avPlots(model.final, id.n=2, id.cex=0.8, col="red")

vif(model.final)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.