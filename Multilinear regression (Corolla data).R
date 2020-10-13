
View(cor)
attach(cor)

cor1 <- cor[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(cor1)
summary(cor1)
attach(cor1)

install.packages("moments")
library(moments)

#Graphical exploration
hist(Price)
summary(Price)
skewness(Price)
kurtosis(Price)
qqnorm(Price)
qqline(Price)

hist(Age_08_04)
summary(Age_08_04)
skewness(Age_08_04)
kurtosis(Age_08_04)
qqnorm(Age_08_04)
qqline(Age_08_04)

hist(KM)
summary(KM)
skewness(KM)
kurtosis(KM)
qqnorm(KM)
qqline(KM)

hist(Quarterly_Tax)
summary(Quarterly_Tax)
skewness(Quarterly_Tax)
kurtosis(Quarterly_Tax)
qqnorm(Quarterly_Tax)
qqline(Quarterly_Tax)


# Explore the data
plot(Age_08_04,Price) # Plot relation ships between each X with Y
plot(KM,Price)

## Or make a combined plot
pairs(cor1)   # Scatter plot for all pairs of variables

cor(Age_08_04,Price)
cor(cor1) # correlation matrix

# The Linear Model of interest
model.str <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight) # lm(Y ~ X)
summary(model.str)

model.carV<-lm(Price~cc)
summary(model.carV)

model.carW <- lm(Price~Doors)
summary(model.carW)

model.carVW <- lm(Price~cc+Doors)
summary(model.carVW)

model.ccdr <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight)
summary(model.ccdr)

### Partial Correlation matrix
install.packages("corpcor")
library(corpcor)

cor(cor1)

cor2pcor(cor(cor1))

install.packages("car")
library(car)

# Deletion Diagnostics for identifying influential variable
influence.measures(model.str)
influenceIndexPlot(model.str) # Index Plots of the influence measures
influencePlot(model.str)# A user friendly representation of the above
?influencePlot

## Regression after deleting the 81st observation
model.str1<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=cor1[-81,])
summary(model.str1)


### Variance Inflation Factors
vif(model.str1)  # VIF is > 10 => collinearity

model.dr<-lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight)
summary(model.dr)

model.cc<-lm(Price~Age_08_04+KM+HP+Doors+Gears+Quarterly_Tax+Weight)
summary(model.cc)

model.gr<-lm(Price~Age_08_04+KM+HP+cc+Doors+Quarterly_Tax+Weight)
summary(model.gr)


#### Added Variable Plots ######
avPlots(model.str, id.n=5, id.cex=100, col="red")
?avPlots

install.packages("MASS")
library(MASS)
stepAIC(model.str) # backward

model.final <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data=cor1[-81])
summary(model.final)
confint(model.final,level=0.95)
pred <- predict(model.final,interval="predict")
pred <- as.data.frame(pred)
View(pred)

cor(pred$fit,cor1$Price)

avPlots(model.final, id.n=2, id.cex=0.8, col="red")

vif(model.final)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.