
View(str)
attach(str)

summary(R.D.Spend)
summary(Administration)
summary(Marketing.Spend)

install.packages("moments")
library(moments)

skewness(R.D.Spend)
kurtosis(R.D.Spend)

skewness(Administration)
kurtosis(Administration)

skewness(Marketing.Spend)
kurtosis(Marketing.Spend)

hist(R.D.Spend)
hist(Administration)
hist(Marketing.Spend)

qqnorm(Administration)
qqline(Administration)

qqnorm(R.D.Spend)
qqline(R.D.Spend)

qqnorm(Marketing.Spend)
qqline(Marketing.Spend)

summary(str)

# Explore the data

plot(R.D.Spend, Profit) # Plot relation ships between each X with Y
plot(Administration,Profit)

## Or make a combined plot
pairs(str)   # Scatter plot for all pairs of variables

str1 <- str[,-4]
View(str1)
cor(str1) # correlation matrix

# The Linear Model of interest
model.str <- lm(Profit~R.D.Spend+Administration+Marketing.Spend) # lm(Y ~ X)
summary(model.str)

model.rd<-lm(Profit~R.D.Spend)
summary(model.rd)

model.ad<-lm(Profit~Administration)
summary(model.ad)

model.mr <- lm(Profit~Marketing.Spend)
summary(model.mr)


###Partial Correlation matrix###
install.packages("corpcor")
library(corpcor)
cor(str1)

cor2pcor(cor(str1))
?cor2pcor

install.packages("car")
library(car)

# Deletion Diagnostics for identifying influential variable
influence.measures(model.str)
influenceIndexPlot(model.str) # Index Plots of the influence measures
influencePlot(model.str)# A user friendly representation of the above
?influencePlot

## Regression after deleting the 50th observation
model.str1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend, data=str1[-50,])
summary(model.str1)


### Variance Inflation Factors
vif(model.str1)  # VIF is > 10 => collinearity

model.rd<-lm(Profit~Administration+Marketing.Spend)
summary(model.rd)

model.ad<-lm(Profit~R.D.Spend+Marketing.Spend)
summary(model.ad)

model.mr<-lm(Profit~R.D.Spend+Administration)
summary(model.mr)

model.adst <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(model.adst)

#### Added Variable Plots ######
avPlots(model.str, id.n=5, id.cex=100, col="red")
?avPlots
install.packages("MASS")
library(MASS)
stepAIC(model.final) # backward


model.final<- lm(Profit~R.D.Spend+Marketing.Spend, data=str1[-50])
summary(model.final)
confint(model.final,level=0.95)
pred <- predict(model.final,interval="predict")
pred <- as.data.frame(pred)
View(pred)

cor(pred$fit, str1$Profit)

avPlots(model.final, id.n=2, id.cex=0.8, col="red")

vif(model.final)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.