#Loading dataset
setwd("C:/Users/HP/OneDrive/Documents/Rstudio")
getwd()
rm(stats)
stats <- read.csv("P2-Demographic-Data (1).csv")
head(stats)
str(stats)
#libraries
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("lmtest")
??lmtest
library(dplyr)
library(broom)
library(ggpubr)
library(olsrr)
library(lmtest)
library(ggplot2)
#convert variable type from chr to factor
stats$Income.Group <- as.factor(stats$Income.Group)  #if you read data by setting working directory
head(stats)
str(stats)
#Drop  useless column
stats = subset(stats, select = -c(Country.Code))
head(stats)
summary(stats)
#Creating a basic scatterplot
x= stats$Birth.rate
y= stats$Internet.users
#sqr <- sqrt(y)
basic <- ggplot(data= stats, aes(x=x,
y=y))
scatter <- basic +
labs(x="Birth Rate",
y= "Internet Users") +
ggtitle("Internet Users vs Birth Rate") +
geom_point(size=3, shape=21, color="black",
fill="green", alpha= 0.75)
#adding theme
scatter <- scatter +
theme(
axis.title.x = element_text(colour= "Blue",
size= 20),
axis.title.y = element_text(colour= "Blue",
size= 20),
axis.text.x = element_text(size= 15),
axis.text.y = element_text(size= 15),
plot.title = element_text(size= 25,color="dodgerblue4"),
legend.title = element_text(size= 30),
legend.text = element_text(size=20),
text = element_text(family= "Comic Sans MS")
)
scatter  #we will use it after
x= stats$Birth.rate
y= stats$Internet.users
cor(x, y, method= 'spearman')
head(stats)
nrow(stats)
#SIMPLE LINEAR REGRESSION
x= stats$Birth.rate
y= stats$Internet.users
sqr <- sqrt(y)
model <- lm(y ~ x, data = stats)
summary(model)
#---------- Checking for assumptions
#-----1 Independence of observations(not needed here)
dwtest(model)
#------2 Normality of errors
#histogram of residuals
hist(residuals(model), main= "Histogram of residuals(model)",
col= "gray", border= "blue")
#qq-plot for residuals
qqnorm(residuals(model))
qqline(residuals(model))
plot(fitted(model), residuals(model))
abline(0,0)
boxplot(residuals(model))
#import olsrr library
ols_test_normality(residuals(model))
#Kolmogorovtest shows that data are normally distributed
#Checking homoscedascity
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
#White test to check for homoscedascity
bptest(model)
#Visualization of regression
scatter +
stat_smooth(method= "lm", formula = sqr ~ x) +
ggtitle("Linear Regression: Internet Users vs. Birth Rate", ) +
stat_regline_equation(label.x=40, label.y=10) +
stat_cor(aes(label=..rr.label..), label.x=40, label.y=9)
cor,test(x, y, method= 'spearman')
cor.test(x, y, method= 'spearman')
cor.test(x, y)
cor(x, y, method= 'spearman')
cor.test(x, y)
?cor.test
cor.test(x, y, method= "pearson")
cor.test(x, y, method= "pearson")
cor.test(y, x, method= "pearson")
summary(stats)
#Loading dataset
setwd("C:/Users/HP/OneDrive/Documents/Rstudio")
getwd()
rm(stats)
stats <- read.csv("P2-Demographic-Data (1).csv")
head(stats)
str(stats)
#libraries
install.packages("dplyr")
install.packages("broom")
source("~/Rstudio/Ergasia Roula.R")
source("~/Rstudio/Ergasia Roula.R")
source("~/Rstudio/Ergasia Roula.R")
sqr <- sqrt(y)
model <- lm(sqr ~ x, data = stats)
summary(model)
sqr <- sqrt(y)
model <- lm(sqr ~ x, data = stats)
summary(model)
setwd("C:/Users/HP/OneDrive/Documents/Rstudio")
getwd()
rm(stats)
stats <- read.csv("P2-Demographic-Data (1).csv")
head(stats)
str(stats)
sqr <- sqrt(y)
model <- lm(sqr ~ x, data = stats)
summary(model)
x= stats$Birth.rate
y= stats$Internet.users
sqr <- sqrt(y)
model <- lm(sqr ~ x, data = stats)
summary(model)
x= stats$Birth.rate
y= stats$Internet.users
x
source("~/Rstudio/Ergasia Roula.R")
