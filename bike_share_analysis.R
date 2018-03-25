library(ggplot)
install.packages("ggplot")
library(corrplot)
library(dplyr)
library(car)


setwd("F:/USF/SPRING_18/SDM/R_Directory")
d <- read.csv("bsdata.csv")
hourdat <- read.csv("BikeShare.csv")
hist(d$cnt,breaks = 15, col="red")

#The Independent variable follows a normal distribution.
ggplot(d)+geom_histogram(aes(cnt),bins = 50)+theme_minimal()

#Histograms and Box Plots of dependednt variables.
ggplot(d)+geom_histogram(aes(temp),bins = 50)+theme_minimal()

multiplot(a1, a2, a3, cols=1)
a1<-ggplot(d)+geom_histogram(aes(atemp),bins = 50)+theme_minimal()
a1 + ggtitle("Temperature Spread") +xlab("Temperature") + ylab("Frequency")
a1
a2<-ggplot(d)+geom_histogram(aes(hum),bins = 50)+theme_minimal()
a2 + ggtitle("Humidity") +xlab("Humidity") + ylab("Frequency")
a2
a3<-ggplot(d)+geom_histogram(aes(windspeed),bins = 50)+theme_minimal()
a3 + ggtitle("Wind Speed") +xlab("Speed") + ylab("Frequency")
a3

#Drop registered
d$registered <- NULL
d$instant <- NULL
d$casual<-NULL
View(d)


#Correlation plot
#Multi Collinearity Analysis
dcont <- d[ ,9:13]
cm<-round(cor(dcont), 3)
cm
corrplot(cm, method="circle")


#Very Large correlation betwen temp and atemp.
#Dropping temp

d$temp <- NULL

dcont <- d[ ,8:12]
cm<-round(cor(dcont), 3)
cm
corrplot(cm, method="circle")

##THe continuous variables have a linear relationship with the independent variable as seen from the below plots.
b1 <- ggplot(d)+geom_smooth(aes(atemp,cnt),method = "lm")+geom_point(aes(atemp,cnt)) +theme_minimal()
b2 <- ggplot(d)+geom_smooth(aes(hum,cnt),method = "lm")+geom_point(aes(hum,cnt)) +theme_minimal()
b3 <- ggplot(d)+geom_smooth(aes(windspeed,cnt),method = "lm")+geom_point(aes(windspeed,cnt)) +theme_minimal()
multiplot(b1, b2, b3, cols=1)
d$casual <- NULL


##Convert categorical variables to factors

d$season <- as.factor(d$season)
d$yr <- as.factor(d$yr)
d$mnth <- as.factor(d$mnth)
d$holiday <- as.factor(d$holiday)
d$weekday <-as.factor(d$weekday)
d$workingday <- as.factor(d$workingday)
d$weathersit <-as.factor(d$weathersit)

d$dteday <- NULL
View(d)
#Model Using all predictor variables
View(d)
m1 <- lm(d$cnt ~ d$season + d$yr+ d$mnth +d$holiday + d$workingday + d$weekday + d$weathersit + d$atemp + d$hum + d$windspeed)
summary(m1)
alias(m1)
vif(m1)
par(mfrow=c(2,2))
plot(m1)


influencePlot(m1,id.method = "identify")

#Workingday is correlated with another variable in the dataset and hence we remove it.
d %>% group_by(workingday) %>% summarise(mean(cnt))
d$workingday <- NULL
d$workingday <- NULL
m1 <- lm(cnt ~.,data = d[,2:10])
summary(m1)
par(mfrow=c(2,2))
plot(m1)
vif(m1)
alias(m1)
m2 <- lm(d$cnt ~  d$season + d$yr+ d$mnth +d$holiday  +d$weathersit + d$atemp + d$hum + d$windspeed)
summary(m2)
par(mfrow=c(2,2))
plot(m2)
vif(m2)

m3 <- lm(d$cnt ~   d$yr+d$mnth +d$holiday +d$weathersit + d$atemp + d$hum + d$windspeed)
summary(m3)
par(mfrow=c(2,2))
plot(m3)
vif(m3)

m4 <- lm(d$cnt ~    d$mnth +d$holiday  +d$weathersit + d$atemp + d$hum + d$windspeed)
summary(m4)
par(mfrow=c(2,2))
plot(m4)
vif(m4)


AIC(m1,m2,m4,m3)
BIC(m1,m2,m4,m3)

shapiro.test(m1$res)
shapiro.test(m2$res)
shapiro.test(m4$res)
shapiro.test(m3$res)
shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(d$cnt)

bartlett.test(list(m1$res,m1$fit))
bartlett.test(list(m2$res,m2$fit))
bartlett.test(list(m4$res,m4$fit))
bartlett.test(list(m3$res,m3$fit))

par(mfrow=c(2,2))
plot(m1$res~m1$fit)
abline(h=0,col="red")
plot(m2$res~m2$fit)
abline(h=0,col="red")
plot(m4$res~m4$fit)
abline(h=0,col="red")

library(car)
par(mfrow=c(2,2))
influencePlot(m1,id.method = "identify")
influencePlot(m2,id.method = "identify")
influencePlot(m4,id.method = "identify")

#Lag plot
qplot(d$cnt[1:729],d$cnt[2:730])


set.seed(99)
x <- sample(731,183)
test <- d[x,]
train <- d[-x,]

m4 <- lm(d$cnt ~    d$mnth +d$holiday  +d$weathersit + d$atemp + d$hum + d$windspeed)
model <- lm(cnt ~ mnth + holiday + weathersit + atemp +hum +windspeed, data=train)
summary(model)

##m5 <- lm(cnt ~ mnth + holiday + weathersit + atemp +hum +windspeed +atemp*hum*windspeed, data=train)
##summary(m5)
shapiro.test(d$cnt[sample(731,100)])

preds <- predict(model,newdata = test) 
RMSE(preds,test$cnt)
rmse

shapiro.test(m1$residuals)
shapiro.test(m2$residuals)
shapiro.test(m3$residuals)
shapiro.test(m4$residuals[sample(731,100)])

str(m4$residuals[sample(731,100)])
bartlett.test(list(m4$res,m4$fit))
?bartlett.test
bartlett.test(m4$fitted.values)
bartlett.test(m4)
