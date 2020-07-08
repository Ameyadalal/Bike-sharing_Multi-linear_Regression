bike <- read.csv('bikeshare1.csv')
head(bike)
any(is.na(bike))
str(bike)
summary(bike)

# missing values
library(Amelia)
missmap(bike,y.at=c(1),y.labels = c(''),col=c('black','peachpuff'))

# ASSUMPTIONS 

# Homoscedasticity of residuals Assumptions-1
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
mod <- lm(count ~ season+humidity+holiday+workingday+casual, data=bike)  # linear model
plot(mod)

# The mean of residuals is zero
mod_1 <- lm(count ~ season+humidity+holiday+workingday+casual, data=bike)
mean(mod_1$residuals)

# Normality of residuals
par(mfrow=c(2,2))
mod1 <- lm(count ~ season+humidity+holiday+workingday+casual, data=bike)
plot(mod1)

# Exploratory Data Analysis
library(ggplot2)

# Creating a scatter plot for Setting a good alpha value.
ggplot(bike,aes(temp,count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()

# Plot count versus datetime based on temperature.
bike$datetime <- as.POSIXct(bike$datetime)
ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5) + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()

# The correlation between temp and count
cor(bike[,c('temp','count')])

# Create a boxplot, count vs each season
ggplot(bike,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) +theme_bw()

# Correlation and Now we can see some mild correlation with Average price and region
library(corrplot)
library(corrgram)
num.cols <- sapply(bike, is.numeric)
print(num.cols)
corr <- cor(bike[,num.cols])
print(corr)
corrplot(corr, method='color')

# Feature Engeering 
# Creating a scatterplot of count versus hour for working days
library(dplyr)
pl <- ggplot(filter(bike,workingday==1),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

# Creating the same plot for non working days
pl1 <- ggplot(filter(bike,workingday==0),aes(hour,count)) 
pl1 <- pl1 + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl1 <- pl1 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl1 + theme_bw()

##split the data into train and test set
library(caTools)
set.seed(101) 
sample <- sample.split(bike$count, SplitRatio = 0.7)
# 70% of data -> train
train <- subset(bike,sample == TRUE)
# 30% will be test 
test <- subset(bike,sample == FALSE)

# Train and Building a model 
bike$hour <- as.numeric(bike$hour)
model <- lm(count ~ .-hour-season-datetime-casual,train)
summary(model)
str(bike)

#predictors 
count.predictions <- predict(model,test)
results <- cbind(count.predictions,test$count)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
print(head(results))

## Residuals of the model 
res <- residuals(model)
#class(res)
res <- as.data.frame(res)
head(res)

# ploting a graph on res 
ggplot(res,aes(res)) + geom_histogram(fill='blue',alpha=0.5,bins = 30)

# mean squared error 
mse <- mean((results$actual - results$predicted)^2)
print('MSE')
print(mse)

# RMSE 
print("Squared Root of MSE")
print(mse^0.5)

### Sum of the squared errors 
SSE <- sum ( ( results$predicted - results$actual)^2)
### Sum of squared total
SST <- sum ( ( mean(bike$count) - results$actual)^2)

R2 <- 1 - SSE/SST 
print('R2')
print(R2)