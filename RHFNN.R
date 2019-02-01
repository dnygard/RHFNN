### use normalized heart data to fit a neural net
# uses values from P09603, O60609, AND P09960 as input features
# should output likelihood of RHF vs healthy heart

library(neuralnet)

#load data
dat = data.frame(read.csv("normdat.csv", header = TRUE))

# Set a seed
set.seed(500)

index <- sample(1:nrow(dat),round(0.80*nrow(dat)))
train <- dat[index,]
test <- dat[-index,]

# Fitting linear model
glm.fit <- glm(Status~P35354, family=binomial, data=train)
summary(glm.fit)
###shows as significant

glm.fit <- glm(Status~P35354+Q15768+O15232, family=binomial, data=train)
summary(glm.fit)
###having all 3 makes none of the variables significant?

#Conditional density plots 
cdplot(Status~P35354, data=train)
cdplot(Status~Q15768, data=train)
cdplot(Status~O15232, data=train)

# Predicted data from lm and MSE
pr.lm <- predict(glm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
#MSE is zero? is that possible? did something mess up?

#need to scale data, is min-max a good scaling method?