library(neuralnet)
library(Metrics)
library(MASS)

getwd()

data <- read.csv("test.csv")
input_data <- data[3:18]

data <- read.csv("After.csv")
output_data <- data

input_output_data <- cbind(input_data,output_data)

keeps <- c("X1M","X4M","X3M","X2M","SW","NW")
data<-input_output_data[keeps]

set.seed(2016)
n = nrow(data)

data_train <- data[1:70,]
train <- data_train[sample(1:nrow(data_train)),]
test <- data[30:70,]

f<-SW + NW ~ X1M + X4M + X3M + X2M
fit<-neuralnet(f, 
               data = train,
               hidden = c(10,12,20),
               algorithm = "rprop+",
               err.fct ="sse",
               act.fct = "logistic",
               threshold = 0.1,
               linear.output = TRUE)

summary(fit)


pred_ANN = predict(fit, train, interval = "confidence")

plot(fit)
