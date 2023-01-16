getwd()
library(Metrics)

data_input <- read.csv("test.csv")
data_output <- read.csv("after.csv")

data_input <- data_input[,3:18]

colnames(data_input) <- c("INPUT1", "INPUT2", "INPUT3", "INPUT4", "INPUT5", "INPUT6", "INPUT7", "INPUT8", "INPUT9", "INPUT10", "INPUT11", "INPUT12", "INPUT13", "INPUT14", "INPUT15", "INPUT16")
colnames(data_output) <- c("OUTPUT1", "OUTPUT2", "OUTPUT3", "OUTPUT4", "OUTPUT5", "OUTPUT6","OUTPUT7" ,"OUTPUT8", "OUTPUT9", "OUTPUT10", "OUTPUT11", "OUTPUT12")

data_train <- cbind(data_input, data_output)

B_E = sample(1:nrow(data_train), 0.7*nrow(data_train))

train_data=data_train[B_E,]
test_data = data_train[-B_E,]


MLR_model =  lm(OUTPUT1 + OUTPUT2 + OUTPUT3 + OUTPUT4 + OUTPUT5 + OUTPUT6 + OUTPUT7 + OUTPUT8 + OUTPUT9 + OUTPUT10 + OUTPUT11 + OUTPUT12 
                ~ INPUT1 + INPUT2 + INPUT3 + INPUT4 + INPUT5 + INPUT6 + INPUT7 + INPUT8+ INPUT9 + INPUT10 + INPUT11 + INPUT12+ INPUT13 + INPUT14 + INPUT15+ INPUT16 , data = train_data)

plot(MLR_model)

summary(MLR_model)


pred_MLR = predict(MLR_model, test_data, interval = 'confidence')
pred_MLR = predict(MLR_model, test_data, interval = 'none')
pred_MLR = predict(MLR_model, test_data, interval = 'prediction')
pred_MLR

rmse(pred_MLR, test_data$OUTPUT2)
