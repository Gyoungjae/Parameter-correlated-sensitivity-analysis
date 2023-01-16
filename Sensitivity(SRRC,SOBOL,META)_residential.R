library(sensitivity)
library(randomForest)
library(ggplot2)
library(nnet)
library(devtools)
library(neuralnet)
library(Metrics)
rm(list=ls())

setwd("C:/Users/ehdna/OneDrive - konkuk.ac.kr/학술발표대회/2022_동계학술발표대회/Sensitivity Analysis/residential")
getwd()

#data_train<- read.csv("AllCombinedResults_2006.csv")
#data_train<- read.csv("AllCombinedResults_2009.csv")
#data_train<- read.csv("AllCombinedResults_2012.csv")
#data_train<- read.csv("AllCombinedResults_2015.csv")
#data_train<- read.csv("AllCombinedResults_2018.csv")
data_train<- read.csv("Uniform_AllCombinedResults.csv")
#data_train<- read.csv("Truncated_AllCombinedResults.csv")

colnames(data_train) <- c("ROOF", "WALL", "FLOOR", "WIN", "SHGC", "LPD","HEATEFF","COP","EPD","HSP", "CSP", "OCC", "TOTAL_E")

B_E = sample(1:nrow(data_train), 0.7*nrow(data_train))

train_data=data_train[B_E,]
test_data = data_train[-B_E,]

colnames(data_train) <- c("ROOF", "WALL", "FLOOR", "WIN", "SHGC", "LPD","HEATEFF","COP","EPD","HSP", "CSP", "OCC", "TOTAL_E")
colnames(train_data) <- c("ROOF", "WALL", "FLOOR", "WIN", "SHGC", "LPD","HEATEFF","COP","EPD","HSP", "CSP", "OCC", "TOTAL_E")
MLR_model =  lm(TOTAL_E ~ ROOF + WALL + FLOOR + WIN + SHGC + LPD + HEATEFF + COP + EPD + HSP + CSP + OCC, data = train_data)
summary(MLR_model)
pred_MLR = predict(MLR_model, train_data)
#rmse(pred_MLR, train_data$TOTAL_E)

#ANN_model =neuralnet(TOTAL_E ~ ROOF + WALL + FLOOR + WIN + SHGC + LPD + HEATEFF + COP + EPD + HSP + CSP + OCC, data = train_data, hidden=c(10,10,10), act.fct = "logistic", linear.output = FALSE)



##################################################################
#######SRRC 민감도 분석##########################################
srrc.result <- src(data_train[,c(1:12)],data_train[,13],rank=T)
srrc <- abs(srrc.result$SRRC[,1])
srrc <- data.frame(srrc)
rownames(srrc) <- c("ROOF", "WALL", "FLOOR", "WIN", "SHGC", "LPD","HEATEFF","COP","EPD","HSP", "CSP", "OCC")


#################################################################
######SoboL 민감도 분석#########################################
x1 <- data_train
set.seed(1)
x2  <- data_train[sample(1:nrow(data_train)),]
Sa <- sobol2002(mode = MLR_model, X1 = x1, X2 = x2, order =2, nboot=500)
print(Sa["S"]) #Total sobol
ggplot(Sa)
Sobol <- print(Sa["S"])
Sobol <- Sobol[["S"]][["original"]]
Sobol <- data.frame(Sobol[1:12])
rownames(Sobol) <- c("ROOF", "WALL", "FLOOR", "WIN", "SHGC", "LPD","HEATEFF","COP","EPD","HSP", "CSP", "OCC")

#################################################################
#######Meta 민감도 분석##########################################
library(tgp)
data_train_baby <- data_train[sample(nrow(data_train),500),]
meta <- sens(X=data_train_baby[,1:12], Z=data_train_baby[,13], nn.lhs=40, model=bgpllm)
meta <- colMeans(meta$sens$S)

Meta.results <- data.frame(meta)

################################################################
######SVI#######################################################
SVI_RESULT <- cbind(srrc,Sobol[1:12,1],Meta.results)
colSums(abs(SVI_RESULT))

SVI_each <- matrix(nrow = 12, ncol = 3)
for (i in 1:3){
  SVI_each[,i] <- (abs(SVI_RESULT)[,i])/colSums(abs(SVI_RESULT))[i]
}


rownames(SVI_each) <- c("ROOF", "WALL", "FLOOR", "WIN", "SHGC", "LPD","HEATEFF","COP","EPD","HSP", "CSP", "OCC")
SVI_RESULT <- data.frame(SVI_each)
colnames(SVI_RESULT) <- c("SVI_SRRC", "SVI_SOBOL", "SVI_TGP")
SVI_RESULT$SVI_TOTAL <- (SVI_RESULT$SVI_SRRC + SVI_RESULT$SVI_SOBOL + SVI_RESULT$SVI_TGP)/3
colnames(SVI_RESULT) <- c("SVI_SRRC", "SVI_SOBOL", "SVI_TGP", "SVI_TOTAL")

#################################################################
Results <- data.frame(srrc,Sobol[1:12,1],Meta.results)
colnames(Results) <- c("SRRC", "SOBOL", "TGP")
TOTAL_Results <- data.frame(Results,SVI_RESULT)

#################################################################
#write.csv(TOTAL_Results,"Truncated_sensitivity_2006.csv")
#write.csv(TOTAL_Results,"Truncated_sensitivity_2009.csv")
#write.csv(TOTAL_Results,"Truncated_sensitivity_2012.csv")
#write.csv(TOTAL_Results,"Truncated_sensitivity_2015.csv")
#write.csv(TOTAL_Results,"Truncated_sensitivity_2018.csv")
write.csv(TOTAL_Results,"Sensitivity_Results_Truncated.csv")


#write.csv(TOTAL_Results,"Uniformed_sensitivity_2006.csv")
#write.csv(TOTAL_Results,"Uniformed_sensitivity_2009.csv")
#write.csv(TOTAL_Results,"Uniformed_sensitivity_2012.csv")
#write.csv(TOTAL_Results,"Uniformed_sensitivity_2015.csv")
#write.csv(TOTAL_Results,"Uniformed_sensitivity_2018.csv")
#write.csv(TOTAL_Results,"Sensitivity_Results_Uniform.csv")

