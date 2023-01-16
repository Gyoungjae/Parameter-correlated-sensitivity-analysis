library(sensitivity)
library(randomForest)

setwd("C:/Users/ehdna/OneDrive - konkuk.ac.kr/학술발표대회/2022_동계학술발표대회/Sensitivity Analysis")
getwd()

data_train<- read.csv("AllCombinedResults_2006.csv")
data_train<- read.csv("AllCombinedResults_2009.csv")
data_train<- read.csv("AllCombinedResults_2012.csv")
data_train<- read.csv("AllCombinedResults_2015.csv")
data_train<- read.csv("AllCombinedResults_2018.csv")

data_train<- read.csv("U_AllCombinedResults_2006.csv")
data_train<- read.csv("U_AllCombinedResults_2009.csv")
data_train<- read.csv("U_AllCombinedResults_2012.csv")
data_train<- read.csv("U_AllCombinedResults_2015.csv")
data_train<- read.csv("U_AllCombinedResults_2018.csv")

colnames(data_train) <- c("ROOF", "WALL", "FLOOR", "WIN", "SHGC", "LPD","HEATEFF","COP","EPD","HSP", "CSP", "OCC", "TOTAL_E")
MLR_model =  lm(TOTAL_E ~ ROOF + WALL + FLOOR + WIN + SHGC + LPD + HEATEFF + COP + EPD + HSP + CSP + OCC, data = data_train)



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
x2  = data_train[sample(1:nrow(data_train)),]
Sa <- sobol(mode = MLR_model, X1 = x1, X2 = x2, order =2, nboot=500)
print(Sa)
Sobol <- print(Sa)
Sobol <- Sobol[c(1:12),1]
Sobol <- data.frame(Sobol)
rownames(Sobol) <- c("ROOF", "WALL", "FLOOR", "WIN", "SHGC", "LPD","HEATEFF","COP","EPD","HSP", "CSP", "OCC")

#################################################################
#######Meta 민감도 분석##########################################
