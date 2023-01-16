library(dplyr)
library(boot)
library(sensitivity)

setwd("C:/Users/ehdna/OneDrive - konkuk.ac.kr/학술발표대회/2022_동계학술발표대회/R_Sampling_Multicorrelation")
getwd()


ecojob <- read.csv("AllCombinedResults_2009.csv",header=T)
ecojob2 <- read.csv("U_AllCombinedResults_2009.csv",header=T)

data_train <- ecojob[,-c(1)]

colnames(data_train)<-c("WALL","FLOOR","WIN","SHGC","LPD","HEATEFF","COP",
                       "EPD","HSP","CSP","OCC","TOTAL_E")
dim(data_train);names(data_train)


maxs <- apply(data_train,2,max)
mins <- apply(data_train,2,min)

str(mins)

scaled.data_train <- as.data.frame(scale(data_train, center = mins, scale = maxs -mins))

apply(data_train[,1:11],2,min)
apply(data_train[,1:11],2,max)

##########################################################################################################
###########srrc민감도 ,src 민감도
library(randomForest)
src.result <- src(data_train[,c(1:11)],data_train[,12])
src <- (abs(src.result$SRC[,1]))

srrc.result <- src(data_train[,c(1:11)],data_train[,12],rank=T)
srrc <- abs(srrc.result$SRRC[,1])

##########################################################################################################
###########Monte Carlo Bayesian Sensitivity Analysis_Meta-model based
library(tgp)
#아래는 시간이 오래 걸리니 주의해서 누르기
variance.result <- variance.result <- sens(X=data_train[,1:11],Z=data_train[,12],nn.lhs=700, model=bgpllm)
meta <- colMeans(variance.result$sens$S)

parameter<-c("ROOF", "WALL", "WIN", "SHGC", "INF","LPD", "EPD", "HSP", "CSP", "HEATEFF","COP")
Meta.results <- data.frame(meta)


###########################################################################################################
############RF sensitivity analysis
set.seed(1) #매번 같은 난수 생성하기
rf.result <- randomForest(data_train[,c(1:11)],data_train[,c(12)], mtry = 4,ntree = 100, importance = T)
#randomForest(x,y,xtest,ytest,ntree,mtry,important)
#x는 선택적 데이터 프레임, y는 A response vector. If a factor, classification is assumed, otherwise regression is assumed  
#ntree 트리의 갯수
#importance 예측 변수의 중요성을 평가
RF <- as.data.frame(importance(rf.result,type=1))
colnames(RF) <- ("RF");RF
RF<- (abs(RF))

###########################################################################################################
###########FAST 민감도 분석
#1 다중 회귀분석(Multi Linear Model=MLR) 모델 만들기
B_E = sample(1:nrow(data_train), 0.7*nrow(data_train))
train_data=data_train[B_E,]
test_data = data_train[-B_E,]

#2 모델 만들기
MLR_model = lm(Annual.T ~ ROOF + WALL + WIN + SHGC + INF + LPD + EPD + HSP + CSP + HEATEFF + COP, data = train_data)

#3 회귀선이 모델에 적합한지 검정
summary(MLR_model) #F-검정, 설명력 검정(R-square)

#4 모델 평가
pred = predict(MLR_model, test_data)
cor(pred, test_data$Annual.T)

#FAST민감도 분석
factors <- c("ROOF", "WALL", "WIN", "SHGC", "INF","LPD", "EPD", "HSP", "CSP", "HEATEFF", "COP")
#FAST <- fast99(model=MLR_model, factors, n = 1000, q.arg=list(min=0, max=2))
#FAST <- print(FAST)
#FAST <- data.frame(FAST)
#FAST <- FAST$total.order #Total order 만 이용

FAST_modify <- fast99(model=MLR_model, factors, n = 1000, q=c("qunif","qunif","qunif","qunif","qunif","qunif","qunif","qunif","qunif","qunif","qunif"),q.arg=list(list(min=0.11, max=0.22),list(min=0.35, max=0.7),list(min=0.94,max=1.87),list(min=0.13,max=0.38),list(min=0.00,max=0.04),list(min=5.53,max=11.06),list(min=1.84,max=10.08),list(min=18,max=26),list(min=18,max=28),list(min=3,max=4),list(min=3,max=5)))
FAST_modify <- print(FAST_modify)
FAST_modify <- data.frame(FAST_modify)
FAST <- FAST_modify$total.order #Total order 만 이용
FAST

###########################################################################################################
######## T-value
t <- matrix(numeric(0), 11 ,1)
for (i in 2:12){
  t[i-1] <- (coef(MLR_model)[i] / sqrt(diag(vcov(MLR_model)))[i])
}
rownames(t) <- names(data_train[,1:11])
colnames(t) = c("T-value");t

############################################################################################################
##############svi 계산
SVI_RESULT <- cbind(srrc,src,t,Meta.results,RF,FAST)
colnames(SVI_RESULT) <- c("SVI_SRRC","SVI_SRC","SVI_T","SVI_TGP","SVI_RF","SVI_FAST")

colSums(abs(SVI_RESULT))

SVI_each <- matrix(nrow = 11, ncol = 6)
for (i in 1:6){
  SVI_each[,i] <- (abs(SVI_RESULT)[,i])*100/colSums(abs(SVI_RESULT))[i]
}

colnames(SVI_each) <- c("SVI_SRRC", "SVI_SRC", "SVI_T", "SVI_TGP", "SVI_RF", "SVI_FAST")
row.names(SVI_each) <- colnames(data_train[1:11])

SVI <- rowSums(SVI_each)/ncol(SVI_each)
############################################################################################################
###############저장 데이터 

Sensitivity_results <- data.frame(srrc,src,t,Meta.results,RF,FAST,SVI_each,SVI)
colnames(Sensitivity_results) <- c("SRRC","SRC","T-value","TGP","RF","FAST","SVI.SRRC", "SVI.SRC", "SVI.T-value", "SVI.TGP", "SVI.RF", "SVI.FAST","SVI")

##############################################################################################################
###############csv 저장
setwd("C:/ECO_STUDY/SENSITIVITY_ANALYSIS_R/2022.06.07")
write.csv(Sensitivity_results,"Sensitivity_results_1_2022.06.07.csv")
write.csv(Sensitivity_results,"Sensitivity_results_2_2022.06.07.csv")
