library(dplyr)
library(plyr)
library(ggvis)
library(caret)
library(randomForest)
library(parallel)
library(doParallel)
library(randomForestExplainer)
library(ggcorrplot)
library(party)

#######################################################################################################
######Build the Random Forest Machine Learning Models, save model outputs and evaluate performance#####
########################Data inputs generated from 'data_prepro.R'#####################################
#######################################################################################################


setwd("C:/Users/farellam/Documents/DryFlux/data/")

data <- read.csv("RFdata.csv") #coarse resolution data with revised SPEI values
data$site <- as.factor(data$site)
summary(data$site)

#add month column
data$month <- substr(data$monthyear, 1,3)
data$month <- as.factor(data$month)

#replace Inf spei values with NA
data <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),NA)))
  
#calculate VPsat
data$VPsat <- 610.78 * exp((17.269*data$Tavg)/(237.3+data$Tavg))
data$vpd <- data$VPsat - data$vap # for fine res data
data$vpd <- data$VPsat - data$vap*100 #for coarse res CRU data
  
names(data)
#select columns wanted for RF
  All_sites <- data[c(2,3,4,6:9,29,11:26)]
  All_sites <- data[c(2,3,4,7,8,12:14,16,22:27)]#for no ecohydrological variables
  #remove samples with incomplete observations
  All_sites <- All_sites[complete.cases(All_sites),]
  All <- All_sites
  names(All_sites)
  rfdata <- All_sites[,c(3:23)]

#Check for highly correlated variables: 
correlationMatrix <- cor(rfdata)
# summarize the correlation matrix
print(round(correlationMatrix,2))
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7, names=TRUE)
highlyCorrelated

corr <- round(cor(All[,-c(1:2)]), 1)
ggcorrplot(corr, type = "lower", lab = TRUE)


################RANDOM FOREST ANALYSIS#################
#Split into training and testing data
#index <- createDataPartition(All$GPP, p=0.80, list=FALSE)
#index
#RFtrain <- All[index,]
#RFtest <- All[-index,]

#Split into training and testing data--- at the site level
#create a list of the sites
set.seed(57)
sites <- as.character(All$site)
sites <- unique(sites)
#define the testing sites
testIDs <- c("us-fuf", "us-scw", "us-so3", "us-srg", "us-vcm")
#define the training sites
trainIDs <- subset(sites, !(sites%in% testIDs))

#Divide data set into training and test set (80% training and 20% testing)
RFtrain <- subset(All, site %in% trainIDs)
RFtest <- subset(All, site %in% testIDs)

#Run the RF
#set the parameters for the training model; number = number of folds or number of resampling iterations; repeats = For repeated k-fold cross-validation only: the number of complete sets of folds to compute
myControl <- trainControl(method="repeatedcv", repeats=3, number=5)

cluster <- makeCluster(detectCores() - 1) 
registerDoParallel(cluster)
RFmodel <- train(RFtrain[,c(4:13)], RFtrain[,"GPP"],
                    method='rf', trControl=myControl, importance=TRUE, 
                    do.trace=TRUE, allowParallel=TRUE)
#Stop cluster
stopCluster(cluster)
registerDoSEQ()

#save the RFmodel
#saveRDS(RFmodel, "Z:/SRER/Martha/DryFlux/RFmodels/coarse_new.rds")
saveRDS(RFmodel, "C:/Users/farellam/Documents/DryFlux/RFmodels/re-do/coarse_new.rds")

diagnostics <- function(model, test, cols){
  print(model)
  print(cols)
  pred <- as.numeric(predict(object=model, test[,cols]))
  perf <- (cor(pred, test[,resvar]))
  print(paste("RF test performace R2 =", perf))
  print(postResample(pred=pred, obs=test[,resvar]))
  MOD <- model$finalModel
  par(mfrow=c(1,2))
  varImpPlot(MOD)
  d <-qplot(pred, test[,resvar]) + 
    geom_point(shape=19, colour="tomato2", size=3)+
    geom_abline(intercept = 0, slope = 1)+
    xlim(0,7.5)+
    ylim(0,7.5)+
    xlab("Flux monthly GPP")+
    ylab("Predicted monthly GPP")+
    theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
    #annotate("text", label = lb1, parse=TRUE, x = 0.5, y = 6, size = 5, colour = "Black")+
    #annotate("text", label = RMSE1, parse=TRUE, x = 0.5, y = 5.5, size = 5, colour = "Black")+
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  print(d)
  
}



expvar <- colnames(RFtrain[c(4:13)])
resvar <- colnames(RFtrain[3])

diagnostics(RFmodel, RFtest, expvar)
importance_frame <- measure_importance(RFmodel$finalModel)
plot_multi_way_importance(importance_frame, x_measure = "node_purity_increase", y_measure = "mse_increase", size_measure = "p_value", no_of_labels = 5)
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

varImpPlot(RFmodel$finalModel)
plot_predict_interaction(RFmodel$finalModel, RFtrain, "spei12", "GPP")
RFmodel$finalModel
explain_forest(RFmodel$finalModel, interactions = TRUE, data = RFtrain)

##R2 and RMSE on testing data
#use the RF model to predict GPP on the testing data
pred <- predict(RFmodel, RFtest[,4:22])
#calculate RMSE
(sqrt (mean((RFtest$GPP - pred)^2)))
#define R2 function
rsq <- function (x, y) cor(x, y) ^ 2
#calcualte R2
rsq(RFtest$GPP, pred)


#variable importance values calculated with conditional permutation importance 
##Takes about 45mins for results
RFtrain2 <- RFtrain[,-c(1,2,23)]
model2 <- party::cforest(GPP ~ ., data = RFtrain2, control=cforest_unbiased(ntree = 500, trace=TRUE, mtry=10))
party::varimp(model2, conditional = TRUE) 
model2
