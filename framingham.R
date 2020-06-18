framingham = read.csv("framingham.csv")
str(framingham)
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

trainCHD = subset(framingham, split == TRUE)
testCHD = subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD ~., data = trainCHD, family = binomial)
summary(framinghamLog)

predictTest = predict(framinghamLog, type = 'response', newdata = testCHD)

# Create confusion matrix with threshold value 0.5

table(testCHD$TenYearCHD, predictTest > 0.5)

library(ROCR)

ROCRpred = prediction(predictTest, testCHD$TenYearCHD)

auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc 

# AUC comes as 0.74 so overall accuracy was only slightly better than Baseline
# However AUC shows that it can differentiate between low risk and high risk pretty well.