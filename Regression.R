rm(list=ls())
dataset_old = read.csv("Retention_train.csv")
score = read.csv("Retention_score.csv")

# Remove rows with promo = 1
train = dataset_old[which(dataset_old$promo == 0),]

# Remove promo variable
train$promo = NULL

# Change planType
train$planType = as.factor(train$planType)
score$planType = as.factor(score$planType)

# Gender to binomial
train$gender = (train$gender == "F")*1
score$gender = (score$gender == "F")*1

# Replace NAs with highest possible value
train$timeSinceLastIsOverData[is.na(train$timeSinceLastIsOverData)] = 117
train$minutesVoice[is.na(train$minutesVoice)] = 0
train$timeSinceLastIsOverVoice[is.na(train$timeSinceLastIsOverVoice)] = 117
train$timeSinceLastComplaints[is.na(train$timeSinceLastComplaints)] = 117
train$timeSinceLastTechProb[is.na(train$timeSinceLastTechProb)] = 117
train$phonePrice[is.na(train$phonePrice)] = 0
train$phoneBalance[is.na(train$phoneBalance)] = 0

score$timeSinceLastIsOverData[is.na(score$timeSinceLastIsOverData)] = 117
score$minutesVoice[is.na(score$minutesVoice)] = 0
score$timeSinceLastIsOverVoice[is.na(score$timeSinceLastIsOverVoice)] = 117
score$timeSinceLastComplaints[is.na(score$timeSinceLastComplaints)] = 117
score$timeSinceLastTechProb[is.na(score$timeSinceLastTechProb)] = 117
score$phonePrice[is.na(score$phonePrice)] = 0
score$phoneBalance[is.na(score$phoneBalance)] = 0

# Scaling the relevant variables
scaled.train = train
scaled.train[c(5,9,10,11,12,14,
               15,16,17,19,20,21,22,
               23,24,25,26,27,28)] <- lapply(scaled.train[c(5,9,10,11,
                                                            12,14,15,16,17,
                                                            19,20,21,22,23,24,25,
                                                            26,27,28)],
                                             function(x) c(scale(x)))

scaled.score = score
scaled.score[c(5,9,10,11,12,14,
               15,16,17,19,20,21,22,
               23,24,25,26,27,28)] <- lapply(scaled.score[c(5,9,10,11,
                                                            12,14,15,16,17,
                                                            19,20,21,22,23,24,25,
                                                            26,27,28)],
                                             function(x) c(scale(x)))


# Divide in train and test
test = scaled.train[540000:677933,]
train = scaled.train[0:540000,]

# Models
model1 = glm(churnIn3Month~nbAdultAvg+age+gender+
               isWorkPhone+planType+data+dataAvgConsumption+
               nbrIsOverData+nbrIsOverVoice+nbrComplaints+nbrTechnicalProblems+
               voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
               baseMonthlyRateForPhone+baseMonthlyRateForPlan+
               timeSinceLastComplaints+timeSinceLastTechProb+timeSinceLastIsOverData+
               timeSinceLastIsOverVoice+minutesVoice+unlimitedVoice+textoAvgConsumption+
               unlimitedText+lifeTime+chrono+IDfamily+ID,
             family = "binomial", data = train)
summary(model1)

# Dropping variables with NAs coefficient to prevent warning message
model2 = glm(churnIn3Month~nbAdultAvg+age+gender+
               isWorkPhone+planType+data+dataAvgConsumption+
               nbrIsOverData+nbrIsOverVoice+nbrComplaints+nbrTechnicalProblems+
               voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
               baseMonthlyRateForPhone+baseMonthlyRateForPlan+
               timeSinceLastComplaints+timeSinceLastTechProb+timeSinceLastIsOverData+
               timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
               lifeTime,
             family = "binomial", data = train)
summary(model1)
#Testing with a third model where I removed variables that are less significant but
# results are worse than model 2
model3 = glm(churnIn3Month~nbAdultAvg+age+gender+
               isWorkPhone+planType+dataAvgConsumption+
               nbrComplaints+nbrTechnicalProblems+
               phonePrice+cashDown+phoneBalance+
               baseMonthlyRateForPhone+
               timeSinceLastComplaints+timeSinceLastTechProb+timeSinceLastIsOverData+
               minutesVoice+textoAvgConsumption+
               lifeTime,
             family = "binomial", data = train)
summary(model3)

# Doing the prediction with test set and comparing models. Final model is after cutoff optimisation
prob.predict.test=predict.glm(model2,test,type="response")

# Setting a cutoff and looking at error rates to find best model
cutoff=0.03
test.pred = rep(0, nrow(test))
test.pred[prob.predict.test > cutoff] = 1
M=table(test.pred, test$churnIn3Month,dnn=c("Prediction","Observation"))
M
a=M[1,1]
b=M[1,2]
c=M[2,1]
d=M[2,2]

# Taux de mauvaise classification
(b+c)/(a+b+c+d)

# Taux faux negatifs
b/(a+b+c+d)

# Sensibilitee
d/(b+d)

# Specificitee
a/(a+c)

# Taux faux positifs
c/(a+b+c+d)


# Finding cutoff with ROCR
library(ROCR)
pred=prediction(prob.predict.test,test$churnIn3Month )
perf=performance(pred,measure="tpr",x.measure="fpr")
str(perf)

# False positive rate
length(perf@x.values)

head(perf@x.values[[1]])

# Cutoffs
length(perf@alpha.values)

head(perf@alpha.values[[1]])

cutoffs=data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]],
                   tpr=perf@y.values[[1]])
head(cutoffs)

plot(perf)
abline(a=0,b=1)

# Finding best cutoff
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.15))

opt.cut = function(perf, pred)
{
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(True_positive_rate = y[[ind]], False_positive_rate = x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(perf, pred))


# Predicting for score dataset and writing to csv file
# Retraining with the whole dataset (scaled.train)
modelF = glm(churnIn3Month~nbAdultAvg+age+gender+
               isWorkPhone+planType+data+dataAvgConsumption+
               nbrIsOverData+nbrIsOverVoice+nbrComplaints+nbrTechnicalProblems+
               voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
               baseMonthlyRateForPhone+baseMonthlyRateForPlan+
               timeSinceLastComplaints+timeSinceLastTechProb+timeSinceLastIsOverData+
               timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
               lifeTime,
             family = "binomial", data = scaled.train)
# Prediction for score
prob.predict.score=predict.glm(modelF,scaled.score,type="response")

# creating score_predict DF
score_predict = scaled.score
score_predict$prediction = prob.predict.score

# Keeping families of 1
score_familyof1 = score_predict[which(score_predict$nbAdultAvg == 1),]

# Creating DF with only the relevant variables: IDfamily and prediction
score_familyof1_less= score_familyof1$IDfamily
score_familyof1_less = data.frame(score_familyof1_less)
colnames(score_familyof1_less)[1] = "IDfamily"
score_familyof1_less$prediction = score_familyof1$prediction

# Creating expected value column
score_familyof1_less$exp_value = score_familyof1$prediction*score_familyof1$baseMonthlyRateForPlan

# Ordering and keeping the first 8000 families
familyof1_invitations = score_familyof1_less[order(score_familyof1_less$exp_value, decreasing = TRUE),]
familyof1_invitations_cvs = head(familyof1_invitations$IDfamily, n = 8000)

# Creating CSV file
write.table(familyof1_invitations_cvs, file = "invitation.familyof1.24.10.18h50.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

