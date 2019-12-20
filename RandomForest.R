library(glmnet)
rm(list=ls())
dataset_old = read.csv("Retention-train_fixed.csv")
score = read.csv("Retention-score-fixed.csv")

train = dataset_old

# Change planType
train$planType = as.factor(train$planType)
train$unlimitedVoice = as.factor(train$unlimitedVoice)
train$unlimitedText = as.factor(train$unlimitedText)
train$churnIn3Month = as.factor(train$churnIn3Month)
train$isWorkPhone = as.factor(train$isWorkPhone)
score$planType = as.factor(score$planType)
score$unlimitedVoice = as.factor(score$unlimitedVoice)
score$unlimitedText = as.factor(score$unlimitedText)
score$isWorkPhone = as.factor(score$isWorkPhone)


# Gender to binomial
train$gender = (train$gender == "F")*1
train$gender = as.factor(train$gender)
score$gender = (score$gender == "F")*1
score$gender = as.factor(score$gender)

# Replace NAs with highest observed value of each variable
train$timeSinceLastIsOverData[is.na(train$timeSinceLastIsOverData)] = 108
train$minutesVoice[is.na(train$minutesVoice)] = 0
train$timeSinceLastIsOverVoice[is.na(train$timeSinceLastIsOverVoice)] = 54
train$timeSinceLastComplaints[is.na(train$timeSinceLastComplaints)] = 116
train$timeSinceLastTechProb[is.na(train$timeSinceLastTechProb)] = 115
train$phonePrice[is.na(train$phonePrice)] = 0
train$phoneBalance[is.na(train$phoneBalance)] = 0
train$cashDown[is.na(train$cashDown)] = 0
train$nbrComplaints[which(train$nbrComplaints != 0)] = 1
train$nbrIsOverData[which(train$nbrIsOverData != 0)] = 1
train$nbrTechnicalProblems[which(train$nbrTechnicalProblems != 0)] = 1
train$nbrIsOverVoice[which(train$nbrIsOverVoice != 0)] = 1


score$timeSinceLastIsOverData[is.na(score$timeSinceLastIsOverData)] = 111
score$minutesVoice[is.na(score$minutesVoice)] = 0
score$timeSinceLastIsOverVoice[is.na(score$timeSinceLastIsOverVoice)] = 57
score$timeSinceLastComplaints[is.na(score$timeSinceLastComplaints)] = 119
score$timeSinceLastTechProb[is.na(score$timeSinceLastTechProb)] = 118
score$phonePrice[is.na(score$phonePrice)] = 0
score$phoneBalance[is.na(score$phoneBalance)] = 0
score$cashDown[is.na(score$cashDown)] = 0
score$nbrComplaints[which(score$nbrComplaints != 0)] = 1
score$nbrIsOverData[which(score$nbrIsOverData != 0)] = 1
score$nbrTechnicalProblems[which(score$nbrTechnicalProblems != 0)] = 1
score$nbrIsOverVoice[which(score$nbrIsOverVoice != 0)] = 1


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
#test = scaled.train[540000:677933,]
#train = scaled.train[0:540000,]

# Divide train in two groups (treatment & control) in order to apply uplift
treatment = train[which(train$promo == 1),]
treatment = subset(treatment, select=-c(promo))

control = train[which(train$promo == 0),]
control = subset(control, select=-c(promo))

# Strategies for rare classes
# Under sampling
nbre.select_control=round(.30*sum(control$churnIn3Month ==0))
index.sampling_control=sample((1:nrow(control))[control$churnIn3Month==0],nbre.select_control)

control_under=rbind(control[control$churnIn3Month==1,],control[index.sampling_control,])

nbre.select_treatment=round(.30*sum(treatment$churnIn3Month ==0))
index.sampling_treatment=sample((1:nrow(treatment))[treatment$churnIn3Month==0],nbre.select_treatment)

treatment_under=rbind(treatment[treatment$churnIn3Month==1,],treatment[index.sampling_treatment,])


# Models
# Uplift without under-sampling
reg_treatment = glm(churnIn3Month~nbAdultAvg+age+gender+
                            isWorkPhone+planType+data+dataAvgConsumption+
                            nbrIsOverData+nbrIsOverVoice+nbrComplaints+nbrTechnicalProblems+
                            voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
                            baseMonthlyRateForPhone+baseMonthlyRateForPlan+
                            timeSinceLastComplaints+timeSinceLastTechProb+timeSinceLastIsOverData+
                            timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
                            lifeTime,
                          family = "binomial", data = treatment)

reg_control = glm(churnIn3Month~nbAdultAvg+age+gender+
                          isWorkPhone+planType+data+dataAvgConsumption+
                          nbrIsOverData+nbrIsOverVoice+nbrComplaints+nbrTechnicalProblems+
                          voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
                          baseMonthlyRateForPhone+baseMonthlyRateForPlan+
                          timeSinceLastComplaints+timeSinceLastTechProb+timeSinceLastIsOverData+
                          timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
                          lifeTime,
                        family = "binomial", data = control)
# Uplift with under-sampling
reg_treatment_under = glm(churnIn3Month~nbAdultAvg+age+gender+
               isWorkPhone+planType+data+dataAvgConsumption+
               nbrIsOverData+nbrIsOverVoice+nbrComplaints+nbrTechnicalProblems+
               voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
               baseMonthlyRateForPhone+baseMonthlyRateForPlan+
               timeSinceLastComplaints+timeSinceLastTechProb+timeSinceLastIsOverData+
               timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
               lifeTime,
             family = "binomial", data = treatment_under)

reg_control_under = glm(churnIn3Month~nbAdultAvg+age+gender+
               isWorkPhone+planType+data+dataAvgConsumption+
               nbrIsOverData+nbrIsOverVoice+nbrComplaints+nbrTechnicalProblems+
               voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
               baseMonthlyRateForPhone+baseMonthlyRateForPlan+
               timeSinceLastComplaints+timeSinceLastTechProb+timeSinceLastIsOverData+
               timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
               lifeTime,
             family = "binomial", data = control_under)


#Predictions on score dataset (change reg_treatment_under for reg_treatment if no under-sampling)
score$prob_treatment = predict.glm(reg_treatment_under,score,type="response")
score$prob_control = predict.glm(reg_control_under,score,type="response")
score$incrementalScore = score$prob_treatment - score$prob_control
persuadables = score[which(score$incrementalScore < 0),]


persuadables$incrementalScore = persuadables$incrementalScore * (-1)
persuadables$exp_value = persuadables$incrementalScore * persuadables$baseMonthlyRateForPlan
persuadables = persuadables[which(persuadables$nbAdultAvg == 1),]
persuadables_less = data.frame("IDfamily" = persuadables$IDfamily,
                               "exp_value" = persuadables$exp_value)

invitations = persuadables_less[order(persuadables_less$exp_value, decreasing = TRUE),]
invitations_cvs = head(invitations$IDfamily, n = 8000)

write.table(invitations_cvs, file = "reglog.under.14.11.16h00.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

# Doing the prediction with test set and comparing models. Final model is after cutoff optimisation
prob.predict.test = predict.glm(reg_treatment,test,type="response")

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


# Reg log with no Uplift and no under sampling
# Retraining with the whole dataset (scaled.train)
classic = glm(churnIn3Month~nbAdultAvg+age+gender+
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
write.table(familyof1_invitations_cvs, file = "classic.13.11.19h40.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

# Classification tree
library(rpart)
mytree = rpart(churnIn3Month~., data=scaled.train, method = "class")

# Graphing the tree gives you a one-leafed tree
library(rpart.plot)
prp(mytree,extra=1,roundint=FALSE)


# Over sampling does not work since dataset is too big
library(DMwR)
train_over = train
train_over$churnIn3Month = as.factor(train_over$churnIn3Month)
train_over.smote=SMOTE(churnIn3Month~nbAdultAvg+age+gender+
                         isWorkPhone+planType+data+dataAvgConsumption+
                         nbrIsOverData+nbrIsOverVoice+nbrComplaints+
                         nbrTechnicalProblems+
                         voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
                         baseMonthlyRateForPhone+baseMonthlyRateForPlan+
                         timeSinceLastComplaints+timeSinceLastTechProb+
                         timeSinceLastIsOverData+
                         timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
                         lifeTime,data=train_over, perc.over = 200,
                       perc.under=500,k = 5)
model_over = glm(churnIn3Month~nbAdultAvg+age+gender+
                   isWorkPhone+planType+data+dataAvgConsumption+
                   nbrIsOverData+nbrIsOverVoice+nbrComplaints+
                   nbrTechnicalProblems+
                   voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
                   baseMonthlyRateForPhone+baseMonthlyRateForPlan+
                   timeSinceLastComplaints+timeSinceLastTechProb+
                   timeSinceLastIsOverData+
                   timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
                   lifeTime,data=train_over.smote,family="binomial")

# SVM
library (e1071)
mysvm =svm(churnIn3Month~nbAdultAvg+age+gender+
             isWorkPhone+planType+data+dataAvgConsumption+
             nbrIsOverData+nbrIsOverVoice+nbrComplaints+
             nbrTechnicalProblems+
             voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
             baseMonthlyRateForPhone+baseMonthlyRateForPlan+
             timeSinceLastComplaints+timeSinceLastTechProb+
             timeSinceLastIsOverData+
             timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
             lifeTime, data=train , kernel ="linear", cost =10,scale =FALSE )


# Random forest
library(randomForest)
rf_treatment_under = randomForest(churnIn3Month~nbAdultAvg+age+gender+
                  isWorkPhone+planType+data+dataAvgConsumption+
                  nbrIsOverData+nbrIsOverVoice+nbrComplaints+
                  nbrTechnicalProblems+
                  voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
                  baseMonthlyRateForPhone+baseMonthlyRateForPlan+
                  timeSinceLastComplaints+timeSinceLastTechProb+
                  timeSinceLastIsOverData+
                  timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
                  lifeTime,data=treatment_under,ntree=100,mtry=10)


rf_control_under = randomForest(churnIn3Month~nbAdultAvg+age+gender+
                  isWorkPhone+planType+data+dataAvgConsumption+
                  nbrIsOverData+nbrIsOverVoice+nbrComplaints+
                  nbrTechnicalProblems+
                  voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
                  baseMonthlyRateForPhone+baseMonthlyRateForPlan+
                  timeSinceLastComplaints+timeSinceLastTechProb+
                  timeSinceLastIsOverData+
                  timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
                  lifeTime,data=control_under,ntree=100,mtry=10)

score.rf_under = score[which(score$nbAdultAvg == 1),]
prob.treatment_under = predict(rf_treatment_under,score,type="prob")
prob.control_under = predict(rf_control_under,score,type="prob")
score.rf_under$prob_treatment = prob.treatment_under[,2]
score.rf_under$prob_control = prob.control_under[,2]
score.rf_under$incrementalScore = score.rf_under$prob_control - score.rf_under$prob_treatment


predrf=predict(rf,newdata=scaled.score)
predrf = data.frame(predrf)
predrf$IDfamily = scaled.score$IDfamily
predrf$value = scaled.score$baseMonthlyRateForPlan
predrf = predrf[which(predrf$predrf == 1),]

pred.rf_under = score.rf_under[order(score.rf_under$incrementalScore, decreasing = TRUE),]
predrf_invitation = head(pred.rf_under$IDfamily, n = 8000)

write.table(predrf_invitation, file = "rf.under.14.11.15h52.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")


# Boosting on random forest (too long)
library(adabag)
myboost=boosting(churnIn3Month~nbAdultAvg+age+gender+
                   isWorkPhone+planType+data+dataAvgConsumption+
                   nbrIsOverData+nbrIsOverVoice+nbrComplaints+
                   nbrTechnicalProblems+
                   voiceAvgConsumption+phonePrice+cashDown+phoneBalance+
                   baseMonthlyRateForPhone+u+
                   timeSinceLastComplaints+timeSinceLastTechProb+
                   timeSinceLastIsOverData+
                   timeSinceLastIsOverVoice+minutesVoice+textoAvgConsumption+
                   lifeTime, data=scaled.train, mfinal = 50,
                 coeflearn = 'Freund', control=rpart.control(maxdepth=10))
