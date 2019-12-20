rm(list=ls())
library(dplyr)
dataset_old = read.csv("Retention-train_fixed.csv")
score_old = read.csv("Retention-score-fixed.csv")

train = dataset_old
score = score_old

# Factorize
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
# score$nbrComplaints[which(score$nbrComplaints != 0)] = 1
# score$nbrIsOverData[which(score$nbrIsOverData != 0)] = 1
# score$nbrTechnicalProblems[which(score$nbrTechnicalProblems != 0)] = 1
# score$nbrIsOverVoice[which(score$nbrIsOverVoice != 0)] = 1

# Standardization but it does not change the cluster so we use the unstandardized data
score.scaled = scale(score[,c(5,9,10,11,12,14,
                              15,16,17,19,20,21,22,
                              23,24,25,26,27,28)], center=TRUE, scale=TRUE)
score.scaled = data.frame(score.scaled)

# We keep a percentage of the dataset to speed up the computations
score_small = score %>% sample_frac(.01)

# Computing euclidian distances
d = dist(score_small[, c("dataAvgConsumption","voiceAvgConsumption",
             "textoAvgConsumption")], method = "euclidean")

d_complaint = dist(score_small[, c("nbrComplaints","nbrTechnicalProblems")],
                   method = "euclidean")

d_nbrOver = dist(score_small[, c("nbrIsOverData","nbrIsOverVoice")],
                 method = "euclidean")

d_all = dist(score_small[, c("nbrIsOverData","nbrIsOverVoice",
                              "dataAvgConsumption","voiceAvgConsumption",
                              "textoAvgConsumption","nbrComplaints",
                              "nbrTechnicalProblems")],
              method = "euclidean")

# Creating clusters
fit = hclust(d, method="complete")
fit_complaint = hclust(d_complaint, method="complete")
fit_nbrOver = hclust(d_nbrOver, method="complete")
fit_all = hclust(d_all, method="complete")

groups = cutree(fit, k=3)
groups_complaint = cutree(fit_complaint, k=3)
groups_nbrOver = cutree(fit_nbrOver, k=3)
groups_all = cutree(fit_all, k=2)

# Tree
fit$merge[1:100,]
plot(fit)
plot(fit, hang = -1, cex = 0.6)

# Find the number of groups
plot(7977:1,fit$height)
plot(7977:1,fit_complaint$height)
plot(7977:1,fit_nbrOver$height)
plot(7977:1,fit_test$height)

# Bind with dataset
score_group_all = cbind(score_small, groups_all)

# K-means
result=kmeans(score_small[, c("nbrIsOverData","nbrIsOverVoice",
                              "dataAvgConsumption","voiceAvgConsumption",
                              "textoAvgConsumption","nbrComplaints",
                              "nbrTechnicalProblems")],5)
groups_km=result$cluster
table(groups_km)
#Bind
score_group_test = cbind(score_small, groups_km)

# Get summary for each group
summary(score_group_test[which(score_group_test$groups_km==5),])


# Trying other methods
fit2 = hclust(d, method="single")
groups2=cutree(fit2, k=5)
table(groups2)

fit3 = hclust(d, method="average")
groups3=cutree(fit3, k=5)
table(groups3)

# Factor analysis that did not yield good results
factanal(score_small[, c(9,10,12,14,
                         15,17,19,20,21,22,
                         23,24,25,26,27,28)], 3)

