library(kernlab)
data(spam)
str(spam[,1:5])
##Perform the Sampling
set.seed(3435)
trainIndicator=rbinom(4601,size=1,prob=0.5)
table(trainIndicator)

###Exploratory analysis
trainSpam=spam[trainIndicator==1,]
testSpam=spam[trainIndicator==0,]

names(trainSpam)
head(trainSpam)

table(trainSpam$type)

plot(trainSpam$capitalAve~trainSpam$type)

plot(log10(trainSpam$capitalAve+1)~trainSpam$type)  ### as there are lot of zeros, then no point to take log, so just add one to make sense to take log of values to get rough sense of data
### it is just exploratory, now is okay to take

plot(log10(trainSpam[,1:4]+1))

hCluster=hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

hClusterUpdated=hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

###Statistical prediction/modeling

trainSpam$numType=as.numeric(trainSpam$type)-1
costFunction=function(x,y) sum(x!=(y>0.5))
cvError=rep(NA,50)
library(boot)
for(i in 1:55){
        lmFomula=reformulate(names(trainSpam)[i],response = "numType")
        glmFit=glm(lmFomula,family = "binomial",data=trainSpam)
        cvError[i]=cv.glm(trainSpam,glmFit,costFunction,2)$delta[2]
}
##which predictor ha minimum cross-validated error
names(trainSpam)[which.min(cvError)]


####Get a measure of uncertainty[this is logistic regression classifier, then need to specify the cut off point]
##use the best model from the group
predictionModel=glm(numType~charDollar,family = "binomial",data=trainSpam)

#get prediction on the test set
predictionTest=predict(predictionModel,testSpam)
predictedSpam=rep("nonspam",dim(testSpam)[1])

##classify as 'spam' for those with prob>0.5
predictedSpam[predictionModel$fitted>0.5]="spam"

##classification model
table(predictedSpam,testSpam$type)

##Error rate
(61+458)/(1346+458+61+449)
