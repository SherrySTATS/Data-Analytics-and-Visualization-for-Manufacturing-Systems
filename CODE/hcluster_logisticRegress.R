#################critical station analysis
load(".../Rdata/dtUniqPath.RData")
load(".../Rdata/dtNum.RData")
rareRate = sum(dtNum$Response)/dim(dtNum)[1] #0.58%
rareRateUniq = sum(dtUniqPath$Response)/dim(dtUniqPath)[1] #12.89%
require(ggplot2)
ggplot(dtUniqPath, aes(x=Response)) + geom_histogram(bins = 2)

#require(DMwR)
#dtBalanceUniqPath = SMOTE(Response~., data=dtUniqPath, perc.over = 300, perc.under = 100)

install.packages("randomForest")
require(randomForest)
set.seed(100)
smp_size <- floor(0.75*nrow(dtUniqPath))
train_ind <- sample(seq_len(nrow(dtUniqPath)), size = smp_size)
train <- dtUniqPath[train_ind,]
test <- dtUniqPath[-train_ind,]
attach(train)
rdffit = randomForest(as.factor(Response)~., data = train)
rdd_pre = predict(rdffit, test)

failure = train[which(Response==1),]
# Ward Hierarchical Clustering
d <- dist(t(failure[,1:50]), method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit, main = "Failure Dateset Cluster Dendrogram",xlab = "Distance", ylab = "Height") # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

Success = train[which(Response==0),]
d <- dist(t(Success[,1:50]), method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit, main = "Successed Dateset Cluster Dendrogram",xlab = "Distance", ylab = "Height") # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
#cut off the tree
successclusterCut = cutree(fit,5)
table(successclusterCut)
###trying to monitor the cluster change


###logistic regression
x = train[,1:50]
y = train$Response
require(glmnet)
x = as.matrix(x)
y = as.factor(y)
lmod = glmnet(x, y, family = "binomial")
lmod_cv = cv.glmnet(x, y, family = "binomial")
plot(lmod_cv)
testX = as.matrix(test[,1:50])
y_predict = predict(lmod,testX)
min_id = which(lmod_cv$lambda==lmod_cv$lambda.min)
y_hat = exp(testX%*%lmod_cv$glmnet.fit$beta[,min_id])/exp(1+testX%*%lmod_cv$glmnet.fit$beta[,min_id])
y_hat = y_hat>0.2

require(mltools)
testY = test$Response
logistic_MCC = mcc(as.double(y_hat), test$Response)
logistic_MCC

###Random Forest
require(randomForest)
set.seed(100)
uniq_rf <- randomForest(Response ~ ., data=train, importance=TRUE,
                        proximity=TRUE)
## Look at variable importance:
round(importance(uniq_rf), 2)
#prediction
y_rf_hat <- predict(uniq_rf,test[,1:50])>0.1 && predict(uniq_rf,test[,1:50])<0.2
rf_MCC = mcc(as.double(y_rf_hat), test$Response)
rf_MCC
