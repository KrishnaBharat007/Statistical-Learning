#Setup ----
#Clear Environment Variables
rm(list = ls())

#Setup working directory to path where file is present
path <- 'E:/Data_Science_Masters_Program/Statistical_Learning/Home_Assignment_2/'
setwd(path)
filename = 'Data_Cortex_Nuclear.xls'
library(readxl)
library(tree)
library(e1071)
library(randomForest)
library(gbm)
library(purrr)
library(cluster)
library(factoextra)

#Load excel file into data frame and analyse structure and summary
df <- data.frame(read_excel(paste0(path,filename)))
str(df)
summary(df)
names(df)

# Make copy of actual dataset
df1 <- df[c(-1)]

# Check for NA values
for (col in names(df1)){ print(paste(col,':',sum(is.na(df1[col])))) }

# Check for distinct value counts in each column
for (col in names(df1)){ print(paste(col,':',length(table(df1[col])))) }

# Split dataframe into seperate groups based on class column
df_split <-split.data.frame(x = df1,f = df1$class)

# Replace NA Values with median of each column in each class
df1_cleansed <- data.frame()
# Looping to update all protein columns with median and column bind to add class columns at the end 
for (df_class in df_split) {
  temp_df <- data.frame(1:c(nrow(df_class)))
  for (col in df_class[c(-78:-81)]){
    temp_col <- col
    temp_col[is.na(temp_col)] <- median(temp_col,na.rm = T)
    temp_df <- cbind(temp_df,temp_col)
  }
  temp_df <- cbind(temp_df,df_class[c(78:81)])
  df1_cleansed <- rbind(df1_cleansed,temp_df[c(-1)])
}
colnames(df1_cleansed) <- names(df1)
shuffled_df <- df1_cleansed[sample(1:nrow(df1_cleansed)),]
sum(is.na(shuffled_df))
col_names <- names(shuffled_df)[1:77]

# Question 1-a: ----
dataset1 <- shuffled_df[c(-79:-81)]
dataset2 <- shuffled_df[c(-78,-80,-81)]
dataset3 <- shuffled_df[c(-78,-79,-81)]
dataset4 <- shuffled_df[c(-78:-80)]

dataset1$Genotype <- factor(dataset1$Genotype)
dataset2$Treatment <- factor(dataset2$Treatment)
dataset3$Behavior <- factor(dataset3$Behavior)
dataset4$class <- factor(dataset4$class)

# Binary Classification - Genotype
set.seed(1)
train <- sample(1:nrow(dataset1),nrow(dataset1)*0.8)
train_set <- dataset1[train,]
test_set <- dataset1[-train,]

# Trees
tree_model <- tree(Genotype~.,train_set)
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_pred <- predict(tree_model,test_set,type = 'class')
table(tree_pred,test_set$Genotype)
cv_dataset1 <- cv.tree(tree_model)
plot(cv_dataset1$size, cv_dataset1$dev,type = 'b')
tree.min <- which.min(cv_dataset1$dev)
points(cv_dataset1$size[tree.min], cv_dataset1$dev[tree.min], col = "red", cex = 2, pch = 20)
prune_model <- prune.tree(tree_model, best = cv_dataset1$size[tree.min])
plot(prune_model)
text(prune_model, pretty = 0)
prune_pred <- predict(prune_model,test_set,type = 'class')
table(prune_pred,test_set$Genotype)

#SVM
classifier = svm(formula = Genotype ~ ., data = train_set, type = 'C-classification', kernel = 'linear')
summary(classifier)
y_pred = predict(classifier, newdata = test_set[-78])
table(y_pred,test_set[, 78])
tune_model <- tune(svm,Genotype~.,data = train_set,kernel = 'linear', ranges = list (cost = c (0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_model)
tune_classifier <- svm(formula = Genotype ~ ., data = train_set, type = 'C-classification', kernel = 'linear',cost = 0.1)
summary(tune_classifier)
y_pred_tune = predict(tune_classifier, newdata = test_set[-78])
table(y_pred_tune,test_set[, 78])

# Binary Classification - Treatment
set.seed(1)
train <- sample(1:nrow(dataset2),nrow(dataset2)*0.8)
train_set <- dataset2[train,]
test_set <- dataset2[-train,]

# Trees
tree_model <- tree(Treatment~.,train_set)
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_pred <- predict(tree_model,test_set,type = 'class')
table(tree_pred,test_set$Treatment)
cv_dataset2 <- cv.tree(tree_model)
plot(cv_dataset2$size, cv_dataset2$dev,type = 'b')
tree.min <- which.min(cv_dataset2$dev)
points(cv_dataset2$size[tree.min], cv_dataset2$dev[tree.min], col = "red", cex = 2, pch = 20)
prune_model <- prune.tree(tree_model, best = cv_dataset2$size[tree.min])
plot(prune_model)
text(prune_model, pretty = 0)
prune_pred <- predict(prune_model,test_set,type = 'class')
table(prune_pred,test_set$Treatment)

# SVM
classifier = svm(formula = Treatment ~ ., data = train_set, type = 'C-classification', kernel = 'linear')
summary(classifier)
y_pred = predict(classifier, newdata = test_set[-78])
table(y_pred,test_set[, 78])
tune_model <- tune(svm,Treatment~.,data = train_set,kernel = 'linear', ranges = list (cost = c (0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_model)
tune_classifier <- svm(formula = Treatment ~ ., data = train_set, type = 'C-classification', kernel = 'linear',cost = 0.1)
summary(tune_classifier)
y_pred_tune = predict(tune_classifier, newdata = test_set[-78])
table(y_pred_tune,test_set[, 78])

# Binary Classification - Behavior
set.seed(1)
train <- sample(1:nrow(dataset3),nrow(dataset3)*0.8)
train_set <- dataset3[train,]
test_set <- dataset3[-train,]

# Trees
tree_model <- tree(Behavior~.,train_set)
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_pred <- predict(tree_model,test_set,type = 'class')
table(tree_pred,test_set$Behavior)
cv_dataset3 <- cv.tree(tree_model)
plot(cv_dataset3$size, cv_dataset3$dev,type = 'b')
tree.min <- which.min(cv_dataset3$dev)
points(cv_dataset3$size[tree.min], cv_dataset3$dev[tree.min], col = "red", cex = 2, pch = 20)
prune_model <- prune.tree(tree_model, best = cv_dataset3$size[tree.min])
plot(prune_model)
text(prune_model, pretty = 0)
prune_pred <- predict(prune_model,test_set,type = 'class')
table(prune_pred,test_set$Behavior)

# SVM
classifier = svm(formula = Behavior ~ ., data = train_set, type = 'C-classification', kernel = 'linear')
summary(classifier)
y_pred = predict(classifier, newdata = test_set[-78])
table(y_pred, test_set[, 78])

# Binary Classification - Class
set.seed(1)
train <- sample(1:nrow(dataset4),nrow(dataset4)*0.8)
train_set <- dataset4[train,]
test_set <- dataset4[-train,]

# Trees
tree_model <- tree(class~.,train_set)
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_pred <- predict(tree_model,test_set,type = 'class')
table(tree_pred,test_set$class)
cv_dataset4 <- cv.tree(tree_model)
plot(cv_dataset4$size, cv_dataset4$dev,type = 'b')
tree.min <- which.min(cv_dataset4$dev)
points(cv_dataset4$size[tree.min], cv_dataset4$dev[tree.min], col = "red", cex = 2, pch = 20)
prune_model <- prune.tree(tree_model, best = cv_dataset4$size[tree.min])
plot(prune_model)
text(prune_model, pretty = 0)
prune_pred <- predict(prune_model,test_set,type = 'class')
table(prune_pred,test_set$class)

# SVM
classifier = svm(formula = class ~ ., data = train_set, type = 'C-classification', kernel = 'linear')
summary(classifier)
y_pred = predict(classifier, newdata = test_set[-78])
table(y_pred, test_set[, 78])
tune_model <- tune(svm,class~.,data = train_set,kernel = 'linear', ranges = list (cost = c (0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_model)
tune_classifier <- svm(formula = class ~ ., data = train_set, type = 'C-classification', kernel = 'linear',cost = 0.1)
summary(tune_classifier)
y_pred_tune = predict(tune_classifier, newdata = test_set[-78])
table(y_pred_tune,test_set[, 78])


# Question 1-b ----
apply(shuffled_df[1:77],2,mean)
apply(shuffled_df[1:77],2,var)
dataset_pca <- prcomp(shuffled_df[1:77] , scale = TRUE)
summary(dataset_pca)
pr.var <- dataset_pca$sdev^2
pve <- pr.var / sum (pr.var)
plot(pve , xlab = " Principal Component ", ylab = " Proportion of Variance Explained ", ylim = c(0, 1), type = "b")
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0, 1), type = "b")
abline(h=0.95, col="red")

pca <- as.data.frame(dataset_pca$x[,1:30])
pca_final <- cbind(pca,shuffled_df[78:81])

dataset1 <- pca_final[c(-32:-34)]
dataset2 <- pca_final[c(-31,-33,-34)]
dataset3 <- pca_final[c(-31,-32,-34)]
dataset4 <- pca_final[c(-31:-33)]

dataset1$Genotype <- factor(dataset1$Genotype)
dataset2$Treatment <- factor(dataset2$Treatment)
dataset3$Behavior <- factor(dataset3$Behavior)
dataset4$class <- factor(dataset4$class)

# Binary Classification - Genotype
set.seed(1)
train <- sample(1:nrow(dataset1),nrow(dataset1)*0.8)
train_set <- dataset1[train,]
test_set <- dataset1[-train,]

# Trees
tree_model <- tree(Genotype~.,train_set)
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_pred <- predict(tree_model,test_set,type = 'class')
table(tree_pred,test_set$Genotype)
cv_dataset1 <- cv.tree(tree_model)
plot(cv_dataset1$size, cv_dataset1$dev,type = 'b')
tree.min <- which.min(cv_dataset1$dev)
points(cv_dataset1$size[tree.min], cv_dataset1$dev[tree.min], col = "red", cex = 2, pch = 20)
prune_model <- prune.tree(tree_model, best = cv_dataset1$size[tree.min])
plot(prune_model)
text(prune_model, pretty = 0)
prune_pred <- predict(prune_model,test_set,type = 'class')
table(prune_pred,test_set$Genotype)

#SVM
classifier = svm(formula = Genotype ~ ., data = train_set, type = 'C-classification', kernel = 'linear')
summary(classifier)
y_pred = predict(classifier, newdata = test_set[-31])
table(y_pred,test_set[, 31])
tune_model <- tune(svm,Genotype~.,data = train_set,kernel = 'linear', ranges = list (cost = c (0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_model)
tune_classifier <- svm(formula = Genotype ~ ., data = train_set, type = 'C-classification', kernel = 'linear',cost = 0.1)
summary(tune_classifier)
y_pred_tune = predict(tune_classifier, newdata = test_set[-31])
table(y_pred_tune,test_set[, 31])

# Binary Classification - Treatment
set.seed(1)
train <- sample(1:nrow(dataset2),nrow(dataset2)*0.8)
train_set <- dataset2[train,]
test_set <- dataset2[-train,]

# Trees
tree_model <- tree(Treatment~.,train_set)
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_pred <- predict(tree_model,test_set,type = 'class')
table(tree_pred,test_set$Treatment)
cv_dataset2 <- cv.tree(tree_model)
plot(cv_dataset2$size, cv_dataset2$dev,type = 'b')
tree.min <- which.min(cv_dataset2$dev)
points(cv_dataset2$size[tree.min], cv_dataset2$dev[tree.min], col = "red", cex = 2, pch = 20)
prune_model <- prune.tree(tree_model, best = cv_dataset2$size[tree.min])
plot(prune_model)
text(prune_model, pretty = 0)
prune_pred <- predict(prune_model,test_set,type = 'class')
table(prune_pred,test_set$Treatment)

# SVM
classifier = svm(formula = Treatment ~ ., data = train_set, type = 'C-classification', kernel = 'linear')
summary(classifier)
y_pred = predict(classifier, newdata = test_set[-31])
table(y_pred,test_set[, 31])
tune_model <- tune(svm,Treatment~.,data = train_set,kernel = 'linear', ranges = list (cost = c (0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_model)
tune_classifier <- svm(formula = Treatment ~ ., data = train_set, type = 'C-classification', kernel = 'linear',cost = 0.1)
summary(tune_classifier)
y_pred_tune = predict(tune_classifier, newdata = test_set[-31])
table(y_pred_tune,test_set[, 31])

# Binary Classification - Behavior
set.seed(1)
train <- sample(1:nrow(dataset3),nrow(dataset3)*0.8)
train_set <- dataset3[train,]
test_set <- dataset3[-train,]

# Trees
tree_model <- tree(Behavior~.,train_set)
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_pred <- predict(tree_model,test_set,type = 'class')
table(tree_pred,test_set$Behavior)
cv_dataset3 <- cv.tree(tree_model)
plot(cv_dataset3$size, cv_dataset3$dev,type = 'b')
tree.min <- which.min(cv_dataset3$dev)
points(cv_dataset3$size[tree.min], cv_dataset3$dev[tree.min], col = "red", cex = 2, pch = 20)
prune_model <- prune.tree(tree_model, best = cv_dataset3$size[tree.min])
plot(prune_model)
text(prune_model, pretty = 0)
prune_pred <- predict(prune_model,test_set,type = 'class')
table(prune_pred,test_set$Behavior)

# SVM
classifier = svm(formula = Behavior ~ ., data = train_set, type = 'C-classification', kernel = 'linear')
summary(classifier)
y_pred = predict(classifier, newdata = test_set[-31])
table(y_pred,test_set[, 31])
tune_model <- tune(svm,Behavior~.,data = train_set,kernel = 'linear', ranges = list (cost = c (0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_model)
tune_classifier <- svm(formula = Behavior ~ ., data = train_set, type = 'C-classification', kernel = 'linear',cost = 0.1)
summary(tune_classifier)
y_pred_tune = predict(tune_classifier, newdata = test_set[-31])
table(y_pred_tune,test_set[, 31])

# Binary Classification - Class
set.seed(1)
train <- sample(1:nrow(dataset4),nrow(dataset4)*0.8)
train_set <- dataset4[train,]
test_set <- dataset4[-train,]

# Trees
tree_model <- tree(class~.,train_set)
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_pred <- predict(tree_model,test_set,type = 'class')
table(tree_pred,test_set$class)
cv_dataset4 <- cv.tree(tree_model)
plot(cv_dataset4$size, cv_dataset4$dev,type = 'b')
tree.min <- which.min(cv_dataset4$dev)
points(cv_dataset4$size[tree.min], cv_dataset4$dev[tree.min], col = "red", cex = 2, pch = 20)
prune_model <- prune.tree(tree_model, best = cv_dataset4$size[tree.min])
plot(prune_model)
text(prune_model, pretty = 0)
prune_pred <- predict(prune_model,test_set,type = 'class')
table(prune_pred,test_set$class)

# SVM
classifier = svm(formula = class ~ ., data = train_set, type = 'C-classification', kernel = 'linear')
summary(classifier)
y_pred = predict(classifier, newdata = test_set[-31])
table(y_pred,test_set[, 31])
tune_model <- tune(svm,class~.,data = train_set,kernel = 'linear', ranges = list (cost = c (0.01, 0.1, 1, 5, 10, 100,1000)))
summary(tune_model)
tune_classifier <- svm(formula = class ~ ., data = train_set, type = 'C-classification', kernel = 'linear',cost = 0.1)
summary(tune_classifier)
y_pred_tune = predict(tune_classifier, newdata = test_set[-31])
table(y_pred_tune,test_set[, 31])

# Question 1-c: ----
dataset1 <- shuffled_df[c(-79:-81)]
dataset2 <- shuffled_df[c(-78,-80,-81)]
dataset3 <- shuffled_df[c(-78,-79,-81)]
dataset4 <- shuffled_df[c(-78:-80)]

dataset1$Genotype <- factor(ifelse(dataset1$Genotype == 'Control',0,1))
dataset2$Treatment <- factor(ifelse(dataset2$Treatment == 'Memantine',0,1))
dataset3$Behavior <- factor(ifelse(dataset3$Behavior == 'C/S',0,1))
dataset4$class <- factor(dataset4$class, labels = c(0:7),
                         levels = c('c-CS-m', 'c-CS-s', 'c-SC-m', 'c-SC-s', 
                                    't-CS-m', 't-CS-s', 't-SC-m', 't-SC-s'))

# Binary Classification - Genotype
set.seed(1)
train <- sample(1:nrow(dataset1),nrow(dataset1)*0.8)
train_set <- dataset1[train,]
test_set <- dataset1[-train,]

# Bagging
bag_model <- randomForest(Genotype ~ ., data = train_set, mtry = 77,ntree = 1000)
bag_model
bag_pred <- predict(bag_model, newdata = test_set)
table(bag_pred,test_set$Genotype)

# Random Forest
rf_model <- randomForest(Genotype ~ ., data = train_set, mtry = round(sqrt(77)),ntree = 1000)
rf_model
rf_pred <- predict(rf_model, newdata = test_set)
table(rf_pred,test_set$Genotype)

# Boosting
dataset1 <- shuffled_df[c(-79:-81)]
dataset1$Genotype <- ifelse(dataset1$Genotype == 'Control',0,1)
set.seed(1)
train <- sample(1:nrow(dataset1),nrow(dataset1)*0.8)
train_set <- dataset1[train,]
test_set <- dataset1[-train,]
boost_model <- gbm(Genotype ~ ., data = train_set, distribution = "bernoulli", n.trees = 1000)
boost_model
boost_probs <- predict(boost_model, newdata = test_set, type='response')
boost_pred <- ifelse(boost_probs >0.5,1,0)
table(boost_pred,test_set$Genotype)

# Binary Classification - Treatment
set.seed(1)
train <- sample(1:nrow(dataset2),nrow(dataset2)*0.8)
train_set <- dataset2[train,]
test_set <- dataset2[-train,]

# Bagging
bag_model <- randomForest(Treatment ~ ., data = train_set, mtry = 77,ntree = 1000)
bag_model
bag_pred <- predict(bag_model, newdata = test_set)
table(bag_pred,test_set$Treatment)

# Random Forest
rf_model <- randomForest(Treatment ~ ., data = train_set, mtry = round(sqrt(77)),ntree = 1000)
rf_model
rf_pred <- predict(rf_model, newdata = test_set)
table(rf_pred,test_set$Treatment)

# Boosting
dataset2 <- shuffled_df[c(-78,-80,-81)]
dataset2$Treatment <- ifelse(dataset2$Treatment == 'Memantine',0,1)
set.seed(1)
train <- sample(1:nrow(dataset2),nrow(dataset2)*0.8)
train_set <- dataset2[train,]
test_set <- dataset2[-train,]
boost_model <- gbm(Treatment ~ ., data = train_set, distribution = "bernoulli", n.trees = 5000)
boost_model
boost_probs <- predict(boost_model, newdata = test_set, type='response')
boost_pred <- ifelse(boost_probs >0.5,1,0)
table(boost_pred,test_set$Treatment)

# Binary Classification - Behavior
set.seed(1)
train <- sample(1:nrow(dataset3),nrow(dataset3)*0.8)
train_set <- dataset3[train,]
test_set <- dataset3[-train,]

# Bagging
bag_model <- randomForest(Behavior ~ ., data = train_set, mtry = 77,ntree = 1000)
bag_model
bag_pred <- predict(bag_model, newdata = test_set)
table(bag_pred,test_set$Behavior)

# Random Forest
rf_model <- randomForest(Behavior ~ ., data = train_set, mtry = round(sqrt(77)),ntree = 1000)
rf_model
rf_pred <- predict(rf_model, newdata = test_set)
table(rf_pred,test_set$Behavior)

# Boosting
dataset3 <- shuffled_df[c(-78,-79,-81)]
dataset3$Behavior <- ifelse(dataset3$Behavior == 'C/S',0,1)
set.seed(1)
train <- sample(1:nrow(dataset3),nrow(dataset3)*0.8)
train_set <- dataset3[train,]
test_set <- dataset3[-train,]
boost_model <- gbm(Behavior ~ ., data = train_set, distribution = "bernoulli", n.trees = 5000)
boost_model
boost_probs <- predict(boost_model, newdata = test_set, type='response')
boost_pred <- ifelse(boost_probs >0.5,1,0)
table(boost_pred,test_set$Behavior)

# Binary Classification - Class
set.seed(1)
train <- sample(1:nrow(dataset4),nrow(dataset4)*0.8)
train_set <- dataset4[train,]
test_set <- dataset4[-train,]

# Bagging
bag_model <- randomForest(class ~ ., data = train_set, mtry = 77,ntree = 1000)
bag_model
bag_pred <- predict(bag_model, newdata = test_set)
table(bag_pred,test_set$class)

# Random Forest
rf_model <- randomForest(class ~ ., data = train_set, mtry = round(sqrt(77)),ntree = 1000)
rf_model
rf_pred <- predict(rf_model, newdata = test_set)
table(rf_pred,test_set$class)

# Boosting
dataset4 <- shuffled_df[c(-78:-80)]
dataset4$class <- factor(dataset4$class, labels = c(0:7),
                          levels = c('c-CS-m', 'c-CS-s', 'c-SC-m', 'c-SC-s',
                                   't-CS-m', 't-CS-s', 't-SC-m', 't-SC-s'))
dataset4$class <- as.integer(dataset4$class)
set.seed(1)
train <- sample(1:nrow(dataset4),nrow(dataset4)*0.8)
train_set <- dataset4[train,]
test_set <- dataset4[-train,]
boost_model <- gbm(class ~ ., data = train_set, distribution = "multinomial", n.trees = 1000)
boost_model
boost_probs <- predict(boost_model, newdata = test_set, type='response')
probs=as.matrix(boost_probs[,,1])
boost_pred <- apply(probs, 1, which.max)
table(boost_pred,test_set$class)

#Question 2 ----
df_proteins <- shuffled_df[1:77]

# Kmeans Clustering
fviz_nbclust(df_proteins, kmeans, method = "wss") + labs(subtitle = 'Elbow Method')
km_model <- kmeans(df_proteins,centers = 3,nstart = 10,iter.max =  1000)
print(km_model)
fviz_cluster(list(data = df_proteins,cluster = km_model$cluster))
cluster <- km_model$cluster
kmeans_dataset <- data.frame(shuffled_df[78:81],cluster)
kmeans_dataset_split <- split.data.frame(kmeans_dataset,f = cluster)

for (cluster in kmeans_dataset_split) {
  print('Cluster')
  for (col in cluster) { print(table(col)) }
}

# Hierarchical Clustering
hc_complete <- hclust(dist(df_proteins), method = "average")
plot(hc_complete)
hc_complete <- hclust(dist(df_proteins), method = "single")
plot(hc_complete)
hc_complete <- hclust(dist(df_proteins), method = "complete")
plot(hc_complete)
abline(h = 6,col = 'red')