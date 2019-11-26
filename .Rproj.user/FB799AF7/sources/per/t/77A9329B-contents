#author:yaxin yu
#Apply binary classification using decision tree and K-NN techniques

install.packages("party")
install.packages("class")
set.seed(45)

#3.1 Load the preprocessed data file from Task 1 into a data frame.
ILPD_Classification <- readRDS("./Data/ilpd_preprocessed.Rda")

#3.1 Divide the dataset into “training” and “test” subsets randomly (70% and 30% respectively)
indices <- sample(2, nrow(ILPD_Classification),replace = TRUE, prob = c(0.7, 0.3))
training_data <- ILPD_Classification[indices == 1,]
testing_data <- ILPD_Classification[indices == 2,]

#3.2 Learn a classification tree from the training data using the default parameters of the function from the library. 
library(party)
ILPD_ctree <- ctree(Class~., data=training_data)
jpeg(filename="./Plot/task3.2_training_data.jpeg")
plot(ILPD_ctree)
dev.off()

#3.2 Using the learned tree, predict the class labels of the test data.
ILPD_prediction <- predict(ILPD_ctree, newdata=testing_data[,-11])
confusionMatrix <- as.matrix(table(testing_data[,11],ILPD_prediction))
accuracy_result <- sum(diag(confusionMatrix))/sum(confusionMatrix)
row_sum <- apply(confusionMatrix, 1, sum)
column_sum <- apply(confusionMatrix, 2, sum)
precision_result <- confusionMatrix[2,2]/column_sum
recall_result <- confusionMatrix[2,2]/row_sum
print(accuracy_result)
print(recall_result)
print(precision_result)

#3.3 Try building your classification tree again via the ctree function but using
#parameters that are different from the default settings. 
ILPD_control <- ctree_control(teststat = c("quad", "max"),
                              testtype = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
                              mincriterion = 0.6, minsplit = 30, minbucket = 5,
                              nresample = 9999, stump = FALSE, maxdepth = 4)
ILPD_ctree2 <- ctree(Class~., data=training_data, controls = ILPD_control)
jpeg(filename="./Plot/task3.3.jpeg")
plot(ILPD_ctree2)
dev.off()
ILPD_prediction2 <- predict(ILPD_ctree2, newdata=testing_data[,-11])
confusionMatrix2 <- as.matrix(table(testing_data[,11],ILPD_prediction2))
accuracy_result2 <- sum(diag(confusionMatrix2))/sum(confusionMatrix2)
row_sum2 <- apply(confusionMatrix2, 1, sum)
column_sum2 <- apply(confusionMatrix2, 2, sum)
precision_result2 <- confusionMatrix2[2,2]/column_sum2
recall_result2 <- confusionMatrix2[2,2]/row_sum2
print(accuracy_result2)
print(recall_result2)
print(precision_result2)

#3.4 
#Apply K-NN classification to predict the labels in the test subset and
#calculate the accuracy, precision and recall. Particularly, try different values
#of K (e.g. K = 1, 2, 3, 4, 5)
set.seed(45)

ILPD_knn <- readRDS("./Data/ilpd_preprocessed.Rda")
ILPD_knn$Gender <- as.numeric(ILPD_knn$Gender)
indexes <- sample(2, nrow(ILPD_knn),replace = TRUE, prob = c(0.7, 0.3))
training_data_knn <- ILPD_knn[indexes == 1,]
testing_data_knn <- ILPD_knn[indexes == 2,]

#k=1
library("class")
knn_prediction <- knn(training_data_knn[,-11], testing_data_knn[,-11], training_data_knn$Class, k=1, prob=TRUE)
knn_table <- table(testing_data_knn$Class,knn_prediction)
knn_matrix <- as.matrix(knn_table)
knn_accuracy <- sum(diag(knn_matrix))/sum(knn_matrix)
knn_row_sum <- apply(knn_matrix, 1, sum)
knn_column_sum <- apply(knn_matrix, 2, sum)
knn_precision_result <- knn_matrix[2,2]/knn_column_sum
knn_recall_result <- knn_matrix[2,2]/knn_row_sum
print(knn_accuracy)
print(knn_precision_result)
print(knn_recall_result)

#k=2
knn_prediction2 <- knn(training_data_knn[,-11], testing_data_knn[,-11], training_data_knn$Class, k=2, prob=TRUE)
knn_table2 <- table(testing_data_knn$Class,knn_prediction2)
knn_matrix2 <- as.matrix(knn_table2)
knn_accuracy2 <- sum(diag(knn_matrix2))/sum(knn_matrix2)
knn_row_sum2 <- apply(knn_matrix2, 1, sum)
knn_column_sum2 <- apply(knn_matrix2, 2, sum)
knn_precision_result2 <- knn_matrix2[2,2]/knn_column_sum2
knn_recall_result2 <- knn_matrix2[2,2]/knn_row_sum2
print(knn_accuracy2)
print(knn_precision_result2)
print(knn_recall_result2)
#k=3
knn_prediction3 <- knn(training_data_knn[,-11], testing_data_knn[,-11], training_data_knn$Class, k=3, prob=TRUE)
knn_table3 <- table(testing_data_knn$Class,knn_prediction3)
knn_matrix3 <- as.matrix(knn_table3)
knn_accuracy3 <- sum(diag(knn_matrix3))/sum(knn_matrix3)
knn_row_sum3 <- apply(knn_matrix3, 1, sum)
knn_column_sum3 <- apply(knn_matrix3, 2, sum)
knn_precision_result3 <- knn_matrix3[2,2]/knn_column_sum3
knn_recall_result3 <- knn_matrix3[2,2]/knn_row_sum3
print(knn_accuracy3)
print(knn_precision_result3)
print(knn_recall_result3)
#k=4
knn_prediction4 <- knn(training_data_knn[,-11], testing_data_knn[,-11], training_data_knn$Class, k=4, prob=TRUE)
knn_table4 <- table(testing_data_knn$Class,knn_prediction4)
knn_matrix4 <- as.matrix(knn_table4)
knn_accuracy4 <- sum(diag(knn_matrix4))/sum(knn_matrix4)
knn_row_sum4 <- apply(knn_matrix4, 1, sum)
knn_column_sum4 <- apply(knn_matrix4, 2, sum)
knn_precision_result4 <- knn_matrix4[2,2]/knn_column_sum4
knn_recall_result4 <- knn_matrix4[2,2]/knn_row_sum4
print(knn_accuracy4)
print(knn_precision_result4)
print(knn_recall_result4)
#k=5
knn_prediction5 <- knn(training_data_knn[,-11], testing_data_knn[,-11], training_data_knn$Class, k=5, prob=TRUE)
knn_table5 <- table(testing_data_knn$Class,knn_prediction5)
knn_matrix5 <- as.matrix(knn_table5)
knn_accuracy5 <- sum(diag(knn_matrix5))/sum(knn_matrix5)
knn_row_sum5 <- apply(knn_matrix5, 1, sum)
knn_column_sum5 <- apply(knn_matrix5, 2, sum)
knn_precision_result5 <- knn_matrix5[2,2]/knn_column_sum5
knn_recall_result5 <- knn_matrix5[2,2]/knn_row_sum5
print(knn_accuracy5)
print(knn_precision_result5)
print(knn_recall_result5)