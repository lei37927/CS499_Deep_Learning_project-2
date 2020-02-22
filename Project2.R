## install package if it is not already.
if(!require("data.table")){
  install.packages("data.table")
}

## attach all functions provided by these packages.
library(data.table)
library(ggplot2)

## KFoldCV Algorithm
KFoldCV <- function(X_mat, y_vec, ComputePredictions, fold_vec){
  # print(fold_vec)
  error_vec <- array(0,dim=max(unique(fold_vec)))
  for(k in unique(fold_vec)){
    ## split into train/test sets.
    is.validation <- fold_vec == k
    is.train <- !is.validation
    X_new <- X_mat[is.validation, ]
    y_new <- y_vec[is.validation]
    X_train <- X_mat[is.train, ]
    y_train <- y_vec[is.train]
    pred_new <- ComputePredictions(X_train,y_train,X_new)
    is.error <- pred_new != y_new
    error_vec[k] <- mean(is.error)
  }
  error_vec
}

## NearestNeighborsCV Algorithm
NearestNeighborsCV <- function(X_mat, y_vec, 
                               X_new, num_folds=5, 
                               max_neighbors=20){
  # print(nrow(X_mat))
  validation_fold_vec <- sample(rep(1:num_folds, l=nrow(X_mat)))
  # validation_fold_vec
  error_mat <- matrix(0,nrow=num_folds, ncol=max_neighbors)
  for(num_neighbor in 1:max_neighbors){
    ComputePreditions <- function(X_train,y_train,X_new){class::knn(X_train,X_new,y_train,k=num_neighbor)}
    error_mat[, num_neighbor] <- KFoldCV(X_mat, y_vec, ComputePreditions, validation_fold_vec)
  }
  mean_error_vec <- colMeans(error_mat)
  best_neighbors <- which.min(mean_error_vec)
  y_pred <- class::knn(X_mat, X_new, y_vec, 
                       k=best_neighbors)
  list(y_pred, mean_error_vec, error_mat)
}

# download spam data set to local directory, if it is not present.
if(!file.exists("spam.data")){
  download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

## Read spam data set and conver to X matrix and y vector we need for
## gradient descent.
spam.dt <- data.table::fread("spam.data")
N.obs <- nrow(spam.dt)
X.raw <- as.matrix(spam.dt[, -ncol(spam.dt), with=FALSE]) 
y.vec <- spam.dt[[ncol(spam.dt)]]
X.sc <- scale(X.raw) #scaled X/feature/input matrix.

## compute and visualize validation error as a function of number of
## neighbors.
X_new = matrix(0, nrow = 1, ncol = ncol(X.sc))
result <- NearestNeighborsCV(X.sc, y.vec, X_new)
df <- data.frame(neighbors = 1:lengths(result[2]),
                 mean_validation_error = unlist(result[2], use.names=FALSE),
                 stringsAsFactors=FALSE)

if(!require("dplyr")){
  install.packages("dplyr")
}
library(dplyr) 
highlight_df <- df %>% filter(mean_validation_error == min(mean_validation_error))

df %>% 
  ggplot(aes(x=neighbors,y=mean_validation_error)) + 
  geom_line(data=df, aes(neighbors, mean_validation_error))  +
  geom_point(data=highlight_df, aes(neighbors, mean_validation_error), color='red',size=3)

test_fold_vec = validation_fold_vec <- sample(rep(1:4, l=nrow(X.raw)))
# counts with a row for each fold (1/2/3/4) and a column for each class (0/1)
for (i in 1:4){
  ds <- y.vec[test_fold_vec == i]
  print("sub-dataset: ")
  print(i)
  print("count for 0")
  print(length(ds) - sum(ds))
  print("count for 1")
  print(sum(ds))
}

# # Use KFoldCV with three algorithms
err_mat = matrix(0, nrow = 4, ncol = 3)
for (k in 1:4){
  is.test <- test_fold_vec == k
  is.train <- !is.test
  X_test <- X.sc[is.test, ]
  y_test <- y.vec[is.test]
  X_train <- X.sc[is.train, ]
  y_train <- y.vec[is.train]
  # (1) baseline/underfit ¨C predict most frequent class
  freq <- 0
  if (sum(y_test) > length(y_test)/2) freq <- 1
  y_pred1 <- array(freq,dim=length(y_test))
  # (2) NearestNeighborsCV
  y_pred2 <- NearestNeighborsCV(X_train, y_train, X_test)[1]
  y_pred2 <- array(as.numeric(unlist(y_pred2))) - 1
  # (3) overfit 1-nearest neighbors model
  y_pred3 <- class::knn(X_train, X_test, y_train, k=1)
  err_mat[k, 1] <- mean(y_pred1 != y_test)
  err_mat[k, 2] <- mean(y_pred2 != y_test)
  err_mat[k, 3] <- mean(y_pred3 != y_test)
}
err_mat

xdf0 <- data.frame(test_set = 1:4, baseline = err_mat[,1], NearestNeighborsCV = err_mat[,2], overfit = err_mat[,3])

xdf0.melt <- reshape2::melt(xdf0, id.vars="test_set")
ggplot(data = xdf0.melt, aes(x=test_set, y=value)) + geom_line(aes(colour=variable))
ggplot(data = xdf0.melt, aes(x=test_set, y=value)) + geom_point(aes(colour=variable))
