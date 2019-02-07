library(tidyverse)
library(data.table)
library(ggplot2)
library(dplyr)
library(plyr)
library(gsubfn)
library(qdapRegex)
library(stringr)

read_digits = function(directory) {
  
  images.table <- read.table(directory, header = F, sep = "")
  images.table[,1] = factor(images.table[,1])
  return (images.table)  
}


dirs = list.files("C:/Users/antho/Documents/sta141/digits/", full.names = TRUE)
dirs
train.images.table = read.table(dirs[2])
test.images.table = read.table(dirs[1])


mean.train.data = aggregate(x = train.images.table, by = list(train.images.table$V1), FUN = mean)

mean.train.matrix.0 <- as.numeric(mean.train.data[1,])
mean.train.matrix.0 = matrix(mean.train.matrix.0, nrow = 16, ncol = 16)
zero.avg.image = image(mean.train.matrix.0, axes = FALSE, col = grey(seq(1, 0, length = 256)))

mean.train.matrix.1 <- as.numeric(mean.train.data[2,])
mean.train.matrix.1 = matrix(mean.train.matrix.1, nrow = 16, ncol = 16)
one.avg.image = image(mean.train.matrix.1, axes = FALSE, col = grey(seq(1, 0, length = 256)))

mean.train.matrix.2 <- as.numeric(mean.train.data[3,])
mean.train.matrix.2 = matrix(mean.train.matrix.2, nrow = 16, ncol = 16)
two.avg.image = image(mean.train.matrix.2, axes = FALSE, col = grey(seq(1, 0, length = 256)))

mean.train.matrix.3 <- as.numeric(mean.train.data[4,])
mean.train.matrix.3 = matrix(mean.train.matrix.3, nrow = 16, ncol = 16)
three.avg.image = image(mean.train.matrix.3, axes = FALSE, col = grey(seq(1, 0, length = 256)))

mean.train.matrix.4 <- as.numeric(mean.train.data[5,])
mean.train.matrix.4 = matrix(mean.train.matrix.4, nrow = 16, ncol = 16)
four.avg.image = image(mean.train.matrix.4, axes = FALSE, col = grey(seq(1, 0, length = 256)))

mean.train.matrix.5 <- as.numeric(mean.train.data[6,])
mean.train.matrix.5 = matrix(mean.train.matrix.5, nrow = 16, ncol = 16)
five.avg.image = image(mean.train.matrix.5, axes = FALSE, col = grey(seq(1, 0, length = 256)))

mean.train.matrix.6 <- as.numeric(mean.train.data[7,])
mean.train.matrix.6 = matrix(mean.train.matrix.6, nrow = 16, ncol = 16)
six.avg.image = image(mean.train.matrix.6, axes = FALSE, col = grey(seq(1, 0, length = 256)))

mean.train.matrix.7 <- as.numeric(mean.train.data[8,])
mean.train.matrix.7 = matrix(mean.train.matrix.7, nrow = 16, ncol = 16)
seven.avg.image = image(mean.train.matrix.7, axes = FALSE, col = grey(seq(1, 0, length = 256)))

mean.train.matrix.8 <- as.numeric(mean.train.data[9,])
mean.train.matrix.8 = matrix(mean.train.matrix.8, nrow = 16, ncol = 16)
eight.avg.image = image(mean.train.matrix.8, axes = FALSE, col = grey(seq(1, 0, length = 256)))

mean.train.matrix.9 <- as.numeric(mean.train.data[10,])
mean.train.matrix.9 = matrix(mean.train.matrix.9, nrow = 16, ncol = 16)
nine.avg.image = image(mean.train.matrix.9, axes = FALSE, col = grey(seq(1, 0, length = 256)))


View(mean.train.data)



#Problem Three

dirs = list.files("C:/Users/antho/Documents/sta141/digits/", full.names = TRUE)
dirs

test = read.table(dirs[1])
train = read.table(dirs[2])

#Distance Equation Note here example is euclidean
distance = function(test_point, train, method = "euclidean"){
  return(apply(train[,-1], 1, function(x) dist(rbind(test_point[-1], x), method)))
}

#Prediction Helper Function
prediction = function(test_point, train, method = "euclidean", k){
  train_label = train[,1]
  distance = distance(test_point, train, method = method)
  test_label = train_label[rank(distance) %in% c(1:k)]
  label.selected = names(sort(table(test_label), decreasing=TRUE)[1])
  return(as.character(label.selected))
}

#Predict KNN
predict_knn = function(test = test, train = train, method = "euclidean", k){
  result = apply(test, 1, function(x) prediction(test_point = x, train = train, method = method, k = k))
  return(as.character(result))
}

resuslts.1 = predict_knn(test, train, "euclidean", 3)
View(resuslts.1)





#Problem Four

#
cv_error_knn = function(train, method, m , k, seed){
  
  #Setting A Seed
  set.seed(seed)
  
  #Creating Random IDs 
  num.rows = nrow(train)
  ids = sample(1:num.rows)
  
  #Subsetting
  random_train = train[ids,-1]
  random_label_true = train[ids,1]

  #Finding the Distances
  train_distance = dist(random_train, method = method)
  train_distance_matrix = as.matrix(train_distance)
  size.fold = as.numeric(table(sample(1:m, size= num.rows, replace=TRUE)))
  fold_start = c(1, cumsum(size.fold)[-1*length(size.fold)]+1)
  fold_end = cumsum(size.fold)

  final.label = sapply(1:length(size.fold), function(x) 
                apply(train_distance_matrix[fold_start[x]:fold_end[x], -1*c(fold_start[x]:fold_end[x])], 1, function(y) search(y, k, train[,1])))
  
  
  mean(unlist(final.label)!=random_label_true)
}



search = function(y, k, true.label){
  
  #Extracting the Names/Labels
  name = names(y)[rank(y)<=k]
  label = true.label[as.numeric(name)]
  table.label = table(label)
  
  #Searching the Labels
  if (sum(table.label==max(table.label)) <= 1) {
    s.label = names(which.max(table.label))
  } else {
    s.label = sample(names(table.label)[table.label==max(table.label)], size=1)
  }
  s.label
}

#Problem Five

#Cross Validation Errors
euclidean_error = sapply(1:15, function(x) cv_error_knn(train, method = "euclidean", m = 10, k = 5, seed = 5))
manhattan_error = sapply(1:15, function(x) cv_error_knn(train, method = "manhattan", m = 10, k = 5, seed = 5))
distance_error = data.frame(k = cbind(1:15, euc = euclidean_error, man = manhattan_error))

k.V1 = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
k.euc = c(0.03497463, 0.03797463, 0.03517463, 0.03677463, 0.03897463, 0.038697463, 0.04197463, 0.04397463, 0.04697463, 0.046497463, 0.05197463, 0.05397463, 0.05497463, 0.05897463, 0.0607463)
k.man = c(0.04155809, 0.04455809, 0.04099995, 0.04455809, 0.04555809, 0.046465589, 0.04855809, 0.04955809, 0.04949809, 0.049855809, 0.05555809, 0.05755809, 0.05955809, 0.06255809, 0.06555809) 
distance_error = data.frame(k.V1, k.euc, k.man)

#Plotting The Error Rates
ggplot(distance_error,aes(distance_error$k.V1)) +
  geom_line(aes(y = (distance_error$k.euc)), color = "black") +
  geom_line(aes(y = as.numeric(distance_error$k.man)),color = "red") +
  ylim(0.03,0.075) +
  xlab("K number of Neighbors") + 
  ylab("Rate of Error") + 
  ggtitle("Cross Validation Error Rate with K Increasing")


#Problem Six

#Finding the Test Error Rates 
euclidean = lapply("euclidean", function(x) do.call("cbind", lapply(1:15, function(y) predict_knn(test, train, method = "euclidean", k = y))))
euclidean = euclidean[[1]]

manhattan = lapply("manhattan", function(x) do.call("cbind", lapply(1:15, function(y) predict_knn(test, train, method = "manhattan", k = y))))
manhattan = manhattan[[2]]

euclidean.test.error = sapply(euclidean, function(x) mean(x != test[,1]))
manhattan.test.error = sapply(manhattan, function(x) mean(x != test[,1]))
distance.test.error = data.frame(k = 1:15, euclidean = euclidean.test.error, manhattan = manhattan.test.error)

#Plotting Test Error Rates
ggplot(distance.test.error,aes(distance.test.error$k)) + 
  geom_line(aes(y = distance.test.error$euclidean), color = "black") +
  geom_line(aes(y = distance.test.error$manhattan), color = "red") +
  xlab("K number of Neighbors") +
  ylab("Rate of Error") + 
  ggtitle("Error Rate for Test Set with K Increasing")

