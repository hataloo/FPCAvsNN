train_distances <- array(dim = c(length(train_labels),length(class_names)))
test_distances <- array(dim = c(length(test_labels),length(class_names)))

train_classifications <- array(dim = c(length(train_labels)))
test_classifications <- array(dim = c(length(test_labels)))

norm_to_use <- function(x) sqrt(sum(x^2))

for(i in 1:length(train_labels)){
  for(class_index in 1:length(class_names)){
    train_distances[i,class_index] <- norm_to_use(train_vectors[i,] - class_means[class_index,])
  }
  train_classifications[i] <- which.min(train_distances[i,])
}
train_accuracy <- mean(train_labels == (train_classifications-1))

print(sprintf("Training accuracy: %0.2f percent", 100*train_accuracy))

for(i in 1:length(test_labels)){
  for(class_index in 1:length(class_names)){
    test_distances[i, class_index] <- norm_to_use(test_vectors[i,] - class_means[class_index,])
  }
  test_classifications[i] <- which.min(test_distances[i,])
}
test_accuracy <- mean(test_labels == (test_classifications-1))
print(sprintf("Test accuracy: %0.2f percent", 100*test_accuracy))
