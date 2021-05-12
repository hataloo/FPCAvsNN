pcs_to_include = c(5, 10, 20, 30, 60, 90, 120, 150, 180)

accuracy <- array(dim = length(pcs_to_include))
iter <- 1
for(included_pc in pcs_to_include){
  tail_distance <- array(dim = c(60000,10))
  tail_classification <- array(dim = c(60000))
  for(i in 1:60000){
    for(class_index in 1:10){
      tail_distance[i,class_index] <- sum(principal_coeffs_array[i,class_index,included_pc:(dim(principal_coeffs_array)[3])]^2)
    }
    tail_classification[i] <- which.min(tail_distance[i,])
  }
  accuracy[iter] <- 100*mean((tail_classification-1) == train_labels)
  print(sprintf("%d included PCs: %0.2f percent", included_pc, accuracy[iter]))
  iter <- iter + 1
}

### Testing the PC with best training accuracy on the test data.
best_pc <- pcs_to_include[which.max(accuracy)]

test_distance <- array(dim = c(length(test_labels), 10))
test_classification <- array(dim = c(length(test_labels)))
for(i in 1:length(test_labels)){
  for(class_index in 1:10){
    test_distance[i, class_index] <- sum(test_principal_coeffs_array[i,class_index,best_pc:(dim(test_principal_coeffs_array)[3])]^2)
  }
  test_classification[i] <- which.min(test_distance[i,])
}
test_accuracy <- 100*mean((test_classification-1) == test_labels)
print(sprintf("%d included PCs: %0.2f percent", best_pc, test_accuracy))