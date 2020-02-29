matrix_result <- function(matrix, model_name) {
  matrix_1 <- as.data.frame(t(as.matrix(matrix, what = "overall")))
  matrix_2 <- as.data.frame(t(as.matrix(matrix, what = "classes")))
  Model <- c(model_name)
  matrix_result <- cbind(Model, matrix_1, matrix_2)
  matrix_result <- matrix_result %>% select(Model, Accuracy, Sensitivity, Specificity, "Pos Pred Value")
  return(matrix_result)
}