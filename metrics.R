metrics <- function(cutoff, prob, ref, postarget, negtarget) 
{
  predict <- as.factor(ifelse(prob >= cutoff, postarget, negtarget))
  conf <- caret::confusionMatrix(predict , ref, positive = postarget)
  acc <- conf$overall[1]
  rec <- conf$byClass[1]
  prec <- conf$byClass[3]
  spec <- conf$byClass[2]
  mat <- t(as.matrix(c(rec , acc , prec, spec))) 
  colnames(mat) <- c("recall", "accuracy", "precicion", "specificity")
  return(mat)
}