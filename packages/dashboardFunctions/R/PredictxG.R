#' Function that adds xG for the shots
#' reads the model file first
#' @param df shot data
#' @keywords predict
#' @export
#' @examples
#' PredictxG(df)
#' @export

PredictxG <- function(df_for_pred){
  df_for_pred$Bodypart[is.na(df_for_pred$Bodypart)] <- "RightFoot"
  testm <- sparse.model.matrix(Goal~.-1, data = df_for_pred)
  test_label <- df_for_pred[,"Goal"] 
  print(length(test_label))
  # test_label <- test_label %>% ifelse(test_label==-1,0,1)
  print(length(df_for_pred$Goal))
  print("################ nc ##############")
  #nc <- length(unique(test_label))
  
  labels <- as.matrix(df_for_pred$Goal)
  #labels <- df_for_pred$Goal
  length(as.matrix(df_for_pred$Goal))
  length(df_for_pred$Goal)
  nrow(df_for_pred)
  print(dim(testm))
  test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = labels)
  
  bst_model2.0 <- xgb.load("xgb.model")
  p <- predict(bst_model2.0, newdata = test_matrix)
  pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
    t() %>%
    data.frame() %>%
    mutate(#label = test_label,
           max_prob = max.col(., "last")-1)
  head(pred)
  #table(Prediction = pred$max_prob, Actual = pred$label)
  table(Prediction = pred$max_prob, Actual = pred$max_prob)
  
  
  length(p)
  
  
  p <- as.data.frame(p)
  p<-p[ !c(TRUE,FALSE), ] 
  
  return(p)
}
