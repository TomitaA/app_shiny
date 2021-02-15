library(tidyverse)
library(Matrix)
library(xgboost)

load_and_predict <- function(data){
  
  colsKey <- "shop_code"
  
  dataFolder <- "sub_file"
  filePaths <- list.files(dataFolder, recursive = TRUE, full.names = TRUE, pattern = 'util')
  
  colsUsed <- readRDS(filePaths)
  
  test.keys <- data %>% dplyr::select(one_of(c(colsKey, 'shop_name', 'todohuken')))
  data.test <- data %>% dplyr::select(colsUsed)
  
  previous_na_action <- options()$na.action
  options(na.action="na.pass")
  
  data.test.matrix <- Matrix::sparse.model.matrix(~ ., data = data.test)
  test <- xgboost::xgb.DMatrix(
    data.test.matrix
  )
  rm(data.test.matrix)
  
  options(na.action=previous_na_action)
  
  
  dataFolder <- "models"
  filePaths <- list.files(dataFolder, recursive = TRUE, full.names = TRUE, pattern = 'model')
  
  pred_model_test <- tibble()
  for (i in 1:length(filePaths)) {
    model_tmp <- xgboost::xgb.load(filePaths[[i]])
    pred_col_name <- paste0('pred_', i)
    predict_tmp <- test.keys %>% mutate(!!pred_col_name := predict(model_tmp, test))
    if (i == 1) {
      pred_model_test <- predict_tmp
    } else {
      pred_model_test <- pred_model_test %>% left_join(predict_tmp %>% dplyr::select(-shop_name, -todohuken), by = colsKey)
    }
  }
  
  pred_model_test <- pred_model_test %>% dplyr::select(-contains('pred')) %>% 
    mutate(
      Predict_Sales = pred_model_test %>% dplyr::select(contains('pred')) %>% apply(MARGIN = 1, FUN = median),
      Predict_Sales = exp(Predict_Sales),
      Monthly_Avg_Predict_Sales = Predict_Sales / 12)
  
  return(pred_model_test)
}