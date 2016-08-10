library(magrittr)
library(stringr)
library(pracma)


# Add models function
#
# This function give model integration.
#
#
# @param models,method_func, model_name, parmeter_list,
#        search_param <- alist(), score_func <- mse,
#        predict_func=predicth
# @keyword model
# @export
# @examples
# models <- list()
#
# models <- add_model(models,svm,"SVM",
#                   parameter_list <- alist(formula=Y~.,
#                     data=data.frame(X,Y)),
#                   search_param <- list(
#                     gamma=c(2^-4,2^-2,2^0,2^1,2^3),
#                     cost=c(2^-4,2^-2,2^0,2^1,2^3)))
#
add_model <- function(models, method_func, model_name,
                      parameter_list, search_param=alist(),
                      score_func=mse, predict_func=predict) {
  
  tmp <- list(list(Method=method_func, Score=score_func,
                   Prediction=predict_func, Parameter=parameter_list,
                   Grid=search_param))
  names(tmp) <- model_name
  return(append(models, tmp))
}


#
# General Cross Validation Function
#
# This function give generall cross-validation.
#
#
# @param method, X, Y, k=5,score_func=mse,predict_func=predict,
#        random_state=0
# @keyword cross validation
# @export
# @values list object.
# For example,
# output---$Score(numeric list)
#        |-$Predict(numeric list)
#        |-$Test(numeric list)
#
# @examples
# score <- cross_validation(function(X,Y){lm(Y~.,data.frame(X,Y))},
#                           iris$Sepal.Length,iris$Petal.Width)
#
cross_validation <- function(method_func, X, Y,
                             k=5, score_func=mse,
                             predict_func=predict,
                             random_state=0) {
  set.seed(random_state)
  index <- rep(1: k, length=length(Y))[randperm(length(Y))]
  tmp_result.score <- c()
  tmp_result.pred <- c()
  tmp_result.test <- c()
  
  make_data <- function(X) return(data.frame(X))
  
  get_coldata <- function(data, col_list) {
    if (is.null(ncol(data))) {
      return(data[col_list])
    } else {
      return(data[col_list, ])
    }
  }
  
 
  for (target_index in 1: k) {
    train_idx <- which(index != target_index)
    test_idx <- which(index == target_index)
    train_X <- get_coldata(X, train_idx)
    train_Y <- get_coldata(Y, train_idx)
    test_X <- get_coldata(X, test_idx)
    test_Y <- get_coldata(Y, test_idx)
    
    predict_Y <-
      method_func(train_X, train_Y) %>%
      predict_func(make_data(test_X))
    
    
    tmp_result.score <- c(tmp_result.score,
                          list(score_func(test_Y, predict_Y)))
    tmp_result.pred <- c(tmp_result.pred, list(predict_Y))
    tmp_result.test <- c(tmp_result.test, list(test_Y))
    
  }
  
  result <- list(Score=tmp_result.score,
                 Predict=tmp_result.pred,
                 Test=tmp_result.test)
  
  return(result)
  
}

#
# General Grid Search Function
#
# This function give generall grid search function.
# If you give test data set, score is evaluation using
# Cross Validation.
# CAUTION: grid search parameters are allowed only NUMERIC
#
# @param models, X, Y, test_X=NULL, test_Y=NULL, k=5
# @keyword grid search
# @export
# @value nested list object.
# For example,
# output---Model1---[[1]]--$Correlation(numeric)
#        |        |   |----$Score(numeric list)
#        |        |   |----$Parameter(parameter's list)
#        |        |         |--PARAMETER1(vector)
#        |        |         |--PARAMETER2(vector)
#        |        |
#        |        |-[[2]]--$Correlation(numeric)
#        |
#        |--Model2---[[1]]
#
# @examples
# models <- list()
#
# models <- add_model(models,svm,"SVM",
#                   parameter_list <-
#                     alist(formula=Y~.,data=data.frame(X,Y)),
#                   search_param <- list(
#                     gamma=c(2^-4,2^-2,2^0,2^1,2^3),
#                     cost=c(2^-4,2^-2,2^0,2^1,2^3)))
#
# score <- grid_search_all(models,iris$Sepal.Length,
#                          iris$Petal.Width)
#
grid_search_all <- function(models, X, Y,
                            test_X=NULL, test_Y=NULL, k=5) {
  
  curry_function <- function(f, ...){
    return(function(X, Y){
      do.call(f, c(...))
    })
  }
  
  
  get_collist <- function(data, col_list) {
    if (ncol(data) == 1) {
      tmp <- list(data[col_list, ])
      names(tmp) <- names(data)
      return(tmp)
    }else{
      return(as.list(data[col_list, ]))
    }
  }
  
  if (length(models) == 0) {
    print("There is no classifier.")
    stop()
  }
  result <- list()
  
  for(i in 1: length(models)){
    model <- models[[i]]
    
    if(length(model$Grid) == 0){
      f <- curry_function(model$Method, model$Parameter)
      
      if(is.null(test_X) && is.null(test_Y)){
        tmp <-
          cross_validation(f, X, Y,
                           predict_func=model$Predict,
                           score_func=model$Score, k=5)
        
      }else{
        
        pred <- f(X, Y) %>%
          model$Prediction(data.frame(test_X))
        
        tmp <- list(Score=list(model$Score(pred, test_Y)),
                    Predict=list(pred),
                    Test=test_Y)
        
      }
      
      if(sd(unlist(tmp$Predict)) == 0){
        co <- NA
      }else{
        co <- cor(unlist(tmp$Test), unlist(tmp$Predict),
                  method="spearman")
      }
      tmp_result <- list(Correlation=co,
                         Score=tmp$Score, Parameter=NA) %>%
        list %>%
        list
      
      names(tmp_result) <- names(models)[i]
      result <- append(result, tmp_result)
      next
    }
    
    tmp_result <- list()
    grid_list <- expand.grid(model$Grid)
    for(k in 1: nrow(grid_list)){
      f <- curry_function(model$Method,
                          model$Parameter,
                          get_collist(grid_list, k))
      
      if(is.null(test_X) && is.null(test_Y)){
        tmp <-
          cross_validation(f, X, Y, predict_func=model$Predict,
                           score_func=model$Score, k=5)
        
      }else{
        
        pred <- f(X, Y) %>%
          model$Prediction(data.frame(test_X))
        
        tmp <- list(Score=list(model$Score(pred, test_Y)),
                    Predict=list(pred),
                    Test=test_Y)
      }
      if(sd(unlist(tmp$Predict)) == 0){
        co <- NA
      }else{
        co <- cor(unlist(tmp$Test), unlist(tmp$Predict),
                  method="spearman")
      }
      tmp_result <- list(Correlation=co,
                         Score=tmp$Score,
                         Parameter=get_collist(grid_list, k)) %>%
        list %>%
        append(tmp_result, .)
      
      
    }
    out <- list(tmp_result)
    names(out) <- names(models)[i]
    result <- append(result, out)
  }
  return(result)
  
}
