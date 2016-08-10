# r_bio
This library is written in R.
It enable to integrate any package for Grid Search and Cross Validation.

## Usage Example
### Addmodel
```R
models <- add_model(models,svm,"SVM",
                 parameter_list <- alist(formula=Y~.,
                   data=data.frame(X,Y)),
                 search_param <- list(
                   gamma=c(2^-4,2^-2,2^0,2^1,2^3),
                   cost=c(2^-4,2^-2,2^0,2^1,2^3)))
```

### Cross Validation
```R
score <- cross_validation(function(X,Y){lm(Y~.,data.frame(X,Y))},
                           iris$Sepal.Length,iris$Petal.Width)
```

### Grid Search All
```R
score <- grid_search_all(models,iris$Sepal.Length,
                          iris$Petal.Width)
```
