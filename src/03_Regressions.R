###############################################################################
############################### 03_ REGRESSIONS ###############################
###############################################################################
#' Script to create the functions for running the required experiments

set.seed(33)

# Function for extracting best fit in an elastic net regression
get_best_result = function(caret_fit) 
{
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}



###############################################################################
############################# RIDGE REGRESSION ################################
###############################################################################
# Function for fitting ridge regression to whole season
fit_ridge <- function(team)
{
  x_train <- x_matrix(team)
  y_train <- y_vector(team)
  
  lambdas <- 10^seq(4, -4, by = -.1)
  fit <- glmnet(x_train, y_train, alpha = 0 , lambda = lambdas,standardize = FALSE)
  
  #Get optimal lambdas:
  cv_fit <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
  opt_lambda <- cv_fit$lambda.min
  
  
  # Predict on training(regular season)
  y_predicted <- predict(fit, s = opt_lambda, newx = x_train)
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((y_predicted - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  # Results df
  results <- data.frame(TEAM = team,
                        LAMBDA = opt_lambda,
                        R2 = rsq,
                        SST = sst,
                        SSE = sse,
                        RMSE = rmse
  )

  return(results)
}

################################################################################
######################### LASSO REGRESSION #####################################
################################################################################

fit_lasso <- function(team)
{
  x_train <- x_matrix(team)
  y_train <- y_vector(team)
  lambdas <- 10^seq(4, -4, by = -.1)
  fit <- glmnet(x_train, y_train, alpha = 1 , lambda = lambdas, standardize = FALSE)
  
  #Get optimal lambdas:
  cv_fit <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas)
  opt_lambda <- cv_fit$lambda.min
  
  
  # Predict on training(regular season)
  y_predicted <- predict(fit, s = opt_lambda, newx = x_train)
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((y_predicted - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  print(paste0("TEAM: ",team))
  print(paste0("Lambda: ",opt_lambda))
  print(paste0("R^2: ",rsq))
  print(paste0("sst: ",sst,"| sse: ",sse))
  
  results <- data.frame(TEAM = team,
                        LAMBDA = opt_lambda,
                        R2 = rsq,
                        SST = sst,
                        SSE = sse,
                        RMSE = rmse
  )
  return(results)
  
}

# Function for predicting playoffs with Ridge Regression 
ridge_playoffs <- function(team)
{
  
  x_train <- x_matrix(team)
  y_train <- y_vector(team)
  x_test <- x_matrix_playoffs(team)
  y_test <- y_vector_playoffs(team)
  
  lambdas <- 10^seq(4, -4, by = -.1)
  fit <- glmnet(x_train, y_train, alpha = 0 , lambda = lambdas,standardize = FALSE)
  
  #Get optimal lambdas:
  cv_fit <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
  opt_lambda <- cv_fit$lambda.min
  
  
  # Predict on training(regular season)
  y_predicted <- predict(fit, s = opt_lambda, newx = x_train)
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((y_predicted - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  #Predict on test (last 24 games)
  y_predicted_test <- predict(fit, s = opt_lambda, newx = x_test)
  sst_test <- sum((y_test - mean(y_test))^2)
  sse_test <- sum((y_predicted_test - y_test)^2)
  rsq_test <- 1 - (sse_test / sst_test)
  
  #calcular aciertos
  ac <- data.frame(y_test, y_predicted_test)
  ac$correct <- ifelse((ac$y_test * ac$X1) > 0,1,0)
  aciertos <- sum(ac$correct)
  
  results <- data.frame(TEAM = team,
                        LAMBDA = opt_lambda,
                        R2 = rsq,
                        SST = sst,
                        SSE = sse,
                        RMSE = rmse,
                        R2_os = rsq_test,
                        correct = aciertos,
                        juegos_pred = length(y_test)
  )
  results <- results %>% 
    mutate(accuracy = correct / juegos_pred)
  return(results)
  
  
}


###############################################################################
################# ELASTIC NET FOR FITTING A WHOLE SEASON ######################
###############################################################################

# Function for extracting best fit in an elastic net regression
get_best_result = function(caret_fit) 
{
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

fit_elastic <- function(team)
{
  x_train <- x_matrix(team)
  y_train <- y_vector(team)
  
  # Define train and test set
  train <- as.data.frame(cbind(x_train,y_train))
  
  # Set training control
  train_cont <- trainControl(method = "cv",
                             number = 10,
                             search = "random",
                             verboseIter = TRUE)
  
  # Train the model
  elastic_reg <- train(y_train ~ .,
                       data = train,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 10, #10 alpha values and 10 lambdas for each
                       trControl = train_cont)
  
  
  # Best tuning parameter
  best_e <- elastic_reg$bestTune
  params <- get_best_result(elastic_reg)
  
  # Make predictions on training set
  predictions_train <- predict(elastic_reg, x_train)
  
  
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((predictions_train - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  res <- data.frame(TEAM = team,
                    R2 = rsq,
                    SST = sst,
                    SSE = sse
  )
  
  results <- cbind(res,params) 
  
  return(results)
  
}


################################################################################
######################## ELASTIC NET FOR PLAYOFF PREDICTIONS ###################
################################################################################
# No transform playoffs elastic with manual alpha 
elastic_playoff_pred <- function(team)
{
  x_train <- x_matrix(team)
  y_train <- y_vector(team)
  x_test <- x_matrix_playoffs(team)
  y_test <- y_vector_playoffs(team)
  
  
  
  # Define train and test set
  train <- as.data.frame(cbind(x_train,y_train))
  
  # Set training control
  train_cont <- trainControl(method = "cv",
                             number = 10,
                             search = "random",
                             verboseIter = TRUE)
  
  # Train the model
  elastic_reg <- train(y_train ~ .,
                       data = train,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 10, #10 alpha values and 10 lambdas for each
                       trControl = train_cont)
  
  
  # Best tuning parameter
  best_e <- elastic_reg$bestTune
  params <- get_best_result(elastic_reg)
  
  # Make predictions on training set
  predictions_train <- predict(elastic_reg, x_train)
  #Predict on test (last 24 games)
  predictions_test <- predict(elastic_reg, x_test)
  
  # Metrics
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((predictions_train - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  sst_test <- sum((y_test - mean(y_test))^2)
  sse_test<- sum((predictions_test - y_test)^2)
  rsq_test <- 1 - (sse_test / sst_test)
  rmse_test = sqrt(sse/nrow(x_test))
  
  #calcular aciertos
  ac <- data.frame(y_test, predictions_test)
  ac$correct <- ifelse((ac$y_test * ac$predictions_test) > 0,1,0)
  aciertos <- sum(ac$correct)
  
  results <- data.frame(TEAM = team,
                        LAMBDA = best_e$lambda,
                        ALPHA = best_e$alpha,
                        R2 = rsq,
                        SST = sst,
                        SSE = sse,
                        RMSE = rmse,
                        R2_os = rsq_test,
                        correct = aciertos,
                        juegos_pred = 24
  )
  results <- results %>% 
    mutate(accuracy = correct / juegos_pred)
  return(results)
  
}




################################################################################
####################### ELASTIC NET EXCLUDING A PLAYER #########################
################################################################################
fit_elastic_exc <- function(team,exc)
{
  x_train <- x_matrix_exclution(team,exc)
  y_train <- y_vector(team)
  
  # Define train and test set
  train <- as.data.frame(cbind(x_train,y_train))
  
  # Set training control
  train_cont <- trainControl(method = "cv",
                             number = 10,
                             search = "random",
                             verboseIter = TRUE)
  
  # Train the model
  elastic_reg <- train(y_train ~ .,
                       data = train,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 10, #10 alpha values and 10 lambdas for each
                       trControl = train_cont)
  
  
  # Best tuning parameter
  best_e <- elastic_reg$bestTune
  params <- get_best_result(elastic_reg)
  
  # Make predictions on training set
  predictions_train_exc <- predict(elastic_reg, x_train)
  
  
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((predictions_train_exc - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  res <- data.frame(TEAM = team,
                    R2_pred = rsq,
                    SST = sst,
                    SSE = sse
  )
  
  results <- cbind(res,params) 
  
  return(results)
}



################################################################################
################# ELASTIC NET WITH TEST AND TRAINING WITHIN ####################
################# REGULAR SEASON AND AUTO PARAM SEARCH   ######################
################################################################################
elastic_season_pred <- function(team)
{
  x <- x_matrix(team)
  y <- y_vector(team)
  x_train <- x[1:58,]
  y_train <- y[1:58]
  x_test <- x[59:82,]
  y_test <- y[59:82]
  
  
  # Define train and test set
  train <- as.data.frame(cbind(x_train,y_train))
  
  # Set training control
  train_cont <- trainControl(method = "cv",
                             number = 10,
                             search = "random",
                             verboseIter = TRUE)
  
  # Train the model
  elastic_reg <- train(y_train ~ .,
                       data = train,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 10, #10 alpha values and 10 lambdas for each
                       trControl = train_cont)
  
  
  # Best tuning parameter
  best_e <- elastic_reg$bestTune
  params <- get_best_result(elastic_reg)
  
  # Make predictions on training set
  predictions_train <- predict(elastic_reg, x_train)
  #Predict on test (last 24 games)
  predictions_test <- predict(elastic_reg, x_test)
  
  # Metrics
  sst <- sum((y_train - mean(y_train))^2)
  sse <- sum((predictions_train - y_train)^2)
  rsq <- 1 - (sse / sst)
  rmse = sqrt(sse/nrow(x_train))
  
  sst_test <- sum((y_test - mean(y_test))^2)
  sse_test<- sum((predictions_test - y_test)^2)
  rsq_test <- 1 - (sse_test / sst_test)
  rmse_test = sqrt(sse/nrow(x_test))
  
  #calcular aciertos
  ac <- data.frame(y_test, predictions_test)
  ac$correct <- ifelse((ac$y_test * ac$predictions_test) > 0,1,0)
  aciertos <- sum(ac$correct)

  results <- data.frame(TEAM = team,
                        LAMBDA = best_e$lambda,
                        ALPHA = best_e$alpha,
                        R2 = rsq,
                        SST = sst,
                        SSE = sse,
                        RMSE = rmse,
                        R2_os = rsq_test,
                        correct = aciertos,
                        juegos_pred = 24
  )
  results <- results %>% 
    mutate(accuracy = correct / juegos_pred)
  return(results)
  
}

