
## lasso

run_lasso <- function(
  dat_train,
  dat_test,
  dat_total,
  params,
  indcv,
  iter,
  budget
) {

  # split/cross-validation
  cv <- params$cv

  ## train
  fit_train <- train_lasso(dat_train)

  ## test
  fit_test <- test_lasso(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )


  return(list(test = fit_test, train = fit_train))
}



train_lasso <- function(dat_train) {

  ## format training data
  training_data_elements_lasso <- create_ml_args_lasso(dat_train)

   ## outcome
  outcome = training_data_elements_lasso[["Y"]]

  if(length(unique(outcome)) > 2){
      ## find the best lambda
      # cv.lasso <- glmnet::cv.glmnet(
      #   training_data_elements_lasso[["X_expand"]],
      #   training_data_elements_lasso[["Y"]],
      #   alpha = 1,
      #   family = "gaussian")

      ## fit
      fit <- glmnet::glmnet(
        training_data_elements_lasso[["X_expand"]],
        training_data_elements_lasso[["Y"]],
        alpha = 1,
        family = "gaussian",
        # lambda = cv.lasso$lambda.min)
        lambda = 0.05)

    }else {
      ## find the best lambda
      # cv.lasso <- glmnet::cv.glmnet(
      #   training_data_elements_lasso[["X_expand"]],
      #   training_data_elements_lasso[["Y"]],
      #   alpha = 1,
      #   family = "binomial")

      ## fit
      fit <- glmnet::glmnet(
        training_data_elements_lasso[["X_expand"]],
        training_data_elements_lasso[["Y"]],
        alpha = 1,
        family = "binomial",
        # lambda = cv.lasso$lambda.min)
        lambda = 0.05)
  }

  return(fit)
}

#'@importFrom stats predict runif
test_lasso <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, budget, cv
) {

  ## format data
  testing_data_elements_lasso <- create_ml_args_lasso(dat_test)
  total_data_elements_lasso   <- create_ml_args_lasso(dat_total)

  if(cv == TRUE){
    ## predict
    Y0t1_total = predict(
      fit_train,
      total_data_elements_lasso[["X0t_expand"]],
      type = "response")
    Y1t1_total = predict(
      fit_train,
      total_data_elements_lasso[["X1t_expand"]],
      type = "response")

    tau_total=Y1t1_total-Y0t1_total + runif(n_df,-1e-6,1e-6)

    ## compute quantities of interest
    tau_test <-  tau_total[indcv == iter]
    That     <-  as.numeric(tau_total > 0)
    That_p   <- as.numeric(tau_total >= sort(tau_test, decreasing = TRUE)[floor(budget*length(tau_test))+1])


    ## output
    cf_output <- list(
      tau      = c(tau_test, rep(NA, length(tau_total) - length(tau_test))),
      tau_cv   = tau_total,
      That_cv  = That,
      That_pcv = That_p
    )
  }

  if(cv == FALSE){
    ## predict
    Y0t1_test = predict(
      fit_train,
      testing_data_elements_lasso[["X0t_expand"]],
      type = "response")
    Y1t1_test = predict(
      fit_train,
      testing_data_elements_lasso[["X1t_expand"]],
      type = "response")

    tau_test=Y1t1_test-Y0t1_test

    ## compute quantities of interest
    That     =  as.numeric(tau_test > 0)
    That_p   = numeric(length(That))
    That_p[sort(tau_test,decreasing =TRUE,index.return=TRUE)$ix[1:(floor(budget*length(tau_test))+1)]] = 1

    ## output
    cf_output <- list(
      tau      = tau_test,
      tau_cv   = tau_test,
      That_cv  = That,
      That_pcv = That_p
      )
  }

  return(cf_output)
}


