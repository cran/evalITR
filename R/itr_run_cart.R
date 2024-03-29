

## cart


run_cart <- function(
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
  fit_train <- train_cart(dat_train)

  ## test
  fit_test <- test_cart(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )


  return(list(test = fit_test, train = fit_train))
}


#' @import rpart
train_cart <- function(dat_train) {

  ## format training data
  training_data_elements_cart = create_ml_args_cart(dat_train)

  ## train formula
  formula_cart = training_data_elements_cart[["formula"]]

  ## outcome
  outcome = training_data_elements_cart[["data"]][["Y"]]

  if(length(unique(outcome)) > 2){
      ## fit
      fit <- rpart(formula_cart, data = training_data_elements_cart[["data"]], method = "anova")
      # control = rpart.control(minsplit = 2, minbucket = 1,
            # cp = 0.0015)) #relax the contraint for cart
  }else {
      ## fit
      fit <- rpart(formula_cart, data = training_data_elements_cart[["data"]], method = "class")
  }

  return(fit)
}

#'@importFrom stats predict runif
test_cart <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, budget, cv
) {

  ## format data
  testing_data_elements_cart = create_ml_args_cart(dat_test)
  total_data_elements_cart   = create_ml_args_cart(dat_total)

  ## outcome
  outcome = testing_data_elements_cart[["data"]][["Y"]]

  if(cv == TRUE){

    if(length(unique(outcome)) > 2){
      ## predict
      Y0t_total = predict(fit_train, newdata=total_data_elements_cart[["data0t"]])
      Y1t_total = predict(fit_train, newdata=total_data_elements_cart[["data1t"]])

      }else {
      ## predict
      Y0t_total = predict(
        fit_train,
        newdata = total_data_elements_cart[["data0t"]],
        type = "prob")[, 2]
      Y1t_total = predict(
        fit_train,
        newdata = total_data_elements_cart[["data1t"]],
        type = "prob")[, 2]
      }

      # predicted tau
      tau_total=Y1t_total - Y0t_total + runif(n_df,-1e-6,1e-6)

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

    if(length(unique(outcome)) > 2){
      ## predict
      Y0t_test = predict(fit_train, newdata=testing_data_elements_cart[["data0t"]])
      Y1t_test = predict(fit_train, newdata=testing_data_elements_cart[["data1t"]])

      }else {
      ## predict
      Y0t_test = predict(
        fit_train,
        newdata = testing_data_elements_cart[["data0t"]],
        type = "prob")[, 2]
      Y1t_test = predict(
        fit_train,
        newdata = testing_data_elements_cart[["data1t"]],
        type = "prob")[, 2]
      }

      # predicted tau
      tau_test = Y1t_test - Y0t_test + runif(length(Y0t_test),-1e-6,1e-6)

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


