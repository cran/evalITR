## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "../man/figures/README-"
  )

library(dplyr)

load("../data/star.rda")

# specifying the outcome
outcomes <- "g3tlangss"

# specifying the treatment
treatment <- "treatment"

# specifying the data (remove other outcomes)
star_data <- star %>% dplyr::select(-c(g3treadss,g3tmathss))

# specifying the formula
user_formula <- as.formula(
  "g3tlangss ~ treatment + gender + race + birthmonth + 
  birthyear + SCHLURBN + GRDRANGE + GKENRMNT + GKFRLNCH + 
  GKBUSED + GKWHITE ")

## ----cv_estimate, message = FALSE, out.width = '60%'--------------------------
library(evalITR)
# estimate ITR 
set.seed(2021)
fit_cv <- estimate_itr(
               treatment = treatment,
               form = user_formula,
               data = star_data,
               algorithms = c("causal_forest"),
               budget = 0.2,
               n_folds = 3)


## ----cv_eval, message = FALSE, out.width = '50%'------------------------------
# evaluate ITR 
est_cv <- evaluate_itr(fit_cv)

# summarize estimates
summary(est_cv)

