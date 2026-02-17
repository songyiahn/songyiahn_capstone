#load library
library(here)
library(rio)
library(tidyverse)
library(recipes)
library(caret)
library(glmnet)
library(vip)
library(gridExtra)
library(tidymodels)

#import data
data <- import(here("data", "wieiad_cleaned.csv"))

pdata_info <- data %>%
  select(id, current_mood, group, ON, SAC, TII, BD, selfesteem, exce_exercise, mother, vlogexperience, help_received, recent_help, race_ethnicity, Age)

pdata_info <- pdata_info %>%
  mutate(help_received = if_else(is.na(help_received), 0, help_received)) 

pdata_info <- pdata_info %>%
  mutate(recent_help = if_else(is.na(recent_help), 0, recent_help))

pdata_info <- pdata_info %>%
  mutate(group = recode(group,
                        "Group1_C" = "non-food vlog",
                        "Group2_N" = "diet WIEIAD vlog",
                        "Group3_I" = "non-diet WIEIAD vlog",
                        "Group4_ID" = "non-diet WIEIAD vlog with prompts"))

pdata_info <- pdata_info %>%
  mutate(across(c(ON, SAC, BD, exce_exercise, mother), ~ round(., 2)))

#PRE-PROCESSING
#group variables into different types
outcome <- c('ON')

id      <- c('id')

categorical <- c('group',
                 'race_ethnicity')

numeric   <- c('current_mood',
               'SAC',
               'TII',
               'BD',
               'selfesteem',
               'exce_exercise',
               'mother',
               'vlogexperience',
               'recent_help',
               'Age')

props      <- c('help_received')

#Convert all nominal, ordinal, and binary variables to factors
for(i in c(categorical)){
  pdata_info[,i] <- as.factor(pdata_info[,i])
}

for(i in c(numeric, props)){
  pdata_info[,i] <- as.numeric(pdata_info[,i])
}

str(pdata_info)

#split Train/Test set
set.seed(11182025)

loc <- sample(1:nrow(pdata_info), round(nrow(pdata_info)*0.8))
pdata_info_tr <- pdata_info[loc, ]
pdata_info_te <- pdata_info[-loc, ]

dim(pdata_info_tr)
dim(pdata_info_te)

#making blueprint

#Create an indicator variable of missingness for all predictors (step_indicate_na)
#Remove the variables with zero variance (step_zv)
#Impute the missing values for all predictor variables using a mean or mode (step_impute_mean and step_impute_mode)
#Logit transform the variables that represent proportions(step_logit)
#Create polynomial terms up to the 2nd degree term for all numeric variables (step_poly)
#Standardize numeric features
#One-hot encoding of all categorical variables (step_dummy)
#Standardize the natural splines of numeric variables and proportions 
#To smooth out the polynomial function we created
# One-hot encoding for all categorical variables

blueprint <- recipe(ON ~ ., data = pdata_info_tr)%>%
  update_role(id, new_role = "ID") %>%
  step_indicate_na(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_logit(all_of(props), offset = 0.001) %>%
  step_ns(all_numeric_predictors(), deg_free = 3) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

blueprint

# create cv fold
set.seed(11182025)

folds <- vfold_cv(pdata_info_tr, v = 10)

# model define
lm_spec <- linear_reg() %>%
  set_engine("lm")

# create workflow
wf <- workflow() %>%
  add_recipe(blueprint) %>%
  add_model(lm_spec)

# 10-fold CV train
cv_res <- fit_resamples(
  wf,
  resamples = folds,
  metrics = metric_set(rsq, mae, rmse)
)

# CV result
cv_metrics <- collect_metrics(cv_res)
cv_metrics

final_fit <- fit(wf, data = pdata_info_tr)

#predict test
pred_te <- predict(final_fit, pdata_info_te) %>%
  bind_cols(pdata_info_te %>% select(ON))

rsq_te <- rsq(pred_te, truth = ON, estimate = .pred)
rsq_te

mae_te <- mae(pred_te, truth = ON, estimate = .pred)
mae_te

rmse_te <- rmse(pred_te, truth = ON, estimate = .pred)
rmse_te

#nonregularized model report
cvreport1 <- cv_metrics %>%
  select(.metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean)

tereport1 <- tibble(
  rsq  = rsq_te$.estimate,
  mae  = mae_te$.estimate,
  rmse = rmse_te$.estimate
)

report_li <- bind_rows(
  Training_CV = cvreport1,
  Test = tereport1,
  .id = "Data"
)

report_li

# Linear Regression with Ridge Penalty Model

# recipe prep
blueprint_prep <- prep(blueprint, training = pdata_info_tr)

train_baked <- bake(blueprint_prep, new_data = pdata_info_tr)
test_baked <- bake(blueprint_prep, new_data = pdata_info_te)

x_train <- as.matrix(train_baked %>% select(-ON))
y_train <- train_baked$ON

x_test <- as.matrix(test_baked %>% select(-ON))
y_test <- test_baked$ON


# CV
set.seed(11182025)

cv <- trainControl(method = "cv",
                   number = 10)

grid <- expand.grid(alpha = 0, lambda = seq(0.01, 1, .01))
grid

#train ridge model 
ridge <- caret::train(x = x_train,
                      y = y_train,
                      method = "glmnet",
                      trControl = cv,
                      tuneGrid = grid)

plot(ridge)

ridge$bestTune

ridge$results[16,]

#ridge performance check
predicted_te_ridge <- predict(ridge, newdata = x_test)
predicted_te_ridge

rsq_te_ridge <- cor(y_test, predicted_te_ridge)^2
rsq_te_ridge

mae_te_ridge <- mean(abs(y_test - predicted_te_ridge))
mae_te_ridge

rmse_te_ridge <- sqrt(mean((y_test - predicted_te_ridge)^2))
rmse_te_ridge

#ridge model report
cvreport2 <- ridge$results[16, c("Rsquared", "MAE", "RMSE")]
tereport2 <- data.frame(Rsquared = rsq_te_ridge,
                        MAE = mae_te_ridge,
                        RMSE = rmse_te_ridge)

report_ridge <- rbind(
  Training_CV = cvreport2,
  Test = tereport2
)

report_ridge

# Linear Regression with Lasso Penalty Model

#CV
set.seed(11182025)

cv <- trainControl(method = "cv",
                   number = 10)

grid <- expand.grid(alpha = 1, lambda = seq(0.01, 0.04, .001))
grid

#train lasso model 
lasso <- caret::train(x = x_train,
                      y = y_train,
                      method = "glmnet",
                      trControl = cv,
                      tuneGrid = grid)

plot(lasso)

lasso$bestTune

lasso$results[7,]

#lasso performance check
predicted_te_lasso <- predict(lasso, newdata = x_test)
predicted_te_lasso

rsq_te_lasso <- cor(y_test, predicted_te_lasso)^2
rsq_te_lasso

mae_te_lasso <- mean(abs(y_test - predicted_te_lasso))
mae_te_lasso

rmse_te_lasso <- sqrt(mean((y_test - predicted_te_lasso)^2))
rmse_te_lasso

#lasso model report
cvreport3 <- lasso$results[7, c("Rsquared", "MAE", "RMSE")]
tereport3 <- data.frame(Rsquared = rsq_te_lasso,
                        MAE = mae_te_lasso,
                        RMSE = rmse_te_lasso)

report_lasso <- rbind(
  Training_CV = cvreport3,
  Test = tereport3
)

report_lasso


# Linear Regression with Elastic Net Penalty Model

#CV
set.seed(11182025)

cv <- trainControl(method = "cv",
                   number = 10)

grid <- expand.grid(
  alpha = seq(0, 1, 0.1),
  lambda = seq(0.001, 0.2, .001))
grid

#train elastic net model 
elastic <- caret::train(x = x_train,
                        y = y_train,
                        method = "glmnet",
                        trControl = cv,
                        tuneGrid = grid)

plot(elastic)

elastic$bestTune

elastic$results[2016,]

#elastic performance check
predicted_te_elastic <- predict(elastic, newdata = x_test)
predicted_te_elastic

rsq_te_elastic <- cor(y_test, predicted_te_elastic)^2
rsq_te_elastic

mae_te_elastic <- mean(abs(y_test - predicted_te_elastic))
mae_te_elastic

rmse_te_elastic <- sqrt(mean((y_test - predicted_te_elastic)^2))
rmse_te_elastic

#elastic model report
cvreport4 <- elastic$results[2016, c("Rsquared", "MAE", "RMSE")]
tereport4 <- data.frame(Rsquared = rsq_te_elastic,
                        MAE = mae_te_elastic,
                        RMSE = rmse_te_elastic)

report_elastic <- rbind(
  Training_CV = cvreport4,
  Test = tereport4
)

report_elastic

#evaluating models
test_li    <- report_li["Test", ]
test_ridge <- report_ridge["Test", ]
test_lasso <- report_lasso["Test", ]
test_elastic <- report_elastic["Test", ]

test_summary <- bind_rows(
  "Linear Regression" = test_li,
  "Ridge" = test_ridge,
  "Lasso" = test_lasso,
  "Elastic Net" = test_elastic,
  .id = "Model"
)

test_summary

# regression coefficients for the final model 
options(scipen = 99)
coefs <- coef(elastic$finalModel, s = elastic$bestTune$lambda)
coefs

coefs.zero <- coefs[which(coefs[,1] == 0), ]
length(coefs.zero)

# find the most important 10 predictors
ind <- order(abs(coefs), decreasing = T)
head(as.matrix(coefs[ind[-1],]), 10)

vip(elastic,
    num_features = 10,
    geom = "point") +
  theme_bw()
