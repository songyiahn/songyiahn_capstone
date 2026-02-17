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
library(tibble)

#import data
data <- import(here("data", "wieiad_cleaned.csv"))

pdata_info <- data %>%
  select(id, current_mood, group, ON, SAC, TII, BD, selfesteem, exce_exercise, mother, vlogexperience, help_received, recent_help, Age)

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

categorical <- c('group')

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

# recipe prep
blueprint_prep <- prep(blueprint, training = pdata_info_tr)

train_baked <- bake(blueprint_prep, new_data = pdata_info_tr)
test_baked <- bake(blueprint_prep, new_data = pdata_info_te)

x_train <- as.matrix(train_baked %>% select(-ON))
y_train <- train_baked$ON

x_test <- as.matrix(test_baked %>% select(-ON))
y_test <- test_baked$ON


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

elastic$results[2014,]

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
cvreport4 <- elastic$results[2014, c("Rsquared", "MAE", "RMSE")]
tereport4 <- data.frame(Rsquared = rsq_te_elastic,
                        MAE = mae_te_elastic,
                        RMSE = rmse_te_elastic)

report_elastic <- rbind(
  Training_CV = cvreport4,
  Test = tereport4
)

report_elastic

saveRDS(elastic, "models/elastic_model.rds")
saveRDS(blueprint_prep, "models/recipe_prep.rds")

#coef names
coef_vals <- coef(elastic$finalModel, elastic$bestTune$lambda)
coef_vals[coef_vals != 0]

coef_df <- as.matrix(coef_vals) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(coef = 2) %>%
  filter(coef != 0) %>%
  arrange(desc(abs(coef)))

coef_df

