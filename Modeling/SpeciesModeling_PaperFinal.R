# CODE 8
# Creating Random Forest models

# Ally Kruper
# 10/8/2024
# Goal: Use lidar and MS-derived metrics and 
  # random forest to identify and map western redcedar trees.
# Code is based off of a combination of code from Bob McGaughey ("speciesModeling_RevisedForPaper.R")
  # and code from this blog https://juliasilge.com/blog/sf-trees-random-tuning/
  # Also, going to try Tidyverse suite and some code from them: https://www.tidymodels.org/

#### General Model Information ####
 
# For models:
# Down-sample other species to match the sample size of cedar
# Mtry has to stay under 20
# Remove top predictors that are correlated variables (correlation greater than 0.7)
# Do THPL vs. Other
# Do mean intensity correction when adding in 2021 WH/DF data
# Due to shorter trees being impacted by shadow in the MS data --> only use trees above 20cm DBH for OESF and above 30cm for ONF

# Final models:
# Lidar only models
# 2023 and 2021 data combined; small cylinder
# 2023 and 2021 data combined; top 3m
# 2023 and 2021 data combined; small cylinder; OESF site only
# 2023 and 2021 data combined; top 3m; OESF site only
# Lidar and MS combined models
# 2023 data; both sites combined; top 3m
# 2023 data; both sites combined; small cylinder
# 2023 data; ONF site only; top 3m
# 2023 data; ONF site only; small cylinder
# 2023 data; OESF site only; top 3m
# 2023 data; OESF site only; small cylinder

## Ultimately, best lidar model results (and what model should be used for expanding the model) was 2023 and 2021 data combined; top 3m; OESF site only** 

library(tidymodels)
library(ranger)
library(vip)
library(themis)

#### 10-fold cross-validation on the lidar only; OESF; top 3m model #####
library(tidymodels)
library(ranger)
library(vip)
library(themis)

# reading in data
ModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics.csv")

# need to filter out "failed" trees (trees with total lidar returns less than 300 --> those tops were clearly missedd and aren't acutally trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 300), ]

# adding in site
ModelData[ , 'Site'] = NA

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Num[i] < 15) {
    ModelData$Site[i] <- "OESF"
  } else {
    ModelData$Site[i] <- "ONF"
  }
}
ModelData <- ModelData[which(ModelData$Site == "OESF"), ]

ModelData_WH_DF <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics_DF_WH.csv")

head(ModelData) # glimpse of data
head(ModelData_WH_DF) # glimpse of data --> need to combine with Model Data

# renaming a couple of the ModelData_WH_DF columns names to better match the ModelData names
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Xfield"] <- "X"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Yfield"] <- "Y"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseX"] <- "BaseX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseY"] <- "BaseY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseElevation"] <- "BaseElevation"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveHeight"] <- "Height"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopX"] <- "TopX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopY"] <- "TopY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveCrownDia"] <- "CrownDia"

# adding 100 to each of the WH/DF plot numbers so can later easily separate plots based on year
ModelData_WH_DF$Plot_Number <- ModelData_WH_DF$Plot_Number + 100

# new dataframes with only the column names that are the same between the dataframes
ModelData_WH_DF <- ModelData_WH_DF[ , colnames(ModelData_WH_DF) %in% colnames(ModelData)]
ModelData <- ModelData[ , colnames(ModelData) %in% colnames(ModelData_WH_DF)]

# combining the two datasets 
ModelData <- rbind(ModelData, ModelData_WH_DF)

# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")

# THPL vs. Other
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

# double checking that worked
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)


# adding in year --> then normalizing intenisty data based on year
ModelData[ , 'Year'] = NA

ModelData$Plot_Number <- as.integer(ModelData$Plot_Number)

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Number[i] == 11 | ModelData$Plot_Number[i] == 12 | ModelData$Plot_Number[i] == 13) {
    ModelData$Year[i] <- "2021"
  } else {
    if(ModelData$Plot_Number[i] > 30) {
      ModelData$Year[i] <- "2021" } else {
        ModelData$Year[i] <- "2023"
      }
  }
}


# normalizing intensity data
ModelData[which(ModelData$Year == "2021"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2021"), "Int.mean"] - 192.6818) / 17.02009777
ModelData[which(ModelData$Year == "2023"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2023"), "Int.mean"] - 119.895) / 57.01547338

# resampling / cross validation (doing the folds)
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(10, 20)),
  min_n(range = c(20, 30)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 20, trees = 1000, min_n = 20

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Int.mean, RP40, Elev.L.skewness, Elev.skewness, ELev.L3, RP70, Elev.P40, Elev.P95, RP50, RP75

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
# need to add the RPs as actual variables in the model data so that can calculate correlation
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )
cors <- lm(ModelData$Int.mean ~ ModelData$RP40)   # corr = 0.06--> RP40 ok
summary(cors)

cors <- lm(ModelData$Int.mean ~ ModelData$Elev.L.skewness)   # corr = 0.04 --> ok
summary(cors)

cors <- lm(ModelData$RP40 ~ ModelData$Elev.L.skewness)   # corr = 0.86 --> remove Elev.L.skewness 
summary(cors)

cors <- lm(ModelData$Elev.skewness ~ ModelData$RP40)   # corr = 0.83 --> remove Elev.skewness
summary(cors)

cors <- lm(ModelData$Elev.L3 ~ ModelData$RP40)   # corr = 0.85 --> remove Elev.L3
summary(cors)

cors <- lm(ModelData$RP40 ~ ModelData$RP70)   # corr = 0.68 --> ok
summary(cors)

cors <- lm(ModelData$Int.mean ~ ModelData$RP70)   # corr = 0.03 --> RP70 ok
summary(cors)

cors <- lm(ModelData$RP40 ~ ModelData$Elev.P40)   # corr = 0.97--> remove Elev.P40
summary(cors)

cors <- lm(ModelData$RP40 ~ ModelData$Elev.P95)   # corr = 0.09 --> ok
summary(cors)

cors <- lm(ModelData$RP70 ~ ModelData$Elev.P95)   # corr = 0.12 --> remove RP50
summary(cors)

cors <- lm(ModelData$Int.mean ~ ModelData$Elev.P95)   # corr = 0.01 --> Elev.P95 ok
summary(cors)

cors <- lm(ModelData$RP40 ~ ModelData$RP50)   # corr = 0.92 --> remove RP50
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$RP70)   # corr = 0.94 --> remove RP75
summary(cors)



# only uncorrelated variables were Int.mean, RP40, RP70, and Elev.P95 --> going to remove all other variables and re-run the model
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis, 
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, 
            Elev.L.skewness, Elev.skewness, Elev.L3, Elev.P40, RP50, RP75))

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(10, 20)),
  min_n(range = c(5, 15)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 17, trees = 1000, min_n = 12

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Int.mean, RP70, RP40, Profile.area, Elev.CV, Elev.P70, RP80, Elev.P30, RP30, Elev.P75 

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
cors <- lm(ModelData$Profile.area ~ ModelData$Int.mean)   # corr = 0.05 -->   ok
summary(cors)

cors <- lm(ModelData$Profile.area ~ ModelData$RP70)   # corr = 0.85 --> remove Profile.area
summary(cors)

cors <- lm(ModelData$Elev.CV ~ ModelData$RP40)   # corr = 0.86--> remove Elev.CV
summary(cors)

cors <- lm(ModelData$Elev.P70 ~ ModelData$RP70)   # corr = 0.92 --> remove Elev.P70
summary(cors)

cors <- lm(ModelData$RP80 ~ ModelData$RP70)   # corr = 0.82 --> remove RP80
summary(cors)

cors <- lm(ModelData$Elev.P30 ~ ModelData$RP40)   # corr = 0.90 --> remove Elev.P30
summary(cors)

cors <- lm(ModelData$RP30 ~ ModelData$RP40)   # corr = 0.91 --> remove RP30 area
summary(cors)

cors <- lm(ModelData$Elev.P75 ~ ModelData$RP70)   # corr = 0.85 --> remove Elev.P75
summary(cors)

# everything (but int.mean,  RP40, and RP70) was correlated  --> going to remove all of the correlated ones found here from the model and run it again
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis, 
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99,
            Elev.L.skewness, Elev.skewness, Elev.L3, Elev.P40, RP50, RP75,
            Profile.area, Elev.CV, Elev.P70, RP80, Elev.P30, RP30, Elev.P75 )) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(10, 20)),
  min_n(range = c(15, 25)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 17, trees = 1000, min_n = 22

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Int.mean, RP70, RP40, RP60, Elev.P50, RP20, Elev.mean, Elev.L1, Elev.P80, Canopy.relief.ratio


# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7; only going until I hit two more uncorrelated to make 5 total
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )
cors <- lm(ModelData$RP70 ~ ModelData$RP60)   # corr = 0.89 -->  remove RP60
summary(cors)

cors <- lm(ModelData$RP70 ~ ModelData$Elev.P50)   # corr = 0.73 --> remove Elev.P50
summary(cors)

cors <- lm(ModelData$RP20 ~ ModelData$RP40)   # corr = 0.77 --> remove RP20
summary(cors)

cors <- lm(ModelData$RP40 ~ ModelData$Elev.mean)   # corr = 0.84 --> remove Elev.mean
summary(cors)

cors <- lm(ModelData$Elev.L1 ~ ModelData$RP40)   # corr = 0.84 -->  remove Elev.L1
summary(cors)

cors <- lm(ModelData$Elev.P80 ~ ModelData$RP70)   # corr = 0.72 -->  remove Elev.P80
summary(cors)

cors <- lm(ModelData$Canopy.relief.ratio ~ ModelData$RP40)   # corr = 0.84 --> remove Canopy.relief.ratio
summary(cors)



# everything but int.mean, RP70, and RP40 was correlated --> going to remove all of the correlated ones found here from the model and run it again

set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis, 
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99,
            Elev.L.skewness, Elev.skewness, Elev.L3, Elev.P40, RP50, RP75,
            Profile.area, Elev.CV, Elev.P70, RP80, Elev.P30, RP30, Elev.P75, 
            RP60, Elev.P50, RP20, Elev.mean, Elev.L1, Elev.P80, Canopy.relief.ratio)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(10, 20)),
  min_n(range = c(5, 15)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 20, trees = 1000, min_n = 5

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Int.mean, RP40, RP70, Elev.P60, RP25, RP90, Elev.P20, Elev.SQRT.mean.SQ, RP95, Elev.P01


# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7; only going until I hit two more uncorrelated to make 5 total
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )
cors <- lm(ModelData$Elev.P60 ~ ModelData$RP70)   # corr = 0.85 -->  remove Elev.P60
summary(cors)

cors <- lm(ModelData$RP25 ~ ModelData$RP40)   # corr = 0.85 --> remove RP25
summary(cors)

cors <- lm(ModelData$RP90 ~ ModelData$RP70)   # corr = 0.49 --> ok
summary(cors)

cors <- lm(ModelData$RP90 ~ ModelData$RP40)   # corr = 0.37 -->  ok
summary(cors)

cors <- lm(ModelData$RP90 ~ ModelData$Int.mean)   # corr = 0.02 -->  RP90 ok
summary(cors)

cors <- lm(ModelData$Elev.P20 ~ ModelData$RP40)   # corr = 0.77 --> remove Elev.P20
summary(cors)

cors <- lm(ModelData$Elev.SQRT.mean.SQ ~ ModelData$RP70)   # corr = 0.75 --> remove Elev.SQRT.mean.SQ
summary(cors)

cors <- lm(ModelData$RP95 ~ ModelData$RP90)    # corr = 0.59 -->  ok
summary(cors)

cors <- lm(ModelData$RP70 ~ ModelData$RP95)   # corr = 0.23 -->  ok
summary(cors)

cors <- lm(ModelData$RP40 ~ ModelData$RP95)   # corr = 0.16 --> ok
summary(cors)

cors <- lm(ModelData$Int.mean ~ ModelData$RP95)   # corr = 0.004 --> RP95 ok
summary(cors)


# these are the final top 5 uncorrelated variables: Int.mean, RP40, RP70, RP90, RP95


# re-running model with those variables only
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.stddev, Elev.CV, Elev.IQ, 
            Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
            Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
            Elev.P60, Elev.P70, Elev.P75, Elev.P90, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Profile.area, Elev.L3, RP05, RP10, RP20, RP25, RP30, RP60, RP75,
            Elev.P99, Elev.P05, Elev.variance, Elev.L.skewness, RP01, RP50, RP80, Elev.P01, Elev.P80,
            Elev.skewness, Elev.P95, Elev.CURT.mean.CUBE)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 2)),
  min_n(range = c(5, 20)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1000, min_n = 8

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Int.mean, RP40, Rp70, RP90, RP95


# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(BaseX, BaseY, Year,
              BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
              Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
              Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.stddev, Elev.CV, Elev.IQ, 
              Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
              Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
              Elev.P60, Elev.P70, Elev.P75, Elev.P90, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Profile.area, Elev.L3, RP05, RP10, RP20, RP25, RP30, RP60, RP75,
              Elev.P99, Elev.P05, Elev.variance, Elev.L.skewness, RP01, RP50, RP80, Elev.P01, Elev.P80,
              Elev.skewness, Elev.P95, Elev.CURT.mean.CUBE)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 85%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 4%

#### 10-fold cross-validation on the MS and lidar; ONF; top 3m model #####
LidarModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics.csv")
MSModelData <- read.csv("data/MS_Data/all_trees_w_MS_values_cleaned_for_thesis.csv")

# lidar data covers plots 11, 12, 13 but MS does not --> need to remove those plots in the lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Plot_Number == 11 | LidarModelData$Plot_Number == 12 |LidarModelData$Plot_Number == 13),]

# MS data also doesn't have coverage for two trees --> removing those from lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "6_37_THPL" | LidarModelData$Tree.ID == "6_38_THPL"),]

# MS data also had a few trees with problems --> removed those at outliers/ones not computed correctly --> need to remove those in the lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "15_123_ACCI" | LidarModelData$Tree.ID == "17_113_THPL" | LidarModelData$Tree.ID == "20_9_THPL" | LidarModelData$Tree.ID == "20_30_TSHE" | LidarModelData$Tree.ID == "18_21_ALRU"),]


# looks like there were two trees (9_41_THPL and 9_206_THPL that were dropped from the lidar data at some point --> removing that from MS data)
MSModelData <- MSModelData[-which(MSModelData$Tree.ID == "9_41_THPL" | MSModelData$Tree.ID == "9_206_THPL"),]

# removing columns in the MS data that have the same/similar column in lidar for easier merging
MSModelData <- subset(MSModelData, select = -c(X, Elevation, Total.Height, DBH, Status.Code, Species, Plot_Num))

# combining the two data sets
ModelData <- merge(x = LidarModelData, y = MSModelData, by = "Tree.ID")

# Smaller/shorter trees in MS data were impacted by shadows --> removing trees under 20cm in OESF data and under 30cm in ONF data
ModelData[ , 'Site'] = NA

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Num[i] < 15) {
    ModelData$Site[i] <- "OESF"
  } else {
    ModelData$Site[i] <- "ONF"
  }
}

ModelData <- rbind(ModelData[which(ModelData$Site == "OESF" & ModelData$DBH > 0.20),], ModelData[which(ModelData$Site == "ONF" & ModelData$DBH > 0.30),])

# subsetting to only ONF site
ModelData <- ModelData[which(ModelData$Site == "ONF"),]

# removing site as variable so don't have to worry about removing it as variable later
ModelData <- subset(ModelData, select = -c(Site))

# need to filter out "failed" trees (trees with total lidar returns less than 300 --> those tops were clearly missedd and aren't acutally trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 300), ]

# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")
# so few not THPL or TSHE --> going to combine all not THPL into one species category
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)

## first, train data on FULL dataset (this is what would be used to predict on new datasets)

# resampling / cross validation (doing the folds) to help determine hyperparameters
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species") # because LOOCV, training based on WHOLE data

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% # because LOOCV, training based on WHOLE data
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 20)),
  min_n(range = c(20, 37)),
  levels = 5 
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 5, trees = 1,000, min_n = 24

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be nvdinir, msr, red, blue, Int.P10, Int.P05, Int.L.CV, msrredrededge, Int.P25, Int.CV

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )

cors <- lm(ModelData$nvdinir ~ ModelData$msr)   # corr = 0.95 --> remove msr
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$red)   # corr = 0.76 --> remove red
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$blue)   # corr = 0.78 --> remove blue
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$Int.L.CV)   # corr = 0.30 --> Int.L.CV ok
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$Int.P10)   # corr = 0.22 -->  ok
summary(cors)

cors <- lm(ModelData$Int.L.CV ~ ModelData$Int.P10)   # corr = 0.79 -->  remove Int.P10
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$msrredrededge)   # corr = 0.89 --> remove msrredrededge
summary(cors)

cors <- lm(ModelData$Int.L.CV ~ ModelData$Int.P05)   # corr = 0.73 -->  remove Int.P05
summary(cors)

cors <- lm(ModelData$Int.L.CV ~ ModelData$Int.CV)   # corr = 0.99 -->  remove Int.CV
summary(cors)

cors <- lm(ModelData$Int.P25 ~ ModelData$Int.L.CV)   # corr = 0.53 --> remove Int.P25
summary(cors)


#  list of uncorrelated variables; still need one more (going to re-run model with correlated variables removed):
# nvdinir, Int.L.CV

set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, msr, red, blue, Int.P10, Int.P05, msrredrededge, Int.P25, Int.CV)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 20)),
  min_n(range = c(1, 20)),
  levels = 5 
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 15, trees = 1,000, min_n = 1

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be nvdinir, Int.L.CV, nvdiredrededge, Int.P20, cigreen, Int.P01, ciredrededge, Int.minimum, Int.P30, RP05

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
# just going until have one more (5 total)
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )

cors <- lm(ModelData$nvdinir ~ ModelData$nvdiredrededge)   # corr = 0.92 --> remove nvdiredrededge
summary(cors)

cors <- lm(ModelData$Int.L.CV ~ ModelData$Int.P20)   # corr = 0.63--> ok
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$Int.P20)   # corr = 0.14--> Int.P20 ok
summary(cors)

cors <- lm(ModelData$cigreen ~ ModelData$nvdinir)   # corr = 0.79 --> remove cigreen
summary(cors)

cors <- lm(ModelData$Int.P01 ~ ModelData$Int.L.CV)   # corr = 0.37 -->  ok
summary(cors)

cors <- lm(ModelData$Int.P01 ~ ModelData$Int.P20)   # corr = 0.05--> ok
summary(cors)

cors <- lm(ModelData$Int.P01 ~ ModelData$nvdinir)   # corr = 0.27 -->  Int.P01 ok
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$ciredrededge)   # corr = 0.84 -->  remove ciredrededge
summary(cors)

cors <- lm(ModelData$Int.minimum ~ ModelData$Int.L.CV)   # corr = 0.18 -->  ok
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$Int.minimum)   # corr = 0.23 -->  ok
summary(cors)

cors <- lm(ModelData$Int.minimum ~ ModelData$Int.P20)   # corr = 0.002-->  ok
summary(cors)

cors <- lm(ModelData$Int.P01 ~ ModelData$Int.minimum)   # corr = 0.67 -->  Int.minimum ok
summary(cors)

cors <- lm(ModelData$Int.P30 ~ ModelData$Int.minimum)   # corr = 0.87 -->  remove Int.P30
summary(cors)

# final variables: nvdinir, Int.L.CV, Int.minimum, Int.P01, Int.P20


# re-running model with those variables only
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.skewness, Int.L.kurtosis,
            Int.P05, Int.P10, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
            Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
            Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
            Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP10, RP25, RP30, RP40, RP60, RP70, RP75,
            RP90, RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.L.skewness, Elev.P01, 
            cigreen, cirededge, ciredrededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
            red, green, blue,  Elev.P99, RP50, RP80, RP20, Elev.L3)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(2, 3)),
  min_n(range = c(10, 20)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 2, trees = 1000, min_n = 20

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be nvdinir, Int.L.CV, Int.P20, Int.P01, Int.minimum



# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
              Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
              BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
              Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.skewness, Int.L.kurtosis,
              Int.P05, Int.P10, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
              Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
              Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
              Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP10, RP25, RP30, RP40, RP60, RP70, RP75,
              RP90, RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.L.skewness, Elev.P01, 
              cigreen, cirededge, ciredrededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
              red, green, blue,  Elev.P99, RP50, RP80, RP20, Elev.L3)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 85%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 10%

#### Testing out 10-fold cross-validation on the MS and lidar; ONF; small cylinder model #####
LidarModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics.csv")
MSModelData <- read.csv("data/MS_Data/all_trees_w_MS_values_cleaned_for_thesis.csv")

# MS data also doesn't have coverage for two trees --> removing those from lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "6_37_THPL" | LidarModelData$Tree.ID == "6_38_THPL"),]

# MS data also had a few trees with problems --> removed those at outliers/ones not computed correctly --> need to remove those in the lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "15_123_ACCI" | LidarModelData$Tree.ID == "17_113_THPL" | LidarModelData$Tree.ID == "20_9_THPL" | LidarModelData$Tree.ID == "20_30_TSHE" | LidarModelData$Tree.ID == "18_21_ALRU"),]

# looks like there were two trees (9_41_THPL and 9_206_THPL that were dropped from the lidar data at some point --> removing that from MS data)
MSModelData <- MSModelData[-which(MSModelData$Tree.ID == "9_41_THPL" | MSModelData$Tree.ID == "9_206_THPL"),]

# removing columns in the MS data that have the same/similar column in lidar for easier merging
MSModelData <- subset(MSModelData, select = -c(X, Elevation, Total.Height, DBH, Status.Code, Species, Plot_Num))

# combining the two data sets
ModelData <- merge(x = LidarModelData, y = MSModelData, by = "Tree.ID")

# Smaller/shorter trees in MS data were impacted by shadows --> removing trees under 20cm in OESF data and under 30cm in ONF data
ModelData[ , 'Site'] = NA

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Num[i] < 15) {
    ModelData$Site[i] <- "OESF"
  } else {
    ModelData$Site[i] <- "ONF"
  }
}

ModelData <- rbind(ModelData[which(ModelData$Site == "OESF" & ModelData$DBH > 0.20),], ModelData[which(ModelData$Site == "ONF" & ModelData$DBH > 0.30),])

# subsetting to only ONF site
ModelData <- ModelData[which(ModelData$Site == "ONF"),]

# removing site as variable so don't have to worry about removing it as variable later
ModelData <- subset(ModelData, select = -c(Site))

# need to filter out "failed" trees (trees with total lidar returns less than 450 for small cylinder trees --> those tops were clearly missed and aren't actually trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 450), ]

# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")
# so few not THPL or TSHE --> going to combine all not THPL into one species category
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)

# resampling / cross validation (doing the folds)
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 15)),
  min_n(range = c(5, 20)),
  levels = 5 
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 8, trees = 1,000, min_n = 8

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be nvdinir, Int.L.CV, Int.CV, msr, Elev.P99, Int.L3, Elev.IQ, Int.skewness, Int.L.skewness, Elev.P90

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
cors <- lm(ModelData$Int.L.CV ~ ModelData$nvdinir)   # corr = 0.28 --> nvdinir ok
summary(cors)

cors <- lm(ModelData$Int.L.CV ~ ModelData$Int.CV)   # corr = 0.99 --> remove Int.CV
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$msr)   # corr = 0.95 -->  remove msr
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Int.L.CV)   # corr = 0.18 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$nvdinir)   # corr = 0.35 -->  Elev.P99 ok
summary(cors)

cors <- lm(ModelData$Int.L3 ~ ModelData$Int.L.CV)   # corr = 0.36 --> ok
summary(cors)

cors <- lm(ModelData$Int.L3 ~ ModelData$nvdinir)   # corr = 0.005 -->  ok
summary(cors)

cors <- lm(ModelData$Int.L3 ~ ModelData$Elev.P99)   # corr = 0.29 --> Int.L3 ok
summary(cors)

cors <- lm(ModelData$Elev.IQ ~ ModelData$Elev.P99)   # corr = 0.34 --> ok
summary(cors)

cors <- lm(ModelData$Elev.IQ ~ ModelData$nvdinir)   # corr = 0.34 -> ok
summary(cors)

cors <- lm(ModelData$Elev.IQ ~ ModelData$Int.L.CV)   # corr = 0.11 --> ok
summary(cors)

cors <- lm(ModelData$Elev.IQ~ ModelData$Int.L3)   # corr = 0.01 --> Elev.IQ ok
summary(cors)



# top 5 predictors: nvdinir, Int.L.CV, Elev.P99, Int.L3, Elev.IQ


# re-running model with those variables only
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.IQ, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L4, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, 
            Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
            Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40,
            Elev.P60, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP10, RP25, RP30, RP40, RP60, RP70, RP75,
            RP90, RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.L.skewness, Elev.P01, 
            cigreen, cirededge, ciredrededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
            red, green, blue, RP50, RP80, RP20, Elev.L3, Int.CV, Elev.P70, 
            Int.skewness, Elev.P50)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 3)),
  min_n(range = c(5, 15)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 3, trees = 1000, min_n = 5

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be nvdinir, Elev.IQ, Int.L3, Int.L.CV, Elev.P99


# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
              Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
              BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.IQ, Int.kurtosis,
              Int.AAD, Int.L1, Int.L2, Int.L4, Int.L.skewness, Int.L.kurtosis,
              Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, 
              Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
              Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40,
              Elev.P60, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP10, RP25, RP30, RP40, RP60, RP70, RP75,
              RP90, RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.L.skewness, Elev.P01, 
              cigreen, cirededge, ciredrededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
              red, green, blue, RP50, RP80, RP20, Elev.L3, Int.CV, Elev.P70, 
              Int.skewness, Elev.P50)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 84%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 17%


#### 10-fold cross-validation on the MS and lidar; OESF; top 3m model #####
LidarModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics.csv")
MSModelData <- read.csv("data/MS_Data/all_trees_w_MS_values_cleaned_for_thesis.csv")

# lidar data covers plots 11, 12, 13 but MS does not --> need to remove those plots in the lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Plot_Number == 11 | LidarModelData$Plot_Number == 12 |LidarModelData$Plot_Number == 13),]

# MS data also doesn't have coverage for two trees --> removing those from lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "6_37_THPL" | LidarModelData$Tree.ID == "6_38_THPL"),]

# MS data also had a few trees with problems --> removed those at outliers/ones not computed correctly --> need to remove those in the lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "15_123_ACCI" | LidarModelData$Tree.ID == "17_113_THPL" | LidarModelData$Tree.ID == "20_9_THPL" | LidarModelData$Tree.ID == "20_30_TSHE" | LidarModelData$Tree.ID == "18_21_ALRU"),]

# looks like there were two trees (9_41_THPL and 9_206_THPL that were dropped from the lidar data at some point --> removing that from MS data)
MSModelData <- MSModelData[-which(MSModelData$Tree.ID == "9_41_THPL" | MSModelData$Tree.ID == "9_206_THPL"),]

# removing columns in the MS data that have the same/similar column in lidar for easier merging
MSModelData <- subset(MSModelData, select = -c(X, Elevation, Total.Height, DBH, Status.Code, Species, Plot_Num))

# combining the two data sets
ModelData <- merge(x = LidarModelData, y = MSModelData, by = "Tree.ID")

# Smaller/shorter trees in MS data were impacted by shadows --> removing trees under 20cm in OESF data and under 30cm in ONF data
ModelData[ , 'Site'] = NA

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Num[i] < 15) {
    ModelData$Site[i] <- "OESF"
  } else {
    ModelData$Site[i] <- "ONF"
  }
}

ModelData <- rbind(ModelData[which(ModelData$Site == "OESF" & ModelData$DBH > 0.20),], ModelData[which(ModelData$Site == "ONF" & ModelData$DBH > 0.30),])

# subsetting to only ONF site
ModelData <- ModelData[which(ModelData$Site == "OESF"),]

# removing site as variable so don't have to worry about removing it as variable later
ModelData <- subset(ModelData, select = -c(Site))

# need to filter out "failed" trees (trees with total lidar returns less than 300 --> those tops were clearly missedd and aren't acutally trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 300), ]


# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")
# so few not THPL or TSHE --> going to combine all not THPL into one species category
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)


# resampling / cross validation (doing the folds)
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 15)),
  min_n(range = c(5, 20)),
  levels = 5 
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1,000, min_n = 8

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.skewness, Profile.area, RP60, RP50, Elev.P50, RP40, Elev.L.skewness, Elev.P40, Elev.L3, Elev.P20

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )

cors <- lm(ModelData$Elev.skewness ~ ModelData$Profile.area)   # corr = 0.85 --> remove profile.area
summary(cors)

cors <- lm(ModelData$Elev.skewness ~ ModelData$RP60)   # corr = 0.92 --> remove RP60 
summary(cors)

cors <- lm(ModelData$Elev.skewness ~ ModelData$RP50)   # corr = 0.91 --> remove RP50 area
summary(cors)

cors <- lm(ModelData$Elev.skewness ~ ModelData$Elev.P50)   # corr = 0.87 --> remove Elev.P50
summary(cors)

cors <- lm(ModelData$Elev.skewness ~ ModelData$RP40)   # corr = 0.87 -->  remove RP40
summary(cors)

cors <- lm(ModelData$Elev.skewness ~ ModelData$Elev.L.skewness)   # corr = 0.97 --> remove Elev.L.skewness
summary(cors)

cors <- lm(ModelData$Elev.skewness ~ ModelData$Elev.P40)   # corr = 0.84 --> remove Elev.P40
summary(cors)

cors <- lm(ModelData$Elev.skewness ~ ModelData$Elev.L3)   # corr = 0.95 --> remove Elev.L3
summary(cors)

cors <- lm(ModelData$Elev.skewness ~ ModelData$Elev.P20)   # corr = 0.62 --> Elev.P20 ok
summary(cors)


# only uncorrelated variables were elev.skewness and Elev.P20  --> going to remove correlated variables and re-run the model --> 
# goal is to get three more uncorrelated variables
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, 
            Profile.area, RP60, RP50, Elev.P50, RP40, Elev.L.skewness, Elev.P40, Elev.L3)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(5, 20)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1000, min_n = 8

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be blue, RP30, Elev.skewness, RP75, Elev.L1, RP70, Elev.SQRT.mean.SQ, RP20, RP80, Elev.L.CV

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
# will go until find two more variables
cors <- lm(ModelData$blue ~ ModelData$RP30)   # corr = 0.18 --> RP30 ok
summary(cors)

cors <- lm(ModelData$RP30 ~ ModelData$Elev.skewness)   # corr = 0.78 --> remove Elev.skewness
summary(cors)

cors <- lm(ModelData$RP30 ~ ModelData$RP75)   # corr = 0.59--> ok
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$blue)   # corr = 0.17 --> RP75 ok
summary(cors)

cors <- lm(ModelData$RP30 ~ ModelData$Elev.L1)   # corr = 0.84-->  remove Elev.L1
summary(cors)

cors <- lm(ModelData$RP70 ~ ModelData$RP75)   # corr = 0.94 --> remove RP70
summary(cors)

cors <- lm(ModelData$Elev.SQRT.mean.SQ ~ ModelData$RP75)   # corr = 0.73 -->  remove Elev.SQRT.mean.SQ
summary(cors)

cors <- lm(ModelData$RP30 ~ ModelData$RP20)   # corr = 0.89 --> remove RP20 
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$RP80)   # corr = 0.94 --> remove RP80
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$Elev.L.CV)   # corr = 0.544 --> ok
summary(cors)

cors <- lm(ModelData$RP30 ~ ModelData$Elev.L.CV)   # corr = 0.93 --> remove Elev.L.CV
summary(cors)


# don't have 5 total uncorrelated variable yet --> removing correlated variables and running model again
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, 
            Elev.skewness, Elev.L1, RP70, Elev.SQRT.mean.SQ, RP20, RP80, Elev.L.CV, 
            Profile.area, RP60, RP50, Elev.P50, RP40, Elev.L.skewness, Elev.P40, Elev.L3)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(5, 20)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1000, min_n = 8

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be RP75, Canopy.relief.ratio, Elev.P70, blue, RP30, Elev.P60, Elev.mean, Int.kurtosis, Int.L1, Elev.CV

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
# will go until find two more variables
cors <- lm(ModelData$RP75 ~ ModelData$Canopy.relief.ratio)   # corr = 0.72 --> remove Canopy.relief.ratio
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$Elev.P70)   # corr = 0.87 --> remove Elev.P70
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$blue)   # corr = 0.17--> blue ok
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$RP30)   # corr = 0.59 -->  ok 
summary(cors)

cors <- lm(ModelData$blue~ ModelData$RP30)   # corr = 0.18--> RP30 ok
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$Elev.P60)   # corr = 0.74 --> remove Elev.P60
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$Elev.mean)   # corr = 0.73 --> remove Elev.mean
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$Int.kurtosis)   # corr = 0.46 --> ok
summary(cors)

cors <- lm(ModelData$RP30 ~ ModelData$Int.kurtosis)   # corr = 0.16 --> ok
summary(cors)

cors <- lm(ModelData$blue ~ ModelData$Int.kurtosis)   # corr = 0.01 --> Int.kurtosis ok
summary(cors)

cors <- lm(ModelData$Int.L1 ~ ModelData$Int.kurtosis)   # corr = 0.60 -->  ok
summary(cors)

cors <- lm(ModelData$RP75 ~ ModelData$Int.L1)   # corr = 0.01 -->  ok
summary(cors)

cors <- lm(ModelData$RP30 ~ ModelData$Int.L1)   # corr = 0.01 -->  ok
summary(cors)

cors <- lm(ModelData$blue ~ ModelData$Int.L1)   # corr = 0.001 --> Int.L1 ok
summary(cors)



# Final top 5 uncorrelated variables: RP75, blue, RP30, Int.kurtosis, Int.L1

# re-running model with those variables only
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness,
            Int.AAD, Int.L2, Int.L3, Int.L4, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
            Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
            Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
            Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP25, RP40, RP60, RP70,
            RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.L.skewness, Elev.P01, 
            cigreen, cirededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
            red, green,  Elev.P99, RP80, nvdinir, Int.L.CV, RP20, Elev.L3, RP90, 
            RP50, ciredrededge, RP10)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 3)),
  min_n(range = c(5, 20)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1000, min_n = 20

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be RP75, blue, RP30, Int.L1, Int.kurtosis


# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
              Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
              BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness,
              Int.AAD, Int.L2, Int.L3, Int.L4, Int.L.skewness, Int.L.kurtosis,
              Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
              Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
              Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
              Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP25, RP40, RP60, RP70,
              RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.L.skewness, Elev.P01, 
              cigreen, cirededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
              red, green,  Elev.P99, RP80, nvdinir, Int.L.CV, RP20, Elev.L3, RP90, 
              RP50, ciredrededge, RP10)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 87%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 8%



#### 10-fold cross-validation on the MS and lidar; OESF; small cylinder model ####
LidarModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics.csv")
MSModelData <- read.csv("data/MS_Data/all_trees_w_MS_values_cleaned_for_thesis.csv")

# MS data also doesn't have coverage for two trees --> removing those from lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "6_37_THPL" | LidarModelData$Tree.ID == "6_38_THPL"),]

# MS data also had a few trees with problems --> removed those at outliers/ones not computed correctly --> need to remove those in the lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "15_123_ACCI" | LidarModelData$Tree.ID == "17_113_THPL" | LidarModelData$Tree.ID == "20_9_THPL" | LidarModelData$Tree.ID == "20_30_TSHE" | LidarModelData$Tree.ID == "18_21_ALRU"),]

# looks like there were two trees (9_41_THPL and 9_206_THPL that were dropped from the lidar data at some point --> removing that from MS data)
MSModelData <- MSModelData[-which(MSModelData$Tree.ID == "9_41_THPL" | MSModelData$Tree.ID == "9_206_THPL"),]

# removing columns in the MS data that have the same/similar column in lidar for easier merging
MSModelData <- subset(MSModelData, select = -c(X, Elevation, Total.Height, DBH, Status.Code, Species, Plot_Num))

# combining the two data sets
ModelData <- merge(x = LidarModelData, y = MSModelData, by = "Tree.ID")


# Smaller/shorter trees in MS data were impacted by shadows --> removing trees under 20cm in OESF data and under 30cm in ONF data
ModelData[ , 'Site'] = NA

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Num[i] < 15) {
    ModelData$Site[i] <- "OESF"
  } else {
    ModelData$Site[i] <- "ONF"
  }
}

ModelData <- rbind(ModelData[which(ModelData$Site == "OESF" & ModelData$DBH > 0.20),], ModelData[which(ModelData$Site == "ONF" & ModelData$DBH > 0.30),])

# subsetting to only ONF site
ModelData <- ModelData[which(ModelData$Site == "OESF"),]

# removing site as variable so don't have to worry about removing it as variable later
ModelData <- subset(ModelData, select = -c(Site))

# need to filter out "failed" trees (trees with total lidar returns less than 450 for small cylinder trees --> those tops were clearly missed and aren't actually trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 450), ]

# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")
# so few not THPL or TSHE --> going to combine all not THPL into one species category
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)

# resampling / cross validation (doing the folds)
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 13)),
  min_n(range = c(1, 15)),
  levels = 5 
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 10, trees = 1,000, min_n = 4

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.L.skewness, blue, ciredrededge, green, RP90, Elev.MAD.mode, RP80, Int.AAD, Elev.P95, Elev.P99

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )

cors <- lm(ModelData$Elev.L.skewness ~ ModelData$blue)   # corr = 0.11 --> blue ok
summary(cors)

cors <- lm(ModelData$blue ~ ModelData$ciredrededge)   # corr = 0.60 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.L.skewness ~ ModelData$ciredrededge)   # corr = 0.10 --> ciredrededge ok
summary(cors)

cors <- lm(ModelData$Elev.L.skewness ~ ModelData$RP90)   # corr = 0.29 --> ok
summary(cors)

cors <- lm(ModelData$blue ~ ModelData$RP90)   # corr = 0.004 -->  ok
summary(cors)

cors <- lm(ModelData$ciredrededge ~ ModelData$RP90)   # corr = 0.03 --> RP90 ok
summary(cors)

cors <- lm(ModelData$blue ~ ModelData$green)   # corr = 0.89 --> remove green
summary(cors)

cors <- lm(ModelData$Elev.L.skewness ~ ModelData$Elev.MAD.mode)   # corr = 0.10 --> ok
summary(cors)

cors <- lm(ModelData$RP90 ~ ModelData$Elev.MAD.mode)   # corr = 0.04 --> ok
summary(cors)

cors <- lm(ModelData$blue ~ ModelData$Elev.MAD.mode)   # corr = 0.08 --> ok
summary(cors)

cors <- lm(ModelData$ciredrededge ~ ModelData$Elev.MAD.mode)   # corr = 0.14 --> Elev.MAD.mode ok
summary(cors)


# final variables: Elev.L.skewness, blue, ciredrededge, RP90, Elev.MAD.mode


# re-running model with those variables only
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
            Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
            Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
            Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP10, RP25, RP30, RP40, RP60, RP70,
            RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.P01, 
            cigreen, cirededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
            red, green, RP80, nvdinir, Int.L.CV, RP20, Elev.L3, RP50, RP75,
            Elev.P99, Elev.P90)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 3)),
  min_n(range = c(5, 20)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1000, min_n = 5

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be blue, Elev.L.skewness, ciredrededge, Elev.MAD.mode, RP90

# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
              Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
              BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
              Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.skewness, Int.L.kurtosis,
              Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
              Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
              Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
              Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP10, RP25, RP30, RP40, RP60, RP70,
              RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.P01, 
              cigreen, cirededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
              red, green, RP80, nvdinir, Int.L.CV, RP20, Elev.L3, RP50, RP75,
              Elev.P99, Elev.P90)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 81%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 15%


#### 10-fold cross-validation on the lidar only, OESF, small cylinder model ####
# reading in data
ModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics.csv")
# have to add in data for plots 11, 12, 13 (ran processing separately)
ModelData_plots111213 <- read.csv("data/LidarMetrics/FieldTrees/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics_plots11_12_13.csv")
ModelData <- rbind(ModelData_plots111213, ModelData)

# adding in site
ModelData[ , 'Site'] = NA

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Num[i] < 15) {
    ModelData$Site[i] <- "OESF"
  } else {
    ModelData$Site[i] <- "ONF"
  }
}
ModelData <- ModelData[which(ModelData$Site == "OESF"), ]


ModelData_WH_DF <- read.csv("data/LidarMetrics/FieldTrees/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics_DF_WH.csv")
head(ModelData) # glimpse of data
head(ModelData_WH_DF) # glimpse of data --> need to combine with Model Data

# renaming a couple of the ModelData_WH_DF columns names to better match the ModelData names
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Xfield"] <- "X"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Yfield"] <- "Y"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseX"] <- "BaseX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseY"] <- "BaseY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseElevation"] <- "BaseElevation"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveHeight"] <- "Height"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopX"] <- "TopX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopY"] <- "TopY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveCrownDia"] <- "CrownDia"

# adding 100 to each of the WH/DF plot numbers so can later easily separate plots based on year
ModelData_WH_DF$Plot_Number <- ModelData_WH_DF$Plot_Number + 100

# new dataframes with only the column names that are the same between the dataframes
ModelData_WH_DF <- ModelData_WH_DF[ , colnames(ModelData_WH_DF) %in% colnames(ModelData)]
ModelData <- ModelData[ , colnames(ModelData) %in% colnames(ModelData_WH_DF)]

# combining the two datasets 
ModelData <- rbind(ModelData, ModelData_WH_DF)

# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")

# THPL vs. Other
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

# double checking that worked
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)

# need to filter out "failed" trees (trees with total lidar returns less than 450 for small cylinder trees --> those tops were clearly missed and aren't actually trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 450), ]

# adding in year --> then normalizing intenisty data based on year
ModelData[ , 'Year'] = NA

ModelData$Plot_Number <- as.integer(ModelData$Plot_Number)

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Number[i] == 11 | ModelData$Plot_Number[i] == 12 | ModelData$Plot_Number[i] == 13) {
    ModelData$Year[i] <- "2021"
  } else {
    if(ModelData$Plot_Number[i] > 30) {
      ModelData$Year[i] <- "2021" } else {
        ModelData$Year[i] <- "2023"
      }
  }
}

# normalizing mean intensity data
ModelData[which(ModelData$Year == "2021"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2021"), "Int.mean"] - 192.6818) / 17.02009777
ModelData[which(ModelData$Year == "2023"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2023"), "Int.mean"] - 119.895) / 57.01547338


# resampling / cross validation (doing the folds)
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis, 
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.mode))

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(5, 15)),
  min_n(range = c(0, 10)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 15, trees = 1000, min_n = 2

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P90, Elev.P99, Elev.P95, Elev.P80, Elev.P75, Elev.P70, Elev.MAD.mode, Int.mean, RP95, Elev.P50

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P99)   # corr = 0.99 --> remove Elev.P99
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P95)   # corr = 0.99 --> remove Elev.P95
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P75)   # corr = 0.99 --> remove Elev.P75
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P80)   # corr = 0.99 --> remove Elev.P80
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P70)   # corr = 0.99 --> remove Elev.P70 
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.MAD.mode)   # corr = 0.04 --> Elev.MAD.mode ok
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Int.mean)   # corr = 0.002 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.MAD.mode ~ ModelData$Int.mean)   # corr = 0.001 -->  Int.mean ok
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P50)   # corr = 0.99 --> remove Elev.P50
summary(cors)


# Elev.P99 was correlated with every other variable except Int.mean and Elev.MAD.mode--> going to remove those variables and re-run the model

set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis, 
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, 
            Elev.P99, Elev.P95, Elev.P80, Elev.P75, Elev.P70, RP95, Elev.P50)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(2, 10)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 10, trees = 1000, min_n = 10

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P90, Elev.mode, Elev.P60, Elev.SQRT.mean.SQ, Elev.CURT.mean.CUBE, Elev.mean, Elev.L1, Elev.P40, Elev.P30, Elev.MAD.mode


# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.mode)   # corr = 0.97 -->  remove Elev.mode
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P60)   # corr = 0.99 --> remove Elev.P60
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.SQRT.mean.SQ)   # corr = 0.99 --> remove Elev.SQRT.mean.SQ
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.CURT.mean.CUBE)   # corr = 0.99 --> remove Elev.CURT.mean.CUBE
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.mean)   # corr = 0.98 --> remove Elev.mean
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.L1)   # corr = 0.98 --> remove Elev.L1
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P40)   # corr = 0.98 --> remove Elev.P40
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P30)   # corr = 0.97 --> remove Elev.P30
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.MAD.mode)   # corr = 0.04 -->  Elev.MAD.mode ok
summary(cors)

# only uncorrelated variable Elev.MAD.mode  --> going to remove all of the ones found here from the model and run it again
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis, 
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, 
            Elev.mode, Elev.P60, Elev.SQRT.mean.SQ, Elev.CURT.mean.CUBE, Elev.mean, Elev.L1, Elev.P40, Elev.P30,
            Elev.P99, Elev.P95, Elev.P80, Elev.P75, Elev.P70, RP95, Elev.P50
  )) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(0, 20)),
  min_n(range = c(0, 10)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 20, trees = 1000, min_n = 2

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P90, Elev.P25, Elev.P20, Elev.MAD.mode, Int.mean, Elev.P10, RP90, Elev.L4, Elev.P05, Elev.AAD


# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
# will go until have top 5 predictors total
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )
cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P25)   # corr = 0.96 -->   remove Elev.P25
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P20)   # corr = 0.94 --> remove Elev.P20
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.MAD.mode)   # corr = 0.05 --> Elev.MAD.mode ok
summary(cors)

cors <- lm(ModelData$Int.mean ~ ModelData$Elev.MAD.mode)   # corr = 0.001 -->  Int.mean ok
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.P10)   # corr = 0.84 --> Remove Elev.P10
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$RP90)   # corr = 0.2 --> ok
summary(cors)

cors <- lm(ModelData$RP90 ~ ModelData$Elev.MAD.mode)   # corr = 0.004 -> ok
summary(cors)

cors <- lm(ModelData$RP90 ~ ModelData$Int.mean)   # corr = 0.003 --> RP90 ok
summary(cors)

cors <- lm(ModelData$Elev.MAD.mode ~ ModelData$Elev.L4)   # corr = 0.005 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.P90 ~ ModelData$Elev.L4)   # corr = 0.19 --> ok
summary(cors)

cors <- lm(ModelData$RP90 ~ ModelData$Elev.L4)   # corr = 0.06 -->  ok
summary(cors)

cors <- lm(ModelData$Int.mean ~ ModelData$Elev.L4)   # corr = 0.007 -->  Elev.L4 ok
summary(cors)


# Top 5 variables: Elev.P90, Elev.MAD.mode, Int.mean, Elev.L4, RP90

# re-running model with those variables only
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
            Elev.AAD, Elev.MAD.median, Elev.L1, Elev.L2, Elev.L.CV, 
            Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
            Elev.P60, Elev.P70, Elev.P80, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, Elev.L3, RP05, RP10, RP20, RP25, RP30, RP40, RP60, RP70, RP75,
            RP95, RP01, RP75, RP50, RP80,
            Elev.P01, Elev.stddev,
            Elev.P75, Elev.P05, Elev.kurtosis, Elev.variance,
            Elev.P99, Elev.L.skewness)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 3)),
  min_n(range = c(5, 20)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1000, min_n = 8

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P90, Elev.MAD.mode, Elev.L4, Int.mean, RP90



# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(BaseX, BaseY, Year,
              BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
              Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
              Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
              Elev.AAD, Elev.MAD.median, Elev.L1, Elev.L2, Elev.L.CV, 
              Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
              Elev.P60, Elev.P70, Elev.P80, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, Elev.L3, RP05, RP10, RP20, RP25, RP30, RP40, RP60, RP70, RP75,
              RP95, RP01, RP75, RP50, RP80,
              Elev.P01, Elev.stddev,
              Elev.P75, Elev.P05, Elev.kurtosis, Elev.variance,
              Elev.P99, Elev.L.skewness)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 82%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 7%


#### 10-fold cross-validation on the lidar only, both sites, top 3m ####
# reading in data
ModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics.csv")
ModelData_WH_DF <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics_DF_WH.csv")
head(ModelData) # glimpse of data
head(ModelData_WH_DF) # glimpse of data --> need to combine with Model Data

# renaming a couple of the ModelData_WH_DF columns names to better match the ModelData names
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Xfield"] <- "X"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Yfield"] <- "Y"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseX"] <- "BaseX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseY"] <- "BaseY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseElevation"] <- "BaseElevation"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveHeight"] <- "Height"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopX"] <- "TopX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopY"] <- "TopY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveCrownDia"] <- "CrownDia"

# adding 100 to each of the WH/DF plot numbers so can later easily separate plots based on year
ModelData_WH_DF$Plot_Number <- ModelData_WH_DF$Plot_Number + 100

# new dataframes with only the column names that are the same between the dataframes
ModelData_WH_DF <- ModelData_WH_DF[ , colnames(ModelData_WH_DF) %in% colnames(ModelData)]
ModelData <- ModelData[ , colnames(ModelData) %in% colnames(ModelData_WH_DF)]

# combining the two datasets 
ModelData <- rbind(ModelData, ModelData_WH_DF)

# need to filter out "failed" trees (trees with total lidar returns less than 300 --> those tops were clearly missedd and aren't acutally trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 300), ]

# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")

# THPL vs. Other
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

# double checking that worked
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)


# adding in year --> then normalizing intenisty data based on year
ModelData[ , 'Year'] = NA

ModelData$Plot_Number <- as.integer(ModelData$Plot_Number)

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Number[i] == 11 | ModelData$Plot_Number[i] == 12 | ModelData$Plot_Number[i] == 13) {
    ModelData$Year[i] <- "2021"
  } else {
    if(ModelData$Plot_Number[i] > 30) {
      ModelData$Year[i] <- "2021" } else {
        ModelData$Year[i] <- "2023"
      }
  }
}

# normalizing intensity data
ModelData[which(ModelData$Year == "2021"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2021"), "Int.mean"] - 192.6818) / 17.02009777
ModelData[which(ModelData$Year == "2023"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2023"), "Int.mean"] - 119.895) / 57.01547338


# resampling / cross validation (doing the folds)
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(10, 20)),
  min_n(range = c(10, 25)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 20, trees = 1000, min_n = 25

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Int.mean, RP80, RP70, RP40, Elev.skewness, RP60, RP75, RP90, RP95, Elev.L.skewness

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
# need to add the RPs as actual variables in the model data so that can calculate correlation
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )
cors <- lm(ModelData$Int.mean ~ ModelData$RP80)   # corr = 0.009--> RP80 ok
summary(cors)

cors <- lm(ModelData$RP80 ~ ModelData$RP70)   # corr = 0.84 --> remove RP70
summary(cors)

cors <- lm(ModelData$RP80 ~ ModelData$RP40)   # corr = 0.55 --> ok
summary(cors)

cors <- lm(ModelData$Int.mean ~ ModelData$RP40)   # corr = 0.025 --> RP40 ok
summary(cors)

cors <- lm(ModelData$RP80 ~ ModelData$Elev.skewness)   # corr = 0.707 --> remove Elev.skewness
summary(cors)

cors <- lm(ModelData$RP80 ~ ModelData$RP60)   # corr = 0.71 --> remove RP60
summary(cors)

cors <- lm(ModelData$RP80 ~ ModelData$RP75)   # corr = 0.93 --> remove RP75
summary(cors)

cors <- lm(ModelData$RP80 ~ ModelData$RP90)   # corr = 0.64 --> ok
summary(cors)

cors <- lm(ModelData$RP40 ~ ModelData$RP90)   # corr = 0.37 --> ok
summary(cors)

cors <- lm(ModelData$Int.mean ~ ModelData$RP90)   # corr = 0.01 --> RP90 ok
summary(cors)

cors <- lm(ModelData$RP90 ~ ModelData$RP95)   # corr = 0.61 --> ok
summary(cors)

cors <- lm(ModelData$RP80 ~ ModelData$RP95)   # corr = 0.33 -->  ok
summary(cors)

cors <- lm(ModelData$RP40 ~ ModelData$RP95)   # corr = 0.18 -->  ok
summary(cors)

cors <- lm(ModelData$Int.mean ~ ModelData$RP95)   # corr = 0.009 -->  RP95 ok
summary(cors)


# final list of variables to use in final model: Int.mean, RP40, RP90, RP80, RP95


# re-running model with those variables only
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.stddev, Elev.CV, Elev.IQ, 
            Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
            Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
            Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, Elev.L3, RP05, RP10, RP20, RP25, RP30, RP60, RP75,
            Elev.P99, Elev.P05, Elev.P01, Elev.variance, Elev.L.skewness, RP01, RP50, RP70)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(2, 3)),
  min_n(range = c(12, 30)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 2, trees = 1000, min_n = 25

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Int.mean, RP80, RP40, RP90, RP95


# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(BaseX, BaseY, Year,
              BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
              Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
              Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.stddev, Elev.CV, Elev.IQ, 
              Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
              Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
              Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, Elev.L3, RP05, RP10, RP20, RP25, RP30, RP60, RP75,
              Elev.P99, Elev.P05, Elev.P01, Elev.variance, Elev.L.skewness, RP01, RP50, RP70)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 78%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 4%


#### 10-fold cross-validation on the lidar only, both sites, small cylinder ####
# reading in data
ModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics.csv")
# have to add in data for plots 11, 12, 13 (ran processing separately)
ModelData_plots111213 <- read.csv("data/LidarMetrics/FieldTrees/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics_plots11_12_13.csv")
ModelData <- rbind(ModelData_plots111213, ModelData)
ModelData_WH_DF <- read.csv("data/LidarMetrics/FieldTrees/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics_DF_WH.csv")
head(ModelData) # glimpse of data
head(ModelData_WH_DF) # glimpse of data --> need to combine with Model Data

# renaming a couple of the ModelData_WH_DF columns names to better match the ModelData names
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Xfield"] <- "X"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "Yfield"] <- "Y"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseX"] <- "BaseX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseY"] <- "BaseY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveBaseElevation"] <- "BaseElevation"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveHeight"] <- "Height"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopX"] <- "TopX"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveTopY"] <- "TopY"
names(ModelData_WH_DF)[names(ModelData_WH_DF) == "aveCrownDia"] <- "CrownDia"

# adding 100 to each of the WH/DF plot numbers so can later easily separate plots based on year
ModelData_WH_DF$Plot_Number <- ModelData_WH_DF$Plot_Number + 100

# new dataframes with only the column names that are the same between the dataframes
ModelData_WH_DF <- ModelData_WH_DF[ , colnames(ModelData_WH_DF) %in% colnames(ModelData)]
ModelData <- ModelData[ , colnames(ModelData) %in% colnames(ModelData_WH_DF)]

# combining the two datasets 
ModelData <- rbind(ModelData, ModelData_WH_DF)

# need to filter out "failed" trees (trees with total lidar returns less than 450 for small cylinder trees --> those tops were clearly missed and aren't actually trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 450), ]

# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")

# THPL vs. Other
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

# double checking that worked
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)

# adding in year --> then normalizing intenisty data based on year
ModelData[ , 'Year'] = NA

ModelData$Plot_Number <- as.integer(ModelData$Plot_Number)

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Number[i] == 11 | ModelData$Plot_Number[i] == 12 | ModelData$Plot_Number[i] == 13) {
    ModelData$Year[i] <- "2021"
  } else {
    if(ModelData$Plot_Number[i] > 30) {
      ModelData$Year[i] <- "2021" } else {
        ModelData$Year[i] <- "2023"
      }
  }
}

# normalizing mean intensity data
ModelData[which(ModelData$Year == "2021"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2021"), "Int.mean"] - 192.6818) / 17.02009777
ModelData[which(ModelData$Year == "2023"), "Int.mean"] <- (ModelData[which(ModelData$Year == "2023"), "Int.mean"] - 119.895) / 57.01547338


# resampling / cross validation (doing the folds)
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis, 
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(5, 15)),
  min_n(range = c(0, 10)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 12, trees = 1000, min_n = 7

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P05, ELev.P99, Elev.P10, Elev.P75, Elev.P95, Elev.P80, ELev.P90, Elev.P70, Elev.L2, Elev.P50

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )


cors <- lm(ModelData$Elev.P05 ~ ModelData$Elev.P99)   # corr = 0.55 --> Elev.P99 ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P10)   # corr = 0.73 --> remove Elev.P10
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P75)   # corr = 0.99 --> remove Elev.P75
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P95)   # corr = 0.99 --> remove Elev.P95 
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P80)   # corr = 0.99 --> remove Elev.P80
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P90)   # corr = 0.99 --> remove Elev.P90
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P70)   # corr = 0.99 --> remove Elev.P70
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.L2)   # corr = 0.33 --> ok
summary(cors)

cors <- lm(ModelData$Elev.P05 ~ ModelData$Elev.L2)   # corr = 0.006 --> Elev.L2 ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P50)   # corr = 0.98 --> remove Elev.P50
summary(cors)


# only got three variables (Elev.P99, Elev.P05, and Elev.L2) --> re-running model with correlated variables removed to get three more

set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis, 
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, 
            Elev.P10, Elev.P75, Elev.P95, Elev.P80, Elev.P90, Elev.P70, Elev.P50
  )) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(10, 20)),
  min_n(range = c(1, 10)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 15, trees = 1000, min_n = 1

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be EElev.P05, Elev.P99, Elev.P60, Elev.P40, Elev.CURT.mean.CUBE, Elev.L2, Elev.mode, Elev.P01, RP95, Elev.SQRT.mean.SQ


# Checking those above top 10 variables for correlations --> will do until have another predictor (so have 5 total)
cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P60)   # corr = 0.98 --> remove Elev.P60
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P40)   # corr = 0.97 --> remove Elev.P40
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.mode)   # corr = 0.97 --> remove Elev.mode
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.CURT.mean.CUBE)   # corr = 0.98 -->  remove Elev.CURT.mean.CUBE
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.mode)   # corr = 0.97-->  remove ELev.mode
summary(cors)

cors <- lm(ModelData$Elev.P05 ~ ModelData$Elev.P01)   # corr = 0.55 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P01)   # corr = 0.19 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.L2 ~ ModelData$Elev.P01)   # corr = 0.06 -->   ok
summary(cors)

cors <- lm(ModelData$Elev.L2 ~ ModelData$Elev.P01)   # corr = 0.06 -->  Elev.P01 ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$RP95)   # corr = 0.06 -->   ok
summary(cors)

cors <- lm(ModelData$Elev.P05 ~ ModelData$RP95)   # corr = 0.11 -->   ok
summary(cors)

cors <- lm(ModelData$Elev.P01 ~ ModelData$RP95)   # corr = 0.04 -->   ok
summary(cors)

cors <- lm(ModelData$Elev.L2 ~ ModelData$RP95)   # corr = 0.001 -->  RP95 ok
summary(cors)


# final variables are: Elev.P99, Elev.P05, Elev.L2, Elev.P01, RP95

set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(BaseX, BaseY, Year,
            BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.stddev, Elev.CV, 
            Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L.CV, 
            Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
            Elev.P60, Elev.P70, Elev.P75, Elev.P90, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Profile.area, Elev.L3, RP10, RP20, RP25, RP30, RP40, RP60, RP70, RP75,
            Elev.variance, Elev.L.skewness, RP01, RP50, RP80, Elev.P80, Elev.skewness, Elev.P95, RP90, Elev.CURT.mean.CUBE,
            Elev.AAD, RP05, Elev.kurtosis, Elev.IQ, Int.mean, Elev.L4)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)


# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(2, 4)),
  min_n(range = c(5, 20)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 3, trees = 1000, min_n = 12

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P99, Elev.P05, Elev.L2, RP95, Elev.P01



# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, topElevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(BaseX, BaseY, Year,
              BaseElevation, Height, CrownDia, TopX, TopY, DataFile,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
              Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
              Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.stddev, Elev.CV, 
              Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L.CV, 
              Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
              Elev.P60, Elev.P70, Elev.P75, Elev.P90, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Profile.area, Elev.L3, RP10, RP20, RP25, RP30, RP40, RP60, RP70, RP75,
              Elev.variance, Elev.L.skewness, RP01, RP50, RP80, Elev.P80, Elev.skewness, Elev.P95, RP90, Elev.CURT.mean.CUBE,
              Elev.AAD, RP05, Elev.kurtosis, Elev.IQ, Int.mean, Elev.L4)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 81%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 3%

#### 10-fold cross-validation on the MS and lidar, both sites, top 3m ####
LidarModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_TreeTops_SmallCylinder_normalized_ConeClip_metrics.csv")
MSModelData <- read.csv("data/MS_Data/all_trees_w_MS_values_cleaned_for_thesis.csv")

# lidar data covers plots 11, 12, 13 but MS does not --> need to remove those plots in the lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Plot_Number == 11 | LidarModelData$Plot_Number == 12 |LidarModelData$Plot_Number == 13),]

# MS data also doesn't have coverage for two trees --> removing those from lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "6_37_THPL" | LidarModelData$Tree.ID == "6_38_THPL"),]

# MS data also had a few trees with problems --> removed those at outliers/ones not computed correctly --> need to remove those in the lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "15_123_ACCI" | LidarModelData$Tree.ID == "17_113_THPL" | LidarModelData$Tree.ID == "20_9_THPL" | LidarModelData$Tree.ID == "20_30_TSHE" | LidarModelData$Tree.ID == "18_21_ALRU"),]

# looks like there were two trees (9_41_THPL and 9_206_THPL that were dropped from the lidar data at some point --> removing that from MS data)
MSModelData <- MSModelData[-which(MSModelData$Tree.ID == "9_41_THPL" | MSModelData$Tree.ID == "9_206_THPL"),]

# removing columns in the MS data that have the same/similar column in lidar for easier merging
MSModelData <- subset(MSModelData, select = -c(X, Elevation, Total.Height, DBH, Status.Code, Species, Plot_Num))

# combining the two data sets
ModelData <- merge(x = LidarModelData, y = MSModelData, by = "Tree.ID")

# Smaller/shorter trees in MS data were impacted by shadows --> removing trees under 20cm in OESF data and under 30cm in ONF data
ModelData[ , 'Site'] = NA

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Num[i] < 15) {
    ModelData$Site[i] <- "OESF"
  } else {
    ModelData$Site[i] <- "ONF"
  }
}

ModelData <- rbind(ModelData[which(ModelData$Site == "OESF" & ModelData$DBH > 0.20),], ModelData[which(ModelData$Site == "ONF" & ModelData$DBH > 0.30),])
# removing site as variable so don't have to worry about removing it as variable later
ModelData <- subset(ModelData, select = -c(Site))

# need to filter out "failed" trees (trees with total lidar returns less than 300 --> those tops were clearly missedd and aren't acutally trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 300), ]


# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")
# so few not THPL or TSHE --> going to combine all not THPL into one species category
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)

# resampling / cross validation (doing the folds)
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(5, 20)),
  min_n(range = c(0, 10)),
  levels = 5 
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 16, trees = 1,000, min_n = 7

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be RP50, RP40, Elev.P99, Elev.L3, Elev.L.skewness, nvdinir, msr, Elev.skewness, Elev.CV, Elev.P40

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )

cors <- lm(ModelData$RP50 ~ ModelData$RP40)   # corr = 0.92 --> remove RP40
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$RP50)   # corr = 0.006 --> Elev.P99 ok
summary(cors)

cors <- lm(ModelData$RP50 ~ ModelData$Elev.L3)   # corr = 0.9 --> remove Elev.L3
summary(cors)

cors <- lm(ModelData$RP50 ~ ModelData$Elev.L.skewness)   # corr = 0.91 --> remove Elev.L skewness
summary(cors)

cors <- lm(ModelData$RP50 ~ ModelData$nvdinir)   # corr = 0.91 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$nvdinir)   # corr = 0.03 --> nvdinir ok
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$msr)   # corr = 0.88 --> remove msr
summary(cors)

cors <- lm(ModelData$RP50 ~ ModelData$Elev.skewness)   # corr = 0.88--> remove Elev.skewness
summary(cors)

cors <- lm(ModelData$RP50 ~ ModelData$Elev.CV)   # corr = 0.79 --> remove Elev.CV
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P40)   # corr = 0.003--> ok
summary(cors)

cors <- lm(ModelData$RP50~ ModelData$Elev.P40)   # corr = 0.9 --> remove ELev.P40
summary(cors)


# only 3 variables were uncorrelated (Elev.P99, RP50, nvdinir) --> going to re-run model without the correlated variables
# (goal is to get two more uncorrelated variables)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Elev.CV, RP50, msr, RP20, RP30, Elev.L3, Elev.L.skewness, 
            RP40, Elev.L3, Elev.L.skewness, msr, Elev.skewness, Elev.CV, Elev.P40)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(5, 20)),
  min_n(range = c(0, 10)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 20, trees = 1000, min_n = 5

# can explore which predictors were most important 
# --> can think about removing less important variables later

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P99, nvdinir, RP60, Profile.area, Elev.P50, RP75, Elev.L.CV, RP70, Elev.P30, Elev.P95

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
# will go until find two more variables
cors <- lm(ModelData$Elev.P99 ~ ModelData$RP60)   # corr = 0.007 -->   RP60 ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Profile.area)   # corr = 0.006 ---> ok
summary(cors)

cors <- lm(ModelData$RP60 ~ ModelData$Profile.area)   # corr = 0.80--> remove Profile.area
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P50)   # corr = 0.001--> ok
summary(cors)

cors <- lm(ModelData$RP60 ~ ModelData$Elev.P50)   # corr = 0.87 --> remove Elev.P50
summary(cors)

cors <- lm(ModelData$RP60 ~ ModelData$RP75)   # corr = 0.82 --> remove RP75
summary(cors)

cors <- lm(ModelData$RP60 ~ ModelData$Elev.L.CV)   # corr = 0.67 --> ok
summary(cors)

cors <- lm(ModelData$Elev.L.CV ~ ModelData$Elev.P99)   # corr = 0.01 -->  ok
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$Elev.L.CV)   # corr = 0.12 -->  Elev.L.CV ok
summary(cors)

cors <- lm(ModelData$RP60 ~ ModelData$RP70)   # corr = 0.92 -->  remove RP70 
summary(cors)

cors <- lm(ModelData$RP60 ~ ModelData$Elev.P30)   # corr = 0.67 -->   ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P30)   # corr = 0.003 -->   ok
summary(cors)

cors <- lm(ModelData$Elev.L.CV ~ ModelData$Elev.P30)   # corr = 0.9 -->   remove ELev.P30
summary(cors)

cors <- lm(ModelData$Elev.L.CV ~ ModelData$Elev.P95)   # corr = 0.05 -->   ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P95)   # corr = 0.4 -->   ok
summary(cors)

cors <- lm(ModelData$RP60 ~ ModelData$Elev.P95)   # corr = 0.09 -->   ok
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$Elev.P95)   # corr = 0.02 -->   Elev.P95 ok
summary(cors)


# final variables: Elev.P99, RP60, nvdinir, Elev.L.CV, Elev.P95

set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
            Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4,
            Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
            Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, Elev.L3, RP01, RP05, RP25, RP30, RP70, RP75,
            RP90, RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.L.skewness, Elev.P01, 
            cigreen, cirededge, ciredrededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
            red, green, blue, RP50, RP20,
            RP40, RP80, RP10)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(3, 5)),
  min_n(range = c(2, 10)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 5, trees = 1000, min_n = 2


# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be RP60, nvdinir, Elev.P99, Elev.L.CV, Elev.P95

# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
              Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
              BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
              Int.AAD, Int.L1, Int.L2, Int.L3, Int.L4, Int.L.CV, Int.L.skewness, Int.L.kurtosis,
              Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
              Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.MAD.mode, Elev.L1, Elev.L2, Elev.L4,
              Elev.L.kurtosis, Elev.P10, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
              Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, Elev.L3, RP01, RP05, RP25, RP30, RP70, RP75,
              RP90, RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.L.skewness, Elev.P01, 
              cigreen, cirededge, ciredrededge, nir, rededge, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
              red, green, blue, RP50, RP20,
              RP40, RP80, RP10)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 81%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 10%




#### 10-fold cross-validation on the MS and lidar, both sites, small cylinder ####
LidarModelData <- read.csv("data/LidarMetrics/FieldTrees/Leaning_Trees_SmallCylinder_normalized_ConeClip_metrics.csv")
MSModelData <- read.csv("data/MS_Data/all_trees_w_MS_values_cleaned_for_thesis.csv")

# MS data also doesn't have coverage for two trees --> removing those from lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "6_37_THPL" | LidarModelData$Tree.ID == "6_38_THPL"),]

# MS data also had a few trees with problems --> removed those at outliers/ones not computed correctly --> need to remove those in the lidar data
LidarModelData <- LidarModelData[-which(LidarModelData$Tree.ID == "15_123_ACCI" | LidarModelData$Tree.ID == "17_113_THPL" | LidarModelData$Tree.ID == "20_9_THPL" | LidarModelData$Tree.ID == "20_30_TSHE" | LidarModelData$Tree.ID == "18_21_ALRU"),]

# looks like there were two trees (9_41_THPL and 9_206_THPL that were dropped from the lidar data at some point --> removing that from MS data)
MSModelData <- MSModelData[-which(MSModelData$Tree.ID == "9_41_THPL" | MSModelData$Tree.ID == "9_206_THPL"),]

# removing columns in the MS data that have the same/similar column in lidar for easier merging
MSModelData <- subset(MSModelData, select = -c(X, Elevation, Total.Height, DBH, Status.Code, Species, Plot_Num))

# combining the two data sets
ModelData <- merge(x = LidarModelData, y = MSModelData, by = "Tree.ID")

# Smaller/shorter trees in MS data were impacted by shadows --> removing trees under 20cm in OESF data and under 30cm in ONF data
ModelData[ , 'Site'] = NA

for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Plot_Num[i] < 15) {
    ModelData$Site[i] <- "OESF"
  } else {
    ModelData$Site[i] <- "ONF"
  }
}

ModelData <- rbind(ModelData[which(ModelData$Site == "OESF" & ModelData$DBH > 0.20),], ModelData[which(ModelData$Site == "ONF" & ModelData$DBH > 0.30),])
# removing site as variable so don't have to worry about removing it as variable later
ModelData <- subset(ModelData, select = -c(Site))


# need to filter out "failed" trees (trees with total lidar returns less than 450 for small cylinder trees --> those tops were clearly missed and aren't actually trees)
ModelData <- ModelData[which(ModelData$Total.return.count > 450), ]

# checking on number of each species data has
barplot(table(ModelData$Species),
        ylab = "Frequency",
        xlab = "Species")
# so few not THPL or TSHE --> going to combine all not THPL into one species category
for (i in 1:length(ModelData$Tree.ID)) {
  if(ModelData$Species[i] == "THPL") {
    ModelData$Species[i] <- "THPL"
  } else {
    ModelData$Species[i] <- "Other"
  }
}

ModelData$Species <- as.factor(ModelData$Species) # making species variable into factor
# using summary for a quick check of data weirdnesses
summary(ModelData)


# resampling / cross validation (doing the folds)
# 10 folds --> 10 different resamplings of training data;
# 9 are used in analysis and 1 (final fold) used in assessment
# this helps with not having RF over estimate own ability
set.seed(42)
folds <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(1, 10)),
  levels = 5 
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1,000, min_n = 10

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P99, ciredrededge, Elev.mean, Elev.P10, Elev.P90, cigreen, nvdinir, Elev.P95, Elev.P50, Elev.P60

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )

cors <- lm(ModelData$Elev.P99 ~ ModelData$ciredrededge)   # corr = 0.26 --> ciredrededge ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.mean)   # corr = 0.97 --> remove Elev.mean
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P10)   # corr = 0.67 -->  ok
summary(cors)

cors <- lm(ModelData$ciredrededge ~ ModelData$Elev.P10)   # corr = 0.12 --> Elev.P10 ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P90)   # corr = 0.99 -->  remove Elev.P90
summary(cors)

cors <- lm(ModelData$ciredrededge ~ ModelData$cigreen)   # corr = 0.74 --> remove cigreen
summary(cors)

cors <- lm(ModelData$ciredrededge ~ ModelData$nvdinir)   # corr = 0.56 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$nvdinir)   # corr = 0.61--> ok
summary(cors)

cors <- lm(ModelData$Elev.P10 ~ ModelData$nvdinir)   # corr = 0.24 -->  nvdinir ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P95)   # corr = 0.99 --> remove Elev.P95
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P50)   # corr = 0.98 --> remove Elev.P50
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P60)   # corr = 0.98 --> remove Elev.60
summary(cors)

# only found 4 uncorrelated variables (Elev.P99, ciredrededge, nvdinir, Elev.P10) --> neeed to re-run model

# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum,
            Elev.mean, Elev.P90, cigreen, Elev.P95, Elev.P50, Elev.P60)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 15)),
  min_n(range = c(1, 10)),
  levels = 5 
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1,000, min_n = 1

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P99, Elev.P70, Elev.P80, Elev.P75, ciredrededge, Elev.P10, Elev.mode, Elev.P20, Elev.CURT.mean.CUBE, nvdinir

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P70)   # corr = 0.99 --> remove Elev.P70
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P80)   # corr = 0.99 --> remove Elev.P80
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P75)   # corr = 0.99 -->  remove Elev.P75
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.mode)   # corr = 0.98 --> remove Elev.mode
summary(cors)

cors <- lm(ModelData$Elev.P10 ~ ModelData$Elev.P20)   # corr = 0.89 -->  remove Elev.P20
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.CURT.mean.CUBE)   # corr = 0.98 --> remove Elev.CURT.mean.CUBE
summary(cors)

# didn't find any more uncorrelated variables --> need to run model again
# building a recipe (modifications to dataset)
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>%
  step_downsample(Species, under_ratio = 1) %>%
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum,
            Elev.mean, Elev.P90, cigreen, Elev.P95, Elev.P50, Elev.P60,
            Elev.P70, Elev.P80, Elev.P75, Elev.mode, Elev.P20, Elev.CURT.mean.CUBE)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 15)),
  min_n(range = c(1, 10)),
  levels = 5 
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1,000, min_n = 5

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P99, Elev.SQRT.mean.SQ, Elev.MAD.mode, ELev.P30, Elev.P10, nvdinir, Elev.L1, Elev.P40, nvdiredrededge, Elev.L.skewness

# Checking those above top 10 variables for correlations --> will remove those with correlations > 0.7
ModelData <- ModelData %>%
  mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  )

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.SQRT.mean.SQ)   # corr = 0.98 --> remove Elev.SQRT.mean.SQ
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.MAD.mode)   # corr = 0.50 --> Elev.MAD.mode ok
summary(cors)

cors <- lm(ModelData$Elev.P30 ~ ModelData$Elev.MAD.mode)   # corr = 0.31 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.P30 ~ ModelData$Elev.P99)   # corr = 0.94 --> remove Elev.P30
summary(cors)

cors <- lm(ModelData$Elev.L1 ~ ModelData$Elev.P99)   # corr = 0.97 -->  remove Elev.L1
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.P40)   # corr = 0.97 --> remove Elev.P40
summary(cors)

cors <- lm(ModelData$nvdiredrededge ~ ModelData$nvdinir)   # corr = 0.87 --> remove nvdiredrededge
summary(cors)

cors <- lm(ModelData$Elev.MAD.mode ~ ModelData$Elev.L.skewness)   # corr = 0.003 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.P99 ~ ModelData$Elev.L.skewness)   # corr = 0.1 -->  ok
summary(cors)

cors <- lm(ModelData$Elev.P10 ~ ModelData$Elev.L.skewness)   # corr = 0.04 -->  ok
summary(cors)

cors <- lm(ModelData$nvdinir ~ ModelData$Elev.L.skewness)   # corr = 0.07 --> Elev.L.skewness ok
summary(cors)

# these are the final uncorrelated variables: Elev.L.skewness, Elev.P10, nvdinir, Elev.P99, Elev.MAD.mode

# re-running model with those variables only
set.seed(42)
ModelRecipe <- recipe(Species ~., data = ModelData) %>% 
  step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
  update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
  step_mutate(
    RP01 = Elev.P01 / Elev.P99, 
    RP05 = Elev.P05 / Elev.P99, 
    RP10 = Elev.P10 / Elev.P99, 
    RP20 = Elev.P20 / Elev.P99, 
    RP25 = Elev.P25 / Elev.P99, 
    RP30 = Elev.P30 / Elev.P99, 
    RP40 = Elev.P40 / Elev.P99, 
    RP50 = Elev.P50 / Elev.P99, 
    RP60 = Elev.P60 / Elev.P99,
    RP70 = Elev.P70 / Elev.P99,
    RP75 = Elev.P75 / Elev.P99,
    RP80 = Elev.P80 / Elev.P99,
    RP90 = Elev.P90 / Elev.P99,
    RP95 = Elev.P95 / Elev.P99
  ) %>%
  step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
            Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
            BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
            Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
            Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
            Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
            Int.AAD, Int.L1, Int.L2, Int.L4, Int.L.skewness, Int.L.kurtosis,
            Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
            Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
            Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
            Elev.L.kurtosis, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
            Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
            Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP10, RP25, RP30, RP40, RP60, RP75,
            RP90, RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.P01, 
            cigreen, cirededge, ciredrededge, nir, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
            red, green, blue, RP50, RP20, Int.L3,
            Elev.L3, rededge, RP70,
            RP80, Int.L.CV)) 

ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
ModelJuiced <- juice(ModelPrep) # creates revised training dataset

# model making and tuning the model by first estimating hyperparameters (mtry and min_n)
tune_specification <- rand_forest(
  mtry = tune(), # number of predictors to sample at each node
  trees = 1000, # number of trees
  min_n = tune() # min number of observations needed at each node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_workflow <- workflow() %>% # workflow function allows you to combine recipe and model specs
  add_recipe(ModelRecipe) %>%
  add_model(tune_specification)

# tune the hyperparameters
# use the cross-validation folds to do so
# uses parallel processing (b/c computationally intense)
# basically, going to run the folds a bunch of times --> see which parameters work best
set.seed(42)
tune_results <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 20 # number of different parameters to be used in tuning combinations
)
tune_results

# looking at AUC-ROC (area under the curve of the receiver operating characteristic)
tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# based on those results, can pull likely hyperparameters (narrowing things down a bit)
rf_grid <- grid_regular(
  mtry(range = c(1, 4)),
  min_n(range = c(5, 25)),
  levels = 5
)
# plug those results back into regular tune
set.seed(42)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid
)

# now graphing those results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# finally, picking from those results the final 'best' results
# can use select_best() function
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_specification,
  best_auc
)

final_rf # best results: mtry = 1, trees = 1000, min_n = 20

# can explore which predictors were most important 

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Species ~ .,
      data = juice(ModelPrep) %>% select(-c(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation))
  ) %>%
  vip(geom = "point")
# most important variables appear to be Elev.P99, Elev.L.skewness, nvdinir, Elev.MAD.mode, Elev.P10


# making final model --> model based on all data; will be used to make predictions on new dataset
final_wf <- workflow() %>%
  add_recipe(ModelRecipe) %>%
  add_model(final_rf)

# Now testing 10-fold cross-validation as a way to test accuracy
set.seed(43)
xfold <- vfold_cv(data = ModelData, v = 10, strata = "Species")

# making empty data frame to put final accuracy data data, because that makes my brain happy
Split_Num <- rep("NA", length(xfold$splits))
Accuracy <- rep("NA", length(xfold$splits))
Pred_Other <- rep("NA", length(xfold$splits))
Pred_THPL <- rep("NA", length(xfold$splits))

vFoldCV_Accuracies <- data.frame(Split_Num, Accuracy, Pred_Other, Pred_THPL)

for (i in 1:length(xfold$splits)) {
  
  # building model (uses same final variables and hyperparameters as model built on all observations)
  set.seed(42)
  Training_Data <- training(xfold$splits[[i]])
  Testing_Data <- testing(xfold$splits[[i]]) 
  ModelRecipe <- recipe(Species ~., data = Training_Data) %>% 
    step_downsample(Species, under_ratio = 1) %>% # down-sampling so that "Other" is equal to THPL count
    update_role(Plot_Number, Tag_Num, Tree.ID, X, Y, DBH, Status.Code, Elevation, new_role = "ID") %>%
    step_mutate(
      RP01 = Elev.P01 / Elev.P99, 
      RP05 = Elev.P05 / Elev.P99, 
      RP10 = Elev.P10 / Elev.P99, 
      RP20 = Elev.P20 / Elev.P99, 
      RP25 = Elev.P25 / Elev.P99, 
      RP30 = Elev.P30 / Elev.P99, 
      RP40 = Elev.P40 / Elev.P99, 
      RP50 = Elev.P50 / Elev.P99, 
      RP60 = Elev.P60 / Elev.P99,
      RP70 = Elev.P70 / Elev.P99,
      RP75 = Elev.P75 / Elev.P99,
      RP80 = Elev.P80 / Elev.P99,
      RP90 = Elev.P90 / Elev.P99,
      RP95 = Elev.P95 / Elev.P99
    ) %>%
    step_rm(c(Total.Height, Height.To.Crown.Base, Max.Crown.Diameter, Min.Crown.Diameter,
              Crown.Rotation, R, G, B, Lean.Angle.From.Vertical, Lean.Azimuth, BaseX, BaseY,
              BaseElevation, Height, topElevation, CrownDia, TopX, TopY, DataFile, FileTitle,
              Total.return.count, Return.1.count, Return.2.count, Return.3.count, Return.4.count, 
              Return.5.count, Return.6.count, Return.7.count, Return.8.count, Return.9.count, Other.return.count,
              Elev.minimum, Elev.maximum, Int.minimum, Int.maximum, Int.mode, Int.stddev, Int.variance, Int.CV, Int.IQ, Int.skewness, Int.kurtosis,
              Int.AAD, Int.L1, Int.L2, Int.L4, Int.L.skewness, Int.L.kurtosis,
              Int.P01, Int.P05, Int.P10, Int.P20, Int.P25, Int.P30, Int.P40, Int.P50, Int.P60, Int.P70, 
              Int.P75, Int.P80, Int.P90, Int.P95, Int.P99, Elev.minimum, Elev.maximum, Elev.mean, Elev.mode, Elev.CV, Elev.IQ, 
              Elev.kurtosis, Elev.AAD, Elev.MAD.median, Elev.L1, Elev.L2, Elev.L4, Elev.L.CV, 
              Elev.L.kurtosis, Elev.P20, Elev.P25, Elev.P30, Elev.P40, Elev.P50,
              Elev.P60, Elev.P70, Elev.P75, Elev.P80, Elev.P90, Elev.P95, Canopy.relief.ratio, Elev.SQRT.mean.SQ,
              Elev.CURT.mean.CUBE, Profile.area, Elev.skewness, RP01, RP05, RP10, RP25, RP30, RP40, RP60, RP75,
              RP90, RP95, Int.mean, Elev.variance, Elev.P05, Elev.stddev, Elev.P01, 
              cigreen, cirededge, ciredrededge, nir, msr, msrrededge, msrredrededge, nvdirededge, nvdiredrededge, 
              red, green, blue, RP50, RP20, Int.L3,
              Elev.L3, rededge, RP70,
              RP80, Int.L.CV)) 
  
  ModelPrep <- prep(ModelRecipe) # computed metrics based on recipe
  ModelJuiced <- juice(ModelPrep) # creates revised training dataset
  
  # making model 
  final_wf <- workflow() %>%
    add_recipe(ModelRecipe) %>%
    add_model(final_rf) # IMPORTANT THAT THE FULL MODEL ABOVE WAS RUN SO THAT THE FINAL_RF HAS THE CORRECT HYPERPARAMETERS
  
  
  # assessing accuracies
  vFoldCV_Accuracies[i, "Split_Num"] <- i
  
  final_res <- final_wf %>%
    last_fit(xfold$splits[[i]])
  
  metrics <- final_res %>%
    collect_metrics() 
  
  vFoldCV_Accuracies[i, "Accuracy"] <- metrics[1, ".estimate"]
  
  predictions <- final_res %>% # predictions (includes percent of trees that predicted each!)
    collect_predictions()
  
  vFoldCV_Accuracies[i, "Pred_Other"] <- predictions[1, ".pred_Other"]
  vFoldCV_Accuracies[i, "Pred_THPL"] <- predictions[1, ".pred_THPL"]
  
}

mean(as.numeric(vFoldCV_Accuracies$Accuracy)) # mean accuracy 80%
sd(as.numeric(vFoldCV_Accuracies$Accuracy)) # sd 8%






