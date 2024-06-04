library(tidyverse)
library(randomForest)
library(caret)
library(pls)

# Setting the seed for repeatability
set.seed(1, sample.kind="Rounding")

# Downloading the fetal health data
fetal_data <- read.csv("https://drive.google.com/uc?export=download&id=1170_4ug-fvVUaw9_zxZmFZa9P5mqZRCb")

# Looking at the top of the data
head(fetal_data)

# Plotting the baseline heart rate data, and calculated its min, max and mean
fetal_data %>% ggplot(aes(baseline.value)) + geom_histogram(color="black", fill="blue", bins=15) + xlab("Baseline Heart Rate") + ylab("Count/Bin")

FHR_mean <- mean(fetal_data$baseline.value)
FHR_min <- min(fetal_data$baseline.value)
FHR_max <- max(fetal_data$baseline.value)

# Making a data table with statistics about the acceleration/deceleration data
data_table <- tibble(Variable = c("Accelerations", "Movement", "Uterine Contractions", 
                                  "Light Decelerations", "Severe Decelerations", 
                                  "Prolonged Decelerations"), 
                     Min = c(min(fetal_data$accelerations), 
                             min(fetal_data$fetal_movement), 
                             min(fetal_data$uterine_contractions), 
                             min(fetal_data$light_decelerations), 
                             min(fetal_data$severe_decelerations), 
                             min(fetal_data$prolongued_decelerations)), 
                     Max = c(max(fetal_data$accelerations), 
                             max(fetal_data$fetal_movement), 
                             max(fetal_data$uterine_contractions), 
                             max(fetal_data$light_decelerations), 
                             max(fetal_data$severe_decelerations), 
                             max(fetal_data$prolongued_decelerations)), 
                     Mean = c(mean(fetal_data$accelerations), 
                              mean(fetal_data$fetal_movement), 
                              mean(fetal_data$uterine_contractions), 
                              mean(fetal_data$light_decelerations), 
                              mean(fetal_data$severe_decelerations), 
                              mean(fetal_data$prolongued_decelerations)), 
                     StDev = c(sd(fetal_data$accelerations), 
                               sd(fetal_data$fetal_movement), 
                               sd(fetal_data$uterine_contractions), 
                               sd(fetal_data$light_decelerations), 
                               sd(fetal_data$severe_decelerations), 
                               sd(fetal_data$prolongued_decelerations))) %>% 
  knitr::kable()

data_table

# Plotting variability
fetal_data %>% ggplot(aes(abnormal_short_term_variability)) + 
  geom_histogram(color="black", fill="blue", bins=30) + 
  xlab("Abnormal Short Term Variability (% of Time)") + 
  ylab("Count/Bin")

fetal_data %>% ggplot(aes(mean_value_of_short_term_variability)) + 
  geom_histogram(color="black", fill="blue", bins=30) + 
  xlab("Mean Value of Short Term Variability (Beats/Min)") + 
  ylab("Count/Bin")

fetal_data %>% ggplot(aes(percentage_of_time_with_abnormal_long_term_variability)) + 
  geom_histogram(color="black", fill="blue", bins=30) + 
  xlab("Abnormal Long Term Variability (% of Time)") + 
  ylab("Count/Bin")

fetal_data %>% ggplot(aes(mean_value_of_long_term_variability)) + 
  geom_histogram(color="black", fill="blue", bins=30) + 
  xlab("Mean Value of Long Term Variability (Beats/Min)") + 
  ylab("Count/Bin")

# Data table of histogram statistics
data_table <- tibble(Variable = c("Width", "Min", "Max", "Number of Peaks", 
                                  "Number of Zeros", "Mode", "Mean", "Median", 
                                  "Variance", "Tendency"), 
                     Min = c(min(fetal_data$histogram_width), 
                             min(fetal_data$histogram_min), 
                             min(fetal_data$histogram_max), 
                             min(fetal_data$histogram_number_of_peaks), 
                             min(fetal_data$histogram_number_of_zeroes), 
                             min(fetal_data$histogram_mode), 
                             min(fetal_data$histogram_mean), 
                             min(fetal_data$histogram_median), 
                             min(fetal_data$histogram_variance), 
                             min(fetal_data$histogram_tendency)), 
                     Max = c(max(fetal_data$histogram_width), 
                             max(fetal_data$histogram_min), 
                             max(fetal_data$histogram_max), 
                             max(fetal_data$histogram_number_of_peaks), 
                             max(fetal_data$histogram_number_of_zeroes), 
                             max(fetal_data$histogram_mode), 
                             max(fetal_data$histogram_mean), 
                             max(fetal_data$histogram_median), 
                             max(fetal_data$histogram_variance), 
                             max(fetal_data$histogram_tendency)), 
                     Mean = c(mean(fetal_data$histogram_width), 
                              mean(fetal_data$histogram_min), 
                              mean(fetal_data$histogram_max), 
                              mean(fetal_data$histogram_number_of_peaks), 
                              mean(fetal_data$histogram_number_of_zeroes), 
                              mean(fetal_data$histogram_mode), 
                              mean(fetal_data$histogram_mean), 
                              mean(fetal_data$histogram_median), 
                              mean(fetal_data$histogram_variance), 
                              mean(fetal_data$histogram_tendency)), 
                     StDev = c(sd(fetal_data$histogram_width), 
                               sd(fetal_data$histogram_min), 
                               sd(fetal_data$histogram_max), 
                               sd(fetal_data$histogram_number_of_peaks), 
                               sd(fetal_data$histogram_number_of_zeroes), 
                               sd(fetal_data$histogram_mode), 
                               sd(fetal_data$histogram_mean), 
                               sd(fetal_data$histogram_median), 
                               sd(fetal_data$histogram_variance), 
                               sd(fetal_data$histogram_tendency))) %>% 
  knitr::kable()

data_table

# Plot histogram_width variable
fetal_data %>% ggplot(aes(histogram_width)) + 
  geom_histogram(color="black", fill="blue", bins=30) + 
  xlab("Histogram Width (Beats/Min)") + 
  ylab("Count/Bin")

# Plot fetal outcome data
fetal_data %>% ggplot(aes(fetal_health)) + 
  geom_histogram(color="black", fill="blue", bins=3) + 
  xlab("Health Outcome") + 
  ylab("Count/Bin")

# Change fetal outcome data to factors
fetal_data$fetal_health <- factor(fetal_data$fetal_health, levels = c(1,2,3))

# Perform test/train splits
train_index <- createDataPartition(fetal_data$fetal_health, times=1, p=0.1, list=FALSE)
train_data <- fetal_data[-train_index, ]
test_data <- fetal_data[train_index, ]

train2_index <- createDataPartition(train_data$fetal_health, times=1, p=0.1, list=FALSE)
train_train_data <- train_data[-train2_index, ]
train_test_data <- train_data[train2_index, ]

# KNN Model
knn_model <- train(fetal_health ~ ., data=train_train_data, method="knn", 
                   trControl = trainControl(method = "cv"), 
                   tuneGrid = data.frame(k=seq(1,20)))
plot(knn_model)
best_model_knn <- knn3(fetal_health ~ ., data=train_train_data, 
                       k = knn_model$bestTune)
predictions_knn <- predict(best_model_knn, train_test_data, type="class")
cm_knn <- confusionMatrix(predictions_knn, train_test_data$fetal_health)
cm_knn

# KNN Model of Individuial Types of Variables

# KNN - Baseline
knn_model_baseline <- train(fetal_health ~ baseline.value, data=train_train_data, method="knn", 
                            trControl = trainControl(method = "cv"), 
                            tuneGrid = data.frame(k=seq(1,20)))
best_model_baseline <- knn3(fetal_health ~ baseline.value, data=train_train_data, 
                            k = knn_model_baseline$bestTune)
predictions_baseline <- predict(best_model_baseline, train_test_data, type="class")
cm_baseline <- confusionMatrix(predictions_baseline, train_test_data$fetal_health)

# KNN - Acceleration/Deceleration
knn_model_celeration <- train(fetal_health ~ accelerations + fetal_movement + uterine_contractions + light_decelerations + severe_decelerations + prolongued_decelerations, data=train_train_data, method="knn", 
                              trControl = trainControl(method = "cv"), 
                              tuneGrid = data.frame(k=seq(1,20)))
best_model_celeration <- knn3(fetal_health ~ accelerations + fetal_movement + uterine_contractions + light_decelerations + severe_decelerations + prolongued_decelerations, data=train_train_data, 
                              k = knn_model_celeration$bestTune)
predictions_celeration <- predict(best_model_celeration, train_test_data, type="class")
cm_celeration <- confusionMatrix(predictions_celeration, train_test_data$fetal_health)

# KNN - Variability
knn_model_variability <- train(fetal_health ~ abnormal_short_term_variability + mean_value_of_short_term_variability + percentage_of_time_with_abnormal_long_term_variability + mean_value_of_long_term_variability, data=train_train_data, method="knn", 
                               trControl = trainControl(method = "cv"), 
                               tuneGrid = data.frame(k=seq(1,20)))
best_model_variability <- knn3(fetal_health ~ abnormal_short_term_variability + mean_value_of_short_term_variability + percentage_of_time_with_abnormal_long_term_variability + mean_value_of_long_term_variability, data=train_train_data, 
                               k = knn_model_variability$bestTune)
predictions_variability <- predict(best_model_variability, train_test_data, type="class")
cm_variability <- confusionMatrix(predictions_variability, train_test_data$fetal_health)

# KNN - Histogram
knn_model_histogram <- train(fetal_health ~ histogram_width + histogram_min + histogram_max + histogram_number_of_peaks + histogram_number_of_zeroes + histogram_mode + histogram_mean + histogram_median + histogram_variance + histogram_tendency, data=train_train_data, method="knn", 
                             trControl = trainControl(method = "cv"), 
                             tuneGrid = data.frame(k=seq(1,20)))
best_model_histogram <- knn3(fetal_health ~ histogram_width + histogram_min + histogram_max + histogram_number_of_peaks + histogram_number_of_zeroes + histogram_mode + histogram_mean + histogram_median + histogram_variance + histogram_tendency, data=train_train_data, 
                             k = knn_model_histogram$bestTune)
predictions_histogram <- predict(best_model_histogram, train_test_data, type="class")
cm_histogram <- confusionMatrix(predictions_histogram, train_test_data$fetal_health)

# Combine data from previous 4 models into a table
knn_results_table <- tibble(Variables_Used = c("Baseline Heart Rate", 
                                               "Accel/Decel", "Variability", 
                                               "Histogram"), 
                            Best_k = c(knn_model_baseline$bestTune, 
                                       knn_model_celeration$bestTune, 
                                       knn_model_variability$bestTune, 
                                       knn_model_histogram$bestTune), 
                            Accuracy = c(round(100*cm_baseline$overall["Accuracy"],2), 
                                         round(100*cm_celeration$overall["Accuracy"],2), 
                                         round(100*cm_variability$overall["Accuracy"],2), 
                                         round(100*cm_histogram$overall["Accuracy"],2))) %>% 
  knitr::kable()

knn_results_table

# KNN - Variability Plus Histogram
knn_model_v_h <- train(fetal_health ~ abnormal_short_term_variability + 
                         mean_value_of_short_term_variability + 
                         percentage_of_time_with_abnormal_long_term_variability + 
                         mean_value_of_long_term_variability + histogram_width + 
                         histogram_min + histogram_max + 
                         histogram_number_of_peaks + histogram_number_of_zeroes + 
                         histogram_mode + histogram_mean + histogram_median + 
                         histogram_variance + histogram_tendency, 
                       data=train_train_data, method="knn", 
                       trControl = trainControl(method = "cv"), 
                       tuneGrid = data.frame(k=seq(1,20)))
best_model_v_h <- knn3(fetal_health ~ abnormal_short_term_variability + 
                         mean_value_of_short_term_variability + 
                         percentage_of_time_with_abnormal_long_term_variability + 
                         mean_value_of_long_term_variability + histogram_width + 
                         histogram_min + histogram_max + 
                         histogram_number_of_peaks + histogram_number_of_zeroes + 
                         histogram_mode + histogram_mean + histogram_median + 
                         histogram_variance + histogram_tendency, 
                       data=train_train_data, 
                       k = knn_model_v_h$bestTune)
predictions_v_h <- predict(best_model_v_h, train_test_data, type="class")
cm_v_h <- confusionMatrix(predictions_v_h, train_test_data$fetal_health)

cm_v_h

# Results Table
model_results_table <- tibble(Model = c("K Nearest Neighbors"), 
                              Accuracy = c(round(100*cm_knn$overall["Accuracy"],2))) %>% 
  knitr::kable()
model_results_table

# Regression Tree Model
rpart <- train(fetal_health ~ ., method = "rpart", 
               trControl = trainControl(method = "cv"), 
               tuneGrid = data.frame(cp = seq(0.0, 0.03, len = 15)), 
               data = train_train_data)
plot(rpart)
rpart_predict <- predict(rpart, train_test_data)
cm_rpart <- confusionMatrix(rpart_predict, train_test_data$fetal_health)
cm_rpart

# Results Table
model_results_table <- tibble(Model = c("K Nearest Neighbors", "Regression Tree"), 
                              Accuracy = c(round(100*cm_knn$overall["Accuracy"],2), 
                                           round(100*cm_rpart$overall["Accuracy"],2))) %>% 
  knitr::kable()
model_results_table

# Show decision tree
plot(rpart$finalModel, margin=0.1)
text(rpart$finalModel, cex=0.25)

# Regression Tree Model with 8 best parameters
rpart8 <- train(fetal_health ~ mean_value_of_short_term_variability + 
                  percentage_of_time_with_abnormal_long_term_variability + 
                  abnormal_short_term_variability + histogram_mean + 
                  histogram_mode + baseline.value + uterine_contractions + 
                  accelerations, method = "rpart", 
                trControl = trainControl(method = "cv"), 
                tuneGrid = data.frame(cp = seq(0.0, 0.03, len = 15)), 
                data = train_train_data)
rpart_predict8 <- predict(rpart8, train_test_data)
cm_rpart8 <- confusionMatrix(rpart_predict8, train_test_data$fetal_health)
cm_rpart8

# Results Table
model_results_table <- tibble(Model = c("K Nearest Neighbors", 
                                        "Regression Tree", 
                                        "Regression Tree - best 8 parameters"), 
                              Accuracy = c(round(100*cm_knn$overall["Accuracy"],2), 
                                           round(100*cm_rpart$overall["Accuracy"],2), 
                                           round(100*cm_rpart8$overall["Accuracy"],2))) %>% 
  knitr::kable()
model_results_table

# KNN Model Redux - Best 8 parameters
knn_model_best_params <- train(fetal_health ~ mean_value_of_short_term_variability + 
                                 percentage_of_time_with_abnormal_long_term_variability + 
                                 abnormal_short_term_variability + histogram_mean + 
                                 histogram_mode + baseline.value + 
                                 uterine_contractions + accelerations, 
                               data=train_train_data, method="knn", 
                               trControl = trainControl(method = "cv"), 
                               tuneGrid = data.frame(k=seq(1,20)))
plot(knn_model_best_params)
best_model_best_params <- knn3(fetal_health ~ mean_value_of_short_term_variability +
                                 percentage_of_time_with_abnormal_long_term_variability + 
                                 abnormal_short_term_variability + histogram_mean + 
                                 histogram_mode + baseline.value + 
                                 uterine_contractions + accelerations, 
                               data=train_train_data, 
                               k = knn_model_best_params$bestTune)
predictions_best_params <- predict(best_model_best_params, 
                                   train_test_data, 
                                   type="class")
cm_best_params <- confusionMatrix(predictions_best_params, 
                                  train_test_data$fetal_health)
cm_best_params

# Results Table
model_results_table <- tibble(Model = c("K Nearest Neighbors", 
                                        "Regression Tree", 
                                        "Regression Tree - best 8 parameters", 
                                        "K Nearest Neighbors - best 8 parameters"), 
                              Accuracy = c(round(100*cm_knn$overall["Accuracy"],2), 
                                           round(100*cm_rpart$overall["Accuracy"],2), 
                                           round(100*cm_rpart8$overall["Accuracy"],2), 
                                           round(100*cm_best_params$overall["Accuracy"],2))) %>% 
  knitr::kable()
model_results_table

# Random Forest Model
rf_model <- train(fetal_health ~ ., method="Rborist", 
                  trControl = trainControl(method = "cv"), 
                  tuneGrid = data.frame(predFixed = 2, 
                                        minNode = c(1,3,5)), 
                  data = train_train_data)
plot(rf_model)
rf_predict <- predict(rf_model, train_test_data)
cm_rf <- confusionMatrix(rf_predict, train_test_data$fetal_health)
cm_rf

# Results Table
model_results_table <- tibble(Model = c("K Nearest Neighbors", 
                                        "Regression Tree", 
                                        "Regression Tree - best 8 parameters", 
                                        "K Nearest Neighbors - best 8 parameters", 
                                        "Random Forest"), 
                              Accuracy = c(round(100*cm_knn$overall["Accuracy"],2), 
                                           round(100*cm_rpart$overall["Accuracy"],2), 
                                           round(100*cm_rpart8$overall["Accuracy"],2), 
                                           round(100*cm_best_params$overall["Accuracy"],2), 
                                           round(100*cm_rf$overall["Accuracy"],2))) %>% 
  knitr::kable()
model_results_table

# Random Forest - 8 best parameters
rf8_model <- train(fetal_health ~ mean_value_of_short_term_variability + 
                     percentage_of_time_with_abnormal_long_term_variability + 
                     abnormal_short_term_variability + histogram_mean + 
                     histogram_mode + baseline.value + uterine_contractions + 
                     accelerations, method="Rborist", 
                   trControl = trainControl(method = "cv"), 
                   tuneGrid = data.frame(predFixed = 2, minNode = c(1,3,5)), 
                   data = train_train_data)
plot(rf8_model)
rf8_predict <- predict(rf8_model, train_test_data)
cm_rf8 <- confusionMatrix(rf8_predict, train_test_data$fetal_health)
cm_rf8

# Results Table
model_results_table <- tibble(Model = c("K Nearest Neighbors", 
                                        "Regression Tree", 
                                        "Regression Tree - best 8 parameters", 
                                        "K Nearest Neighbors - best 8 parameters", 
                                        "Random Forest", 
                                        "Random Forest - best 8 parameters"), 
                              Accuracy = c(round(100*cm_knn$overall["Accuracy"],2), 
                                           round(100*cm_rpart$overall["Accuracy"],2), 
                                           round(100*cm_rpart8$overall["Accuracy"],2), 
                                           round(100*cm_best_params$overall["Accuracy"],2),
                                           round(100*cm_rf$overall["Accuracy"],2), 
                                           round(100*cm_rf8$overall["Accuracy"],2))) %>% 
  knitr::kable()
model_results_table

# Final model test
rf_final_predict <- predict(rf8_model, test_data)
cm_rf_final <- confusionMatrix(rf_final_predict, test_data$fetal_health)
cm_rf_final

# Final Results Table
model_results_table <- tibble(Model = c("K Nearest Neighbors", 
                                        "Regression Tree", 
                                        "Regression Tree - best 8 parameters", 
                                        "K Nearest Neighbors - best 8 parameters", 
                                        "Random Forest", "Random Forest - best 8 parameters",
                                        "", 
                                        "Final Model on Holdout Test Data"), 
                              Accuracy = c(round(100*cm_knn$overall["Accuracy"],2), 
                                           round(100*cm_rpart$overall["Accuracy"],2), 
                                           round(100*cm_rpart8$overall["Accuracy"],2), 
                                           round(100*cm_best_params$overall["Accuracy"],2), 
                                           round(100*cm_rf$overall["Accuracy"],2), 
                                           round(100*cm_rf8$overall["Accuracy"],2), 
                                           "", 
                                           round(100*cm_rf_final$overall["Accuracy"],2))) %>% 
  knitr::kable()
model_results_table