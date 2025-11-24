# install.packages(c("caret", "e1071", "randomForest", "xgboost"))
library(caret)
library(e1071)        # For SVM
library(randomForest) # For Random Forest
library(xgboost)      # For XGBoost
library(mlr3verse)
library(tidyverse)
library(future)
## here is only consider topsoil and subsoil

data <- read.csv("Env_factors_cart.csv", header = T)

na_columns <- names(data)[colSums(is.na(data)) > 0]
print(na_columns)

data$depth_class <- as.factor(data$depth_class)
data$Ecosystem_type <- as.factor(data$Ecosystem_type)

num_cols <- setdiff(names(data),c("depth_class","Ecosystem_type"))
data[num_cols] <- lapply(data[num_cols],as.numeric)

data <- data %>%
  rename(Landuse = Ecosystem_type, pH = Mean_pH, Sand = Sand.content, Clay = Clay.content, PS = PS_Bio15, TS = TS_Bio4, 
         Silt = Silt.content, BD = Bulk.Density, AGB = AbovegroundBiomass, BGB = BelowgroundBiomass)

data <- data %>%
  select("depth_class", "MAT", "MAP","PS","TS","AI","Elevation","Slope","pH","Moisture","BD","CEC","Sand",
         "Clay","Silt","RootDepth","Bedrock","AGB","BGB","LAI","Shannon_EVI","GPP", "CUE")


##========================================================================topsoil=============================================

Top <- data %>%
  filter(depth_class=="0-30") %>%
  select(-depth_class)

y_top <- Top$CUE

x <- Top[, c("MAT", "MAP","PS","TS","AI","Elevation","Slope","pH","Moisture","BD","CEC","Sand",
             "Clay","Silt","RootDepth","Bedrock","AGB","BGB","LAI","Shannon_EVI","GPP")]

#RF_recursive feature elimination________________________________________________________________________________________________________________________________________
set.seed(629)
ctrl_rf <- rfeControl(functions = rfFuncs, method = "cv",number = 5, verbose = T)
rfe_rf_top <- rfe(x=x, y=y_top, sizes=1:21, rfeControl = ctrl_rf)
selected_rf_top <- predictors(rfe_rf_top)
p1 <- ggplot(data=rfe_rf_top, metric="RMSE") + theme_bw() + 
  geom_vline(xintercept = 21, color="blue", linetype="dashed")+
  labs(x="",y="")+
  scale_x_continuous(limits = c(0,22), expand = c(0,0))+
  theme(axis.text = element_text(size=8, color="black", family="Arial"))
p1

rf  <- rfe_rf_top$results[,c("Variables","RMSE")]

# rf$Variables=factor(rf$Variables)

p1.1 <- ggplot(rf, aes(x=Variables, y=RMSE, group = 1))+
  geom_line()+
  geom_point()+
  geom_point(data = subset(rf, Variables==21),
             aes(x=Variables, y=RMSE),
             color="blue",
             size=4)+
  geom_vline(xintercept = 21, color="blue", linetype="dashed")+
  labs(x="",y="")+
  theme_bw()+
  annotate(geom="text",x=5, y= 0.10, label="RF", family="Arial", fontface="plain", size=3)
p1.1
####train RF________________________________________________________________

task = as_task_regr(Top, target="CUE")

simulate <- data.frame(ID = 1:nrow(Top))
actual <- data.frame(ID = 1:nrow(Top))

for (m in 1:50) {
  print(paste('set seed=',m))
  set.seed(m)
  row_id <- seq_len(nrow(Top))
  index_train <- sample(row_id, length(row_id)*0.8)
  #define learner and search space
  rf=lrn("regr.ranger", importance="impurity")
  search_space=ps(
    mtry=p_int(2,10),
    num.trees=p_int(100,500)
  )
  at = auto_tuner(
    tuner = tnr("grid_search", batch_size = 20),
    learner = rf,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    search_space = search_space,
    term_evals = 10
    )
  future::plan("multicore")
  at$train(task, row_ids = index_train)
  #use best parameters
  rf$param_set$values = at$tuning_results$learner_param_vals[[1]]
  rf$train(task, row_ids = index_train)
  #predict on full dataset
  pred <- rf$predict(task, row_ids = row_id)
  
  simulate[[paste0("run_",m)]] <- pred$response
  actual[[paste0("run_",m)]] <- Top$CUE
}

##analyze prediction results
a <- rowMeans(simulate[,-1])
b <- rowMeans(actual[,-1])

#regression fit
m <- lm(a ~ b)
plot(a ~ b, xlab="Observed", ylab = "predicted")
abline(m, col="red")
summary(m)

#RMSE
rmse <- sqrt(mean((a - b)^2))
print(paste("RMSE:",rmse))

#save output

result_df <- data.frame(Predicted = a, Observed = b)
write.csv(result_df,"CUE_RF_validation_Top.csv", row.names = F)

#SVM_recursive feature elimination______________________________________________________________________________________________________________________
set.seed(629)
ctrl_svm <- rfeControl(functions = caretFuncs, method = "cv",number = 5)

caretFuncs$fit <- function(x,y,...){
  train(x,y,method = "svmRadial", trControl = trainControl(method = "cv"))
}

rfe_svm_top <- rfe(x=x, y=y_top, sizes = 1:21, rfeControl = ctrl_svm)

selected_svm_top <- predictors(rfe_svm_top)

p2 <- ggplot(data=rfe_svm_top, metric="RMSE") + theme_bw() + 
  geom_vline(xintercept = 14, color="blue", linetype="dashed") +
  labs(x="",y="")+
  scale_x_continuous(limits = c(0,22), expand = c(0,0))+
  theme(axis.text = element_text(size=8, color="black", family="Arial"))
p2


svm_data  <- rfe_svm_top$results[,c("Variables","RMSE")]

p2.1 <- ggplot(svm_data, aes(x=Variables, y=RMSE, group = 1))+
  geom_line()+
  geom_point()+
  geom_point(data = subset(svm_data, Variables==14),
             aes(x=Variables, y=RMSE),
             color="blue",
             size=4)+
  geom_vline(xintercept = 14, color="blue", linetype="dashed")+
  labs(x="",y="")+
  theme_bw()+
  annotate(geom="text",x=5, y= 0.11, label="SVM", family="Arial", fontface="plain", size=3)
p2.1


##train SVM____________________________________________________________________________________________________________________
#create regression task
task <- as_task_regr(Top, target = "CUE")
#prepare data frames for storing predictions
simulate <- data.frame(ID = 1:nrow(Top))
actual <- data.frame(ID = 1:nrow(Top))

#loop over 50 seeds

for (m in 1:50){
  print(paste('set seed =',m))
  set.seed(m)
  row_id <- seq_len(nrow(Top))
  index_train <- sample(row_id, size = floor(0.8*length(row_id)))
  #define SVM learner
  svm <- lrn("regr.svm", type = "eps-regression", kernel = "radial")
  #define hyperparameter search space
  search_space <- ps(
    gamma = p_dbl(0.01,0.1),
    cost = p_dbl(0.1,10)
  )
  #define auto-tuner
  at <- auto_tuner(
    tuner = tnr("grid_search", batch_size = 40),
    learner = svm,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    search_space = search_space,
    term_evals = 10
  )
  #parallel execution
  future::plan("multicore")
  #train auto-tuner and final model
  at$train(task,row_ids = index_train)
  svm$param_set$values <- at$tuning_result$learner_param_vals[[1]]
  svm$train(task, row_ids = index_train)
  #predict for full dataset
  pred <- svm$predict(task, row_ids = row_id)
  
  simulate[[paste0("run_",m)]] <- pred$response
  actual[[paste0("run_",m)]] <- Top$CUE
}

#calculate average predictions
a <- rowMeans(simulate[,-1]) #drop ID
b <- rowMeans(actual[,-1])

#plot and evaluate 
m <- lm(a ~ b)
plot(a ~ b, xlab="Observed", ylab = "predicted")
abline(m, col="red")
summary(m)
sqrt(mean((b-a)^2)) #RMSE

# Save RMSE results

rmse <- data.frame(Predicted = a, Observed = b)
write.csv(rmse,"CUE_SVM_validation_Top.csv", row.names = F)

#XGBoost_recursive feature elimination__________________________________________________________________________________
set.seed(629)

caretFuncs$fit <- function(x,y,...){
  train(x,y,method = "xgbTree", trControl = trainControl(method = "cv"))
}

ctrl_xgb <- rfeControl(functions = caretFuncs, method="cv", number = 5)

rfe_xgb_top <- rfe(x=x, y=y_top, sizes = 1:21, rfeControl = ctrl_xgb)

selected_xgb_top <- predictors(rfe_xgb_top)

p3 <- ggplot(data=rfe_xgb_top, metric="RMSE") + theme_bw() + 
  geom_vline(xintercept = 19, color="blue", linetype="dashed") +
  labs(x="",y="")+
  scale_x_continuous(limits = c(0,22), expand = c(0,0))+
  theme(axis.text = element_text(size=8, color="black", family="Arial"))
p3

xgb_data  <- rfe_xgb_top$results[,c("Variables","RMSE")]

p3.1 <- ggplot(xgb_data, aes(x=Variables, y=RMSE, group = 1))+
  geom_line()+
  geom_point()+
  geom_point(data = subset(xgb_data, Variables==8),
             aes(x=Variables, y=RMSE),
             color="blue",
             size=4)+
  geom_vline(xintercept = 8, color="blue", linetype="dashed")+
  labs(x="",y="")+
  theme_bw()+
  annotate(geom="text",x=5, y= 0.11, label="XGBoost", family="Arial", fontface="plain", size=3)
p3.1

# library(ggpubr)
# p1 <- p1 + labs(x = NULL, y = NULL)
# p2 <- p2 + labs(x = NULL, y = NULL)
# p3 <- p3 + labs(x = NULL, y = NULL)
# 
# # Arrange with custom labels
# p_top <- ggarrange(p1, p2, p3,
#                    ncol = 3)
# 
# # Add shared axis labels
# p_top <- annotate_figure(
#   p_top,
#   left = text_grob("RMSE", rot = 90, size = 12),
#   bottom = text_grob("Number of predictor variables", size = 12)
# )

p_top <- ggpubr::ggarrange(p1.1, p2.1, p3.1,ncol = 3)

library(cowplot)

combined <- plot_grid(p1.1, p2.1,p3.1, ncol = 3,align = "v", axis = "lr",
                      labels = c("", ""),
                      label_size = 14,
                      label_fontface = "plain",
                      label_fontfamily = "Arial")


label_plot <- ggdraw() +
  annotate("text",
           x=0, y=0.5, label = "RMSE",
           angle = 90, vjust = 4.5,
           size = 4, family="Arial", fontface="plain")+
  annotate("text",
           x=10, y=0.02,label = "Number of variables",
           angle = 0, vjust = 0,
           size = 4, family="Arial", fontface="plain" )

final_pot <- ggdraw() +
  draw_plot(combined, x=0.05, width=0.9) +
  draw_plot(label_plot, x=0, width = 0.05 )

final_pot

ggsave("figures/Fig.S3.1.tiff", width = 6.5, height = 3, dpi = 300)

#train XGBoost_________________________________________________________________________________

Top_XGBoost <- Top %>%
  select(-RootDepth,-GPP)

# write.csv(Top_XGBoost, file = "training_data_topsoil.csv")

# initialize storage for predicted and actual values
simulate <- data.frame(Top_XGBoost[,"CUE"]) # predictions
actual <- data.frame(Top_XGBoost[,"CUE"]) #true CUE vales

#repeat model fitting 50 times with different random seeds

for (m  in 1:50) {
  print(paste('set seed=',m))
  set.seed(m)
  #relaod data each iteration
  data <- Top_XGBoost
  len <- nrow(data)
  row_id <- seq_len(len)
  #train-test split (80/20)
  index_train <- sample(nrow(data), nrow(data)*0.8)
  index_test <- setdiff(row_id, index_train)
  #define regression task with CUE as target
  task <- as_task_regr(data, target = "CUE")
  #define learner
  xgb <- lrn("regr.xgboost")
  #define hyperparameter search space
  search_space <- ps(
    eta = p_dbl(0.1,0.5),
    min_child_weight = p_dbl(1,20),
    subsample = p_dbl(0.5,1),
    colsample_bytree = p_dbl(0.5,1),
    colsample_bylevel = p_dbl(0.5,1),
    nrounds = p_int(1,50)
  )
  #auto-tuner with grid search
  at <- auto_tuner(tuner = tnr("grid_search", batch_size = 40),
                   learner = xgb,
                   resampling = rsmp("holdout"),
                   measure = msr("regr.rmse"),
                   search_space = search_space,
                   term_evals = 10)
  #enable parallel processing
  future::plan("multicore")
  #train the tuner
  at$train(task,row_ids = index_train)
  #set best parameters and retrain
  xgb$param_set$values <- at$tuning_result$learner_param_vals[[1]]
  xgb$train(task,row_ids = index_train)
  #predict on full data
  pred <- xgb$predict(task, row_ids = row_id)
  #store predictions and actuals
  simulate[,m] <- pred$response
  actual[,m] <- data$CUE
}

#evaluate model performace
#Mean predicted and actual values across all values
a <- rowMeans(simulate)
b <- rowMeans(actual)
#linear model and plot
m <- lm(a ~ b)
plot(a ~ b,xlab="Actual CUE", ylab = "Predicted CUE")
abline(m, col="red")
summary(m)
#compute RMSE
rmse_value <- sqrt(mean((b-a)^2))
cat("RSME =", rmse_value,"\n")
#save results
rmse_out <- cbind(predicted = a, actual = b)
write.csv(rmse_out,"CUE_XGBoost_validation_Top.csv", row.names = F)

#projection______________________________________________________________________

library(terra)
predictor_stack <- rast("global_predictors_topsoil_0.25deg.tif")
modis_lc <- rast("landcover_0.25deg.tif")
excluded_classes <- c(0, 11, 12, 13, 14, 15)
natural_mask <- !modis_lc %in% excluded_classes
predictor_stack_masked <- mask(predictor_stack, natural_mask, maskvalues=0, updatevalue=NA)
predictor_df <- as.data.frame(predictor_stack_masked, xy=T, na.rm=T)
pred_input <- predictor_df[,names(predictor_stack), drop=F]
all_preds <- list()
for (m  in 1:50) {
  print(paste('set seed=',m))
  set.seed(m)
  #relaod data each iteration
  data <- Top_XGBoost
  len <- nrow(data)
  row_id <- seq_len(len)
  #train-test split (80/20)
  index_train <- sample(nrow(data), nrow(data)*0.8)
  index_test <- setdiff(row_id, index_train)
  #define regression task with CUE as target
  task <- as_task_regr(data, target = "CUE")
  #define learner
  xgb <- lrn("regr.xgboost")
  #define hyperparameter search space
  search_space <- ps(
    eta = p_dbl(0.1,0.5),
    min_child_weight = p_dbl(1,20),
    subsample = p_dbl(0.5,1),
    colsample_bytree = p_dbl(0.5,1),
    colsample_bylevel = p_dbl(0.5,1),
    nrounds = p_int(1,50)
  )
  #auto-tuner with grid search
  at <- auto_tuner(tuner = tnr("grid_search", batch_size = 40),
                   learner = xgb,
                   resampling = rsmp("holdout"),
                   measure = msr("regr.rmse"),
                   search_space = search_space,
                   term_evals = 10)
  #enable parallel processing
  future::plan("multicore")
  #train the tuner
  at$train(task,row_ids = index_train)
  
  #set best parameters and retrain
  xgb$param_set$values <- at$tuning_result$learner_param_vals[[1]]
  xgb$train(task,row_ids = index_train)
  #predict on full data
  pred_global <- xgb$predict_newdata(pred_input)
  preds_this_model <- predictor_df %>%
    select(x,y) %>%
    mutate(pred=pred_global$response,
           model=m)
  #store predictions in the matrix
  all_preds[[m]] <- preds_this_model
}

all_pred_df <- bind_rows(all_preds)

pred_summary <- all_pred_df %>%
  group_by(x,y) %>%
  summarise(
    mean_pred=mean(pred),
    lower_95 = quantile(pred, 0.025),
    upper_95 = quantile(pred, 0.975),
    uncertainty = (upper_95 - lower_95),
    .groups = "drop")

min(pred_summary$mean_pred)
max(pred_summary$mean_pred)
min(pred_summary$uncertainty)
max(pred_summary$uncertainty)

write.csv(pred_summary, file = "CUE_Top_mean_prediction.csv", row.names = F)

template_025 <- rast(
  extent = ext(min(pred_summary$x), max(pred_summary$x), min(pred_summary$y),max(pred_summary$y)),
  resolution = 0.25,
  crs = "EPSG:4326"
)

mean_rast <- terra::rasterize(
  vect(pred_summary[,c("x","y","mean_pred")], geom=c("x","y")),
  template_025,
  field = "mean_pred",
  fun = "mean"
)
# plot(mean_rast)
# summary(mean_rast)
uncertainty_rast <- terra::rasterize(
  vect(pred_summary[,c("x","y","uncertainty")], geom=c("x","y")),
  template_025,
  field = "uncertainty",
  fun = "mean"
)
# plot(uncertainty_rast)

writeRaster(mean_rast, "CUE_Top_mean_prediction.tif", overwrite=T)
writeRaster(uncertainty_rast, "CUE_Top_uncertainty.tif", overwrite=T)


