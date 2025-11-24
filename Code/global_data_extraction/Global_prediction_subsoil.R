library(terra)
library(data.table)
library(mlr3)
library(mlr3learners)
library(xgboost)

#-----------------------------
# 1. Load predictor rasters
#-----------------------------
# Your multi-band predictor raster
predictor_stack <- rast("global_predictors_subsoil_0.25deg.tif")

# Note: GPP and rootdepth should not be present in this final raster

#-----------------------------
# 2. Load MODIS landcover and create mask
#-----------------------------
modis_lc <- rast("landcover_0.25deg.tif")

# MODIS IGBP classification codes to exclude
# Check the official MODIS class table for exact codes
excluded_classes <- c(0, 11, 12, 13, 14, 15)  
# Example: 0=Water, 11=Permanent Wetlands, 12=Croplands,
# 13=Urban/Built-up, 14=Snow/Ice, 15=Barren (optional)

# Create a binary mask for natural land
natural_mask <- !modis_lc %in% excluded_classes

# Apply mask to predictors (pixels outside natural_mask become NA)
predictor_stack_masked <- mask(predictor_stack, natural_mask, maskvalues=0, updatevalue=NA)

#-----------------------------
# 3. Convert masked raster to data.frame for prediction
#-----------------------------
predictor_df <- as.data.frame(predictor_stack_masked, xy = TRUE, na.rm = TRUE)

# Keep only predictor columns (drop coordinates for prediction)
pred_input <- predictor_df[, names(predictor_stack), drop = FALSE]

#-----------------------------
# 4. Load training data and hyperparameters
#-----------------------------
train_data <- fread("training_data_subsoil.csv")

# Same predictors as in raster
train_data <- train_data[, c(names(predictor_stack), "CUE"), with = FALSE]

train_data[,CUE := as.numeric(CUE)]

# Load saved 50 hyperparameter sets (list of named lists)
load("hyperparameter_list_subsoil.RData")  # loads object: hyperparameter_list

#-----------------------------
# 5. Create mlr3 task
#-----------------------------
task <- TaskRegr$new(id = "CUE_final", backend = train_data, target = "CUE")
xgb_learner <- lrn("regr.xgboost")

#-----------------------------
# 6. Predict with 50 models
#-----------------------------
predictions_matrix <- matrix(NA, nrow = nrow(pred_input), ncol = length(hyperparameter_list))

for(i in seq_along(hyperparameter_list)) {
  cat("Training model", i, "\n")
  
  # Set hyperparameters
  xgb_learner$param_set$values <- hyperparameter_list[[i]]
  
  # Train model on all data
  xgb_learner$train(task)
  
  # Predict on global masked data
  temp_task <- TaskRegr$new(id = paste0("global_pred_", i),
                            backend = data.table(pred_input, CUE = NA),
                            target = "CUE")
  pred <- xgb_learner$predict(temp_task)
  
  predictions_matrix[, i] <- pred$response
}

#-----------------------------
# 7. Compute mean & 95% CI
#-----------------------------
mean_pred <- rowMeans(predictions_matrix, na.rm = TRUE)
lower_ci  <- apply(predictions_matrix, 1, quantile, probs = 0.025, na.rm = TRUE)
upper_ci  <- apply(predictions_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)

#-----------------------------
# 8. Convert predictions back to raster
#-----------------------------
# Start from empty rasters based on the mask template
mean_raster  <- rast(predictor_stack[[1]])
lower_raster <- rast(predictor_stack[[1]])
upper_raster <- rast(predictor_stack[[1]])

# Fill rasters using coordinates
cells <- cellFromXY(mean_raster, predictor_df[, c("x", "y")])
mean_raster[cells]  <- mean_pred
lower_raster[cells] <- lower_ci
upper_raster[cells] <- upper_ci

#-----------------------------
# 9. Save rasters
#-----------------------------
writeRaster(mean_raster,  "CUE_mean_prediction_0.25deg_sub.tif",  overwrite = TRUE)
writeRaster(lower_raster, "CUE_lower95CI_0.25deg_sub.tif", overwrite = TRUE)
writeRaster(upper_raster, "CUE_upper95CI_0.25deg_sub.tif", overwrite = TRUE)

#-----------------------------