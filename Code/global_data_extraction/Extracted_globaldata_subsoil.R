
#define working directory on scratch
workdir <- "/scratch/yg6266fu/Subsoil"
setwd(workdir)

library(terra)

# Create template raster at 0.25° resolution
template_01 <- rast(
  extent = ext(-180, 180, -90, 90),
  resolution = 0.1,
  crs = "EPSG:4326"
)
#read nc file
nc2 <- rast("PHH2O2.nc")
pH_sub <- nc2[[1:3]]
pH_sub_mean <- mean(pH_sub, na.rm=T)
pH_sub_mean <- pH_sub_mean * 0.1
pH_01 <- resample(pH_sub_mean, template_01, method = "bilinear")

nc4 <- rast("CEC2.nc")
CEC_sub <- nc4[[1:3]]
CEC_sub_mean <- mean(CEC_sub, na.rm=T)
CEC_sub_mean <- CEC_sub_mean * 0.01
CEC_01 <- resample(CEC_sub_mean, template_01, method = "bilinear")

nc5 <- rast("CLAY2.nc")
clay_sub <- nc5[[4:6]]
clay_sub_mean <- mean(clay_sub, na.rm=T)
clay_01 <- resample(clay_sub_mean, template_01, method = "bilinear")

nc8 <- rast("elevation_1KMmn_GMTEDmn.tif")
elevation_01 <- resample(nc8, template_01, method = "bilinear")

nc10 <-rast("LAI_0.1d.tif")
LAI_01 <- resample(nc10, template_01, method = "bilinear")

nc14 <- rast("MAT_bio_1.tif")
MAT_01 <- resample(nc14, template_01, method = "bilinear")

nc15 <- rast("TS_bio_4.tif")
TS_01 <- resample(nc15, template_01, method = "bilinear")

nc19 <- rast("Depth_to_Bedrock_0.1d.tif")
Bedrock_01 <- resample(nc19, template_01, method = "bilinear")

# === 3. Stack All Resampled Layers ===
predictors <- c(
  pH_01, clay_01, CEC_01, elevation_01, LAI_01,
  MAT_01, TS_01, Bedrock_01
)
# Assign readable names
names(predictors) <- c(
  "pH", "Clay", "CEC",
  "Elevation", "LAI",
  "MAT","TS", "Bedrock"
)
# === 4. Write Combined Raster to File ===
output_file <- file.path(workdir, "global_predictors_subsoil_0.1deg.tif")
writeRaster(predictors, output_file, overwrite = TRUE)

cat("✅ Finished! Output saved to:", output_file, "\n")

# ======================================================resolution0.25====================================================================================
#define working directory on scratch
workdir <- "/scratch/yg6266fu/Subsoil"
setwd(workdir)

library(terra)

# Create template raster at 0.25° resolution
template_025 <- rast(
  extent = ext(-180, 180, -90, 90),
  resolution = 0.25,
  crs = "EPSG:4326"
)
#read nc file
nc2 <- rast("PHH2O2.nc")
pH_sub <- nc2[[1:3]]
pH_sub_mean <- mean(pH_sub, na.rm=T)
pH_sub_mean <- pH_sub_mean * 0.1
pH_025 <- resample(pH_sub_mean, template_025, method = "bilinear")

nc4 <- rast("CEC2.nc")
CEC_sub <- nc4[[1:3]]
CEC_sub_mean <- mean(CEC_sub, na.rm=T)
CEC_sub_mean <- CEC_sub_mean * 0.01
CEC_025 <- resample(CEC_sub_mean, template_025, method = "bilinear")

nc5 <- rast("CLAY2.nc")
clay_sub <- nc5[[4:6]]
clay_sub_mean <- mean(clay_sub, na.rm=T)
clay_025 <- resample(clay_sub_mean, template_025, method = "bilinear")

nc8 <- rast("elevation_1KMmn_GMTEDmn.tif")
elevation_025 <- resample(nc8, template_025, method = "bilinear")

nc10 <-rast("LAI_0.1d.tif")
LAI_025 <- resample(nc10, template_025, method = "bilinear")

nc14 <- rast("MAT_bio_1.tif")
MAT_025 <- resample(nc14, template_025, method = "bilinear")

nc15 <- rast("TS_bio_4.tif")
TS_025 <- resample(nc15, template_025, method = "bilinear")

nc19 <- rast("Depth_to_Bedrock_0.1d.tif")
Bedrock_025 <- resample(nc19, template_025, method = "bilinear")

# === 3. Stack All Resampled Layers ===
predictors <- c(
  pH_025, clay_025, CEC_025, elevation_025, LAI_025,
  MAT_025, TS_025, Bedrock_025
)
# Assign readable names
names(predictors) <- c(
  "pH", "Clay", "CEC",
  "Elevation", "LAI",
  "MAT","TS", "Bedrock"
)
# === 4. Write Combined Raster to File ===
output_file <- file.path(workdir, "global_predictors_subsoil_0.25deg.tif")
writeRaster(predictors, output_file, overwrite = TRUE)

cat("✅ Finished! Output saved to:", output_file, "\n")




