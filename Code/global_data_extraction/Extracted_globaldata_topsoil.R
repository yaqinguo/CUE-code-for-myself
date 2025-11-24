# Define working directory and set
workdir <- "/scratch/yg6266fu/Topsoil"
setwd(workdir)

library(terra)

# Create template raster at 0.25° resolution
template_01 <- rast(
  extent = ext(-180, 180, -90, 90),
  resolution = 0.1,
  crs = "EPSG:4326"
)
#_____________________________________________Soil____________________________________________________________________________

#read nc file
nc1 <- rast("VMC21.nc")
moisture_top <- nc1[[1:3]]
moisture_top_mean <- mean(moisture_top, na.rm=T)
moisture_01 <- resample(moisture_top_mean, template_01, method = "bilinear")

nc2 <- rast("PHH2O1.nc")
pH_top <- nc2[[1:3]]
pH_top_mean <- mean(pH_top, na.rm=T)
pH_top_mean <- pH_top_mean * 0.1
pH_01 <- resample(pH_top_mean, template_01, method = "bilinear")

nc3 <- rast("BD1.nc")
BD_top <- nc3[[1:3]]
BD_top_mean <- mean(BD_top, na.rm=T)
BD_top_mean <- BD_top_mean * 0.01
BD_01 <- resample(BD_top_mean, template_01, method = "bilinear")

nc4 <- rast("CEC1.nc")
CEC_top <- nc4[[1:3]]
CEC_top_mean <- mean(CEC_top, na.rm=T)
CEC_top_mean <- CEC_top_mean * 0.01
CEC_01 <- resample(CEC_top_mean, template_01, method = "bilinear")

nc5 <- rast("CLAY1.nc")
clay_top <- nc5[[1:3]]
clay_top_mean <- mean(clay_top, na.rm=T)
clay_01 <- resample(clay_top_mean, template_01, method = "bilinear")

nc6 <- rast("SAND1.nc")
sand_top <- nc6[[1:3]]
sand_top_mean <- mean(sand_top, na.rm=T)
sand_01 <- resample(sand_top_mean, template_01, method = "bilinear")

nc7 <- rast("SILT1.nc")
silt_top <- nc7[[1:3]]
silt_top_mean <- mean(silt_top, na.rm=T)
silt_01 <- resample(silt_top_mean, template_01, method = "bilinear")

#_____________________________________________topgraphy____________________________________________________________________________
nc8 <- rast("elevation_1KMmn_GMTEDmn.tif")
elevation_01 <- resample(nc8, template_01, method = "bilinear")
nc9 <- rast("slope_1KMmn_GMTEDmd.tif")
slope_01 <- resample(nc9, template_01, method = "bilinear")
#_____________________________________________vegetation__________________________________________________________________________
nc10 <-rast("LAI_0.1d.tif")
LAI_01 <- resample(nc10, template_01, method = "bilinear")

nc11 <- rast("shannon_EVI_0.1d.tif")
shannon_01 <- resample(nc11, template_01, method = "bilinear")

nc12 <- rast("aboveground_biomass_carbon_2010.tif")
AGB_01 <- resample(nc12, template_01, method = "bilinear")

nc13 <- rast("belowground_biomass_carbon_2010.tif")
BGB_01 <- resample(nc13, template_01, method = "bilinear")

#_____________________________________________climate__________________________________________________________________________
nc14 <- rast("MAT_bio_1.tif")
MAT_01 <- resample(nc14, template_01, method = "bilinear")

nc15 <- rast("TS_bio_4.tif")
TS_01 <- resample(nc15, template_01, method = "bilinear")

nc16 <- rast("MAP_bio_12.tif")
MAP_01 <- resample(nc16, template_01, method = "bilinear")

nc17 <- rast("PS_bio_15.tif")
PS_01 <- resample(nc17, template_01, method = "bilinear")

nc18 <- rast("ai_v3_yr.tif")
nc18 <- nc18 * 0.0001
AI_01 <- resample(nc18, template_01, method = "bilinear")

nc19 <- rast("Depth_to_Bedrock_0.1d.tif")
Bedrock_01 <- resample(nc19, template_01, method = "bilinear")

# === 3. Stack All Resampled Layers ===
predictors <- c(
  moisture_01, BD_01, pH_01, CEC_01, clay_01, sand_01, silt_01,
  elevation_01, slope_01, LAI_01, shannon_01, AGB_01, BGB_01,
  MAT_01, MAP_01, PS_01, TS_01, AI_01, Bedrock_01
)
# Assign readable names
names(predictors) <- c(
  "Moisture", "BD", "pH", "CEC", "Clay", "Sand", "Silt",
  "Elevation","Slope", "LAI","Shannon_EVI", "AGB", "BGB",
  "MAT","MAP","PS", "TS", "AI","Bedrock"
)
# === 4. Write Combined Raster to File ===
output_file <- file.path(workdir, "global_predictors_topsoil_0.1deg.tif")
writeRaster(predictors, output_file, overwrite = TRUE)

cat("✅ Finished! Output saved to:", output_file, "\n")

# =============================================0.25 resolution==================================================================================================================
# Define working directory and set
workdir <- "/scratch/yg6266fu/Topsoil"
setwd(workdir)

library(terra)

# Create template raster at 0.25° resolution
template_025 <- rast(
  extent = ext(-180, 180, -90, 90),
  resolution = 0.25,
  crs = "EPSG:4326"
)
#_____________________________________________Soil____________________________________________________________________________

#read nc file
nc1 <- rast("VMC21.nc")
moisture_top <- nc1[[1:3]]
moisture_top_mean <- mean(moisture_top, na.rm=T)
moisture_025 <- resample(moisture_top_mean, template_025, method = "bilinear")

nc2 <- rast("PHH2O1.nc")
pH_top <- nc2[[1:3]]
pH_top_mean <- mean(pH_top, na.rm=T)
pH_top_mean <- pH_top_mean * 0.1
pH_025 <- resample(pH_top_mean, template_025, method = "bilinear")

nc3 <- rast("BD1.nc")
BD_top <- nc3[[1:3]]
BD_top_mean <- mean(BD_top, na.rm=T)
BD_top_mean <- BD_top_mean * 0.01
BD_025 <- resample(BD_top_mean, template_025, method = "bilinear")

nc4 <- rast("CEC1.nc")
CEC_top <- nc4[[1:3]]
CEC_top_mean <- mean(CEC_top, na.rm=T)
CEC_top_mean <- CEC_top_mean * 0.01
CEC_025 <- resample(CEC_top_mean, template_025, method = "bilinear")

nc5 <- rast("CLAY1.nc")
clay_top <- nc5[[1:3]]
clay_top_mean <- mean(clay_top, na.rm=T)
clay_025 <- resample(clay_top_mean, template_25, method = "bilinear")

nc6 <- rast("SAND1.nc")
sand_top <- nc6[[1:3]]
sand_top_mean <- mean(sand_top, na.rm=T)
sand_025 <- resample(sand_top_mean, template_025, method = "bilinear")

nc7 <- rast("SILT1.nc")
silt_top <- nc7[[1:3]]
silt_top_mean <- mean(silt_top, na.rm=T)
silt_025 <- resample(silt_top_mean, template_025, method = "bilinear")

#_____________________________________________topgraphy____________________________________________________________________________
nc8 <- rast("elevation_1KMmn_GMTEDmn.tif")
elevation_025 <- resample(nc8, template_025, method = "bilinear")
nc9 <- rast("slope_1KMmn_GMTEDmd.tif")
slope_025 <- resample(nc9, template_025, method = "bilinear")
#_____________________________________________vegetation__________________________________________________________________________
nc10 <-rast("LAI_0.1d.tif")
LAI_025 <- resample(nc10, template_025, method = "bilinear")

nc11 <- rast("shannon_EVI_0.1d.tif")
shannon_025 <- resample(nc11, template_025, method = "bilinear")

nc12 <- rast("aboveground_biomass_carbon_2010.tif")
AGB_025 <- resample(nc12, template_025, method = "bilinear")

nc13 <- rast("belowground_biomass_carbon_2010.tif")
BGB_025 <- resample(nc13, template_025, method = "bilinear")

#_____________________________________________climate__________________________________________________________________________
nc14 <- rast("MAT_bio_1.tif")
MAT_025 <- resample(nc14, template_025, method = "bilinear")

nc15 <- rast("TS_bio_4.tif")
TS_025 <- resample(nc15, template_025, method = "bilinear")

nc16 <- rast("MAP_bio_12.tif")
MAP_025 <- resample(nc16, template_025, method = "bilinear")

nc17 <- rast("PS_bio_15.tif")
PS_025 <- resample(nc17, template_025, method = "bilinear")

nc18 <- rast("ai_v3_yr.tif")
nc18 <- nc18 * 0.0001
AI_025 <- resample(nc18, template_025, method = "bilinear")

nc19 <- rast("Depth_to_Bedrock_0.1d.tif")
Bedrock_025 <- resample(nc19, template_025, method = "bilinear")

# === 3. Stack All Resampled Layers ===
predictors <- c(
  moisture_025, BD_025, pH_025, CEC_025, clay_025, sand_025, silt_025,
  elevation_025, slope_025, LAI_025, shannon_025, AGB_025, BGB_025,
  MAT_025, MAP_025, PS_025, TS_025, AI_025, Bedrock_025
)
# Assign readable names
names(predictors) <- c(
  "Moisture", "BD", "pH", "CEC", "Clay", "Sand", "Silt",
  "Elevation","Slope", "LAI","Shannon_EVI", "AGB", "BGB",
  "MAT","MAP","PS", "TS", "AI","Bedrock"
)
# === 4. Write Combined Raster to File ===
output_file <- file.path(workdir, "global_predictors_topsoil_0.25deg.tif")
writeRaster(predictors, output_file, overwrite = TRUE)

cat("✅ Finished! Output saved to:", output_file, "\n")


