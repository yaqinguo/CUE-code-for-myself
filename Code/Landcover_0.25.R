# Define working directory and set
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
lc <- rast("LC_hd_global_2012_0.1.tif")
lc <- resample(lc, template_025, method = "bilinear")

# === Write Combined Raster to File ===
output_file <- file.path(workdir, "landcover_0.25deg.tif")
writeRaster(output_file, overwrite = TRUE)

cat("✅ Finished! Output saved to:", output_file, "\n")