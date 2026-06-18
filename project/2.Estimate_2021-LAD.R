rm(list = ls())

library(lidR)
library(terra)
library(sf)

# List laz files
laz_fn <- list.files('data/NEON_lidar-point-cloud-line/NEON.D07.MLBS.DP1.30003.001.2021-06.basic.20260618T140709Z.RELEASE-2026/')

# bring in RRC lidar
laz <- readLAS(paste0('data/NEON_lidar-point-cloud-line/NEON.D07.MLBS.DP1.30003.001.2021-06.basic.20260618T140709Z.RELEASE-2026/', laz_fn))

# move to DEM; use the entire scene to get the dem
dem <- rasterize_terrain(laz, res = 1, algorithm = tin())
plot(dem)

# make CHM
chm <- rasterize_canopy(laz, res = 1, algorithm = p2r())
plot(chm)

# bring in the shapefile
rrc <- st_read("data/GIS_Files/Neon_Plots_Shapefile/NEON_TOS_Plot_polygons_of_Interest.shp")
rrc <- st_transform(rrc, crs = crs(laz))

# clip laz
las.rrc <- clip_roi(laz, rrc)

# move to DEM
las.norm1 <- normalize_height(las.rrc[[1]], dem)
las.norm2 <- normalize_height(las.rrc[[2]], dem)
las.norm3 <- normalize_height(las.rrc[[3]], dem)
las.norm4 <- normalize_height(las.rrc[[4]], dem)
las.norm5 <- normalize_height(las.rrc[[5]], dem)
las.norm6 <- normalize_height(las.rrc[[6]], dem)

# removing negatives and crazy high values (meters) — CRUCIAL
las.norm1 <- filter_poi(las.norm1, Z >= 0, Z <= 60)
las.norm2 <- filter_poi(las.norm2, Z >= 0, Z <= 60)
las.norm3 <- filter_poi(las.norm3, Z >= 0, Z <= 60)
las.norm4 <- filter_poi(las.norm4, Z >= 0, Z <= 60)
las.norm5 <- filter_poi(las.norm5, Z >= 0, Z <= 60)
las.norm6 <- filter_poi(las.norm6, Z >= 0, Z <= 60)

# LAD with VOXEL
# Proper LAD using the Beer-Lambert / MacArthur-Wilson extinction approach.
# dz  = vertical bin size (must match the z-resolution passed to voxel_metrics)
# k   = light-extinction coefficient (0.5 is a common default for forest)

my_lad_func <- function(z, dz = 1, k = 0.5) {
  n_total <- length(z)
  if (n_total == 0) return(list(lad = NA_real_))
  
  # Gap fraction per voxel layer: proportion of pulses that pass THROUGH
  # (i.e. were not intercepted).  Using point count as a proxy for intercepted
  # pulses and assuming uniform sampling per layer.
  gap_fraction <- exp(-k * n_total * dz)   # Beer-Lambert approximation
  gap_fraction <- max(min(gap_fraction, 1 - 1e-6), 1e-6)  # clamp to (0,1)
  
  lad_value <- -log(gap_fraction) / (k * dz)
  return(list(lad = lad_value))
}

# 3-D voxel metrics: 5 m horizontal, 1 m vertical
# voxel_metrics res = c(xy, z) — only TWO values, not three
voxels1 <- voxel_metrics(las.norm1, ~my_lad_func(Z, dz = 1, k = 0.5),
                        res = 1,      # horizontal resolution (x and y), in meters
                        zres = 1)     # vertical resolution (z), in meters
voxels2 <- voxel_metrics(las.norm2, ~my_lad_func(Z, dz = 1, k = 0.5),
                         res = 1,
                         zres = 1)
voxels3 <- voxel_metrics(las.norm3, ~my_lad_func(Z, dz = 1, k = 0.5),
                         res = 1,
                         zres = 1)
voxels4 <- voxel_metrics(las.norm4, ~my_lad_func(Z, dz = 1, k = 0.5),
                         res = 1,
                         zres = 1)
voxels5 <- voxel_metrics(las.norm5, ~my_lad_func(Z, dz = 1, k = 0.5),
                         res = 1,
                         zres = 1)
voxels6 <- voxel_metrics(las.norm6, ~my_lad_func(Z, dz = 1, k = 0.5),
                         res = 1,
                         zres = 1)


# Make arrays for dataframes
lad_arr1 <- reshape2::acast(voxels1, X ~ Y ~ Z, value.var = 'lad')
lad_arr2 <- reshape2::acast(voxels2, X ~ Y ~ Z, value.var = 'lad')
lad_arr3 <- reshape2::acast(voxels3, X ~ Y ~ Z, value.var = 'lad')
lad_arr4 <- reshape2::acast(voxels4, X ~ Y ~ Z, value.var = 'lad')
lad_arr5 <- reshape2::acast(voxels5, X ~ Y ~ Z, value.var = 'lad')
lad_arr6 <- reshape2::acast(voxels6, X ~ Y ~ Z, value.var = 'lad')

# Convert 0 to NA
#lad_arr1[which(is.na(lad_arr1))] <- 0
#lad_arr2[which(is.na(lad_arr2))] <- 0
#lad_arr3[which(is.na(lad_arr3))] <- 0
#lad_arr4[which(is.na(lad_arr4))] <- 0
#lad_arr5[which(is.na(lad_arr5))] <- 0
#lad_arr6[which(is.na(lad_arr6))] <- 0

# Pivot back to dataframes
lad_df1 <- reshape2::melt(lad_arr1)
colnames(lad_df1) <- c('x', 'y', 'z', 'lad')
lad_df2 <- reshape2::melt(lad_arr2)
colnames(lad_df2) <- c('x', 'y', 'z', 'lad')
lad_df3 <- reshape2::melt(lad_arr3)
colnames(lad_df3) <- c('x', 'y', 'z', 'lad')
lad_df4 <- reshape2::melt(lad_arr4)
colnames(lad_df4) <- c('x', 'y', 'z', 'lad')
lad_df5 <- reshape2::melt(lad_arr5)
colnames(lad_df5) <- c('x', 'y', 'z', 'lad')
lad_df6 <- reshape2::melt(lad_arr6)
colnames(lad_df6) <- c('x', 'y', 'z', 'lad')

# Add plot number
lad_df1$plot <- '019'
lad_df2$plot <- '020'
lad_df3$plot <- '025'
lad_df4$plot <- '013'
lad_df5$plot <- '006'
lad_df6$plot <- '007'

# Combine
lad <- rbind(lad_df1, lad_df2, lad_df3,
             lad_df4, lad_df5, lad_df6)

# Save
saveRDS(object = lad,
        file = 'data/lad_processed.RDS')
