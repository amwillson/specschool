## Visualizing LAD at MLBS distributed plots

## This script uses inputs of LAD estimated on a 1 cubic
## meter voxel for select distributed plots at MLBS

## The RDS input files were made as follows
## 1. Download NEON data product DP1.30003.001 for year
##    2021 and for the tiles that contain the desired
##    distributed plots using the NEON online data portal
## 2. Run the following two steps from the SPEC_School Github repo
##    a. spec_school_2026/ERSAM_Lab_3D_traits/2_canopy_structure/02_clean_pointcloud.R
##    b. spec_school_2026/ERSAM_Lab_3D_traits/3_canopy_structure/03_uncalibrated_LAD.R
## 3. Add a save in the 03_uncalibrated_LAD.R script after
##    creating the object lad.raster for each tile
##    NOTE THAT THE SAVE MUST BE DONE SO THAT THE FILE NAME 
##    CHANGES FOR EACH ITERATION OF THE LOOP

rm(list = ls())

# Read in LAD files
lad_1 <- readRDS('data/lad_lai_processed/lad_2021_1.RDS')
lad_2 <- readRDS('data/lad_lai_processed/lad_2021_2.RDS')
lad_3 <- readRDS('data/lad_lai_processed/lad_2021_3.RDS')

# Load polygons for plot extents
poly_dat <- sf::st_read('data/GIS_Files/Neon_Plots_Shapefile/')

# Convert to spatVector
poly_dat <- terra::vect(poly_dat)

# Change CRS
lad_1 <- terra::project(lad_1, terra::crs(poly_dat))
lad_2 <- terra::project(lad_2, terra::crs(poly_dat))
lad_3 <- terra::project(lad_3, terra::crs(poly_dat))

# List of individual plot extents
poly_split <- terra::split(poly_dat, 'plotID')

# Clip LAD to plot extents
## Note that I had to manually find which of the three
## rasters contained each plot
lad_clip_006 <- terra::crop(lad_2, poly_split[[1]])
lad_clip_007 <- terra::crop(lad_1, poly_split[[2]])
lad_clip_013 <- terra::crop(lad_2, poly_split[[3]])
lad_clip_019 <- terra::crop(lad_3, poly_split[[4]])
lad_clip_020 <- terra::crop(lad_2, poly_split[[5]])
lad_clip_025 <- terra::crop(lad_2, poly_split[[6]])

# Just testing to make sure it looks right
terra::plot(lad_clip_006)
terra::plot(lad_clip_007)
terra::plot(lad_clip_013)
terra::plot(lad_clip_019)
terra::plot(lad_clip_020)
terra::plot(lad_clip_025)

# Make dataframes
lad_mat_006 <- terra::as.data.frame(lad_clip_006, 
                                    xy = TRUE)
lad_mat_007 <- terra::as.data.frame(lad_clip_007,
                                    xy = TRUE)
lad_mat_013 <- terra::as.data.frame(lad_clip_013,
                                    xy = TRUE)
lad_mat_019 <- terra::as.data.frame(lad_clip_019,
                                    xy = TRUE)
lad_mat_020 <- terra::as.data.frame(lad_clip_020,
                                    xy = TRUE)
lad_mat_025 <- terra::as.data.frame(lad_clip_025,
                                    xy = TRUE)

height_to_bin <- as.data.frame(cbind(colnames(lad_mat_006)[3:58],
                       c(rep('1-5', 5), rep('6-10', 5),
                         rep('11-15', 5), rep('16-20', 5),
                         rep('21-25', 5), rep('26-30', 5),
                         rep('>30', 26))))
colnames(height_to_bin) <- c('height', 'height_bin')

# Bin order
bin_order <- c('1-5', '6-10',
               '11-15', '16-20',
               '21-25', '26-30',
               '>30')

# Plot vertical profile of LAD summed across y dimension
p_006 <- lad_mat_006 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::group_by(x, height) |>
  dplyr::summarize(lad = mean(lad, na.rm = TRUE)) |>
  dplyr::mutate(height_m = sub(pattern = '.*_', 
                               replacement = '',
                               x = height),
                height_m = as.numeric(height_m)) |>
  dplyr::filter(height_m <= 30) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = height_m,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') +
  ggplot2::ggtitle('Base plot 006\n2021, Pre-burn') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

p_007 <- lad_mat_007 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::group_by(x, height) |>
  dplyr::summarize(lad = mean(lad, na.rm = TRUE)) |>
  dplyr::mutate(height_m = sub(pattern = '.*_', 
                               replacement = '',
                               x = height),
                height_m = as.numeric(height_m)) |>
  dplyr::filter(height_m <= 30) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = height_m,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') +
  ggplot2::ggtitle('Base plot 007\n2021, Pre-burn') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

p_020 <- lad_mat_020 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::group_by(x, height) |>
  dplyr::summarize(lad = mean(lad, na.rm = TRUE)) |>
  dplyr::mutate(height_m = sub(pattern = '.*_', 
                               replacement = '',
                               x = height),
                height_m = as.numeric(height_m)) |>
  dplyr::filter(height_m <= 30) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = height_m,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') +
  ggplot2::ggtitle('Base plot 020\n2021, Pre-burn') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

p_013 <- lad_mat_013 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::group_by(x, height) |>
  dplyr::summarize(lad = mean(lad, na.rm = TRUE)) |>
  dplyr::mutate(height_m = sub(pattern = '.*_', 
                               replacement = '',
                               x = height),
                height_m = as.numeric(height_m)) |>
  dplyr::filter(height_m <= 30) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = height_m,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') +
  ggplot2::ggtitle('Base plot 013\n2021, Unburned') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

p_019 <- lad_mat_019 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::group_by(x, height) |>
  dplyr::summarize(lad = mean(lad, na.rm = TRUE)) |>
  dplyr::mutate(height_m = sub(pattern = '.*_', 
                               replacement = '',
                               x = height),
                height_m = as.numeric(height_m)) |>
  dplyr::filter(height_m <= 30) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = height_m,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') +
  ggplot2::ggtitle('Base plot 019\n2021, Unburned') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

p_025 <- lad_mat_025 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::group_by(x, height) |>
  dplyr::summarize(lad = mean(lad, na.rm = TRUE)) |>
  dplyr::mutate(height_m = sub(pattern = '.*_', 
                               replacement = '',
                               x = height),
                height_m = as.numeric(height_m)) |>
  dplyr::filter(height_m <= 30) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = height_m,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') +
  ggplot2::ggtitle('Base plot 025\n2021, Unburned') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

cowplot::plot_grid(p_006 + ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                                          legend.title = ggplot2::element_text(size = 8)),
                   p_007 + ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                                          legend.title = ggplot2::element_text(size = 8)), 
                   p_020 + ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                                          legend.title = ggplot2::element_text(size = 8)),
                   p_013 + ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                                          legend.title = ggplot2::element_text(size = 8)), 
                   p_019 + ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                                          legend.title = ggplot2::element_text(size = 8)), 
                   p_025 + ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                                          legend.title = ggplot2::element_text(size = 8)),
                   nrow = 2)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lad_side_profile.png',
                dpi = 300)

# Plot overhead of each LAD height bin
lad_mat_006 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') + # cork
  ggplot2::facet_wrap(~factor(height_bin, bin_order)) +
  ggplot2::ggtitle('Base plot 006\n2021, Pre-burn') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_mat_007 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') + # cork
  ggplot2::facet_wrap(~factor(height_bin, bin_order)) +
  ggplot2::ggtitle('Base plot 007\n2021, Pre-burn') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_mat_020 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') + # cork
  ggplot2::facet_wrap(~factor(height_bin, bin_order)) +
  ggplot2::ggtitle('Base plot 020\n2021, Pre-burn') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_mat_013 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') + # cork
  ggplot2::facet_wrap(~factor(height_bin, bin_order)) +
  ggplot2::ggtitle('Base plot 013\n2021, Unburned') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_mat_019 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') + # cork
  ggplot2::facet_wrap(~factor(height_bin, bin_order)) +
  ggplot2::ggtitle('Base plot 019\n2021, Unburned') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_mat_025 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = lad)) +
  khroma::scale_fill_cork(name = 'LAD') + # cork
  ggplot2::facet_wrap(~factor(height_bin, bin_order)) +
  ggplot2::ggtitle('Base plot 025\n2021, Unburned') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Combine burned vs unburned plots regardless of location
lad_comb_006 <- lad_mat_006 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  dplyr::ungroup() |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Burned',
                plot = '006')

lad_comb_007 <- lad_mat_007 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  dplyr::ungroup() |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Burned',
                plot = '007')

lad_comb_020 <- lad_mat_020 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  dplyr::ungroup() |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Burned',
                plot = '020')

lad_comb_013 <- lad_mat_013 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  dplyr::ungroup() |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Unburned',
                plot = '013')

lad_comb_019 <- lad_mat_019 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  dplyr::ungroup() |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Unburned',
                plot = '019')

lad_comb_025 <- lad_mat_025 |>
  tidyr::pivot_longer(cols = -c(x, y),
                      names_to = 'height',
                      values_to = 'lad') |>
  dplyr::left_join(y = height_to_bin,
                   by = 'height') |>
  dplyr::group_by(x, y, height_bin) |>
  dplyr::summarize(lad = sum(lad, na.rm = TRUE)) |>
  dplyr::filter(height_bin != '>30') |>
  dplyr::ungroup() |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Unburned',
                plot = '025')

# Combine
lad_comb <- rbind(lad_comb_006, lad_comb_007,
                  lad_comb_020, lad_comb_013,
                  lad_comb_019, lad_comb_025)

# Plot with plots separated
lad_comb |>
  dplyr::filter(height_bin == '1-5') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad,
                                    fill = plot),
                       linewidth = 1) +
  khroma::scale_fill_bright() +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 1-5 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lad_violin_1-5.png',
                dpi = 300)

lad_comb |>
  dplyr::filter(height_bin == '6-10') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad,
                                    fill = plot),
                       linewidth = 1) +
  khroma::scale_fill_bright() +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 6-10 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_comb |>
  dplyr::filter(height_bin == '11-15') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad,
                                    fill = plot),
                       linewidth = 1) +
  khroma::scale_fill_bright() +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 11-15 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_comb |>
  dplyr::filter(height_bin == '16-20') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad,
                                    fill = plot),
                       linewidth = 1) +
  khroma::scale_fill_bright() +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 16-20 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_comb |>
  dplyr::filter(height_bin == '21-25') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad,
                                    fill = plot),
                       linewidth = 1) +
  khroma::scale_fill_bright() +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 21-25 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lad_violin_21-25.png',
                dpi = 300)

lad_comb |>
  dplyr::filter(height_bin == '26-30') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad,
                                    fill = plot),
                       linewidth = 1) +
  khroma::scale_fill_bright() +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 26-30 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Plot just by burned/unburned
lad_comb |>
  dplyr::filter(height_bin == '1-5') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad),
                       fill = '#00000000', linewidth = 1) +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 1-5 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_comb |>
  dplyr::filter(height_bin == '6-10') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad),
                       fill = '#00000000', linewidth = 1) +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 6-10 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_comb |>
  dplyr::filter(height_bin == '11-15') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad),
                       fill = '#00000000', linewidth = 1) +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 11-15 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_comb |>
  dplyr::filter(height_bin == '16-20') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad),
                       fill = '#00000000', linewidth = 1) +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 16-20 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_comb |>
  dplyr::filter(height_bin == '21-25') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad),
                       fill = '#00000000', linewidth = 1) +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 21-25 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lad_comb |>
  dplyr::filter(height_bin == '26-30') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = lad),
                       fill = '#00000000', linewidth = 1) +
  ggplot2::xlab('') + ggplot2::ylab('LAD') +
  ggplot2::ggtitle('Leaf area density: 26-30 m\n2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Komogorov-Smirnov test
ks1 <- ks.test(lad_comb$lad[which(lad_comb$type == 'Burned' & 
                                   lad_comb$height_bin == '1-5')],
              lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                   lad_comb$height_bin == '1-5')])
ks1

ks2 <- ks.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                   lad_comb$height_bin == '6-10')],
              lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                   lad_comb$height_bin == '6-10')])
ks2

ks3 <- ks.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                   lad_comb$height_bin == '11-15')],
              lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                   lad_comb$height_bin == '11-15')])
ks3

ks4 <- ks.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                    lad_comb$height_bin == '16-20')],
               lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                    lad_comb$height_bin == '16-20')])
ks4

ks5 <- ks.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                    lad_comb$height_bin == '21-25')],
               lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                    lad_comb$height_bin == '21-25')])
ks5

ks6 <- ks.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                    lad_comb$height_bin == '26-30')],
               lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                    lad_comb$height_bin == '26-30')])
ks6

# Mann-Whitney test
mw1 <- wilcox.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                       lad_comb$height_bin == '1-5')],
                  lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                       lad_comb$height_bin == '1-5')])
mw1

mw2 <- wilcox.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                        lad_comb$height_bin == '6-10')],
                   lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                        lad_comb$height_bin == '6-10')])
mw2

mw3 <- wilcox.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                        lad_comb$height_bin == '11-15')],
                   lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                        lad_comb$height_bin == '11-15')])
mw3

mw4 <- wilcox.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                        lad_comb$height_bin == '16-20')],
                   lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                        lad_comb$height_bin == '16-20')])
mw4

mw5 <- wilcox.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                        lad_comb$height_bin == '21-25')],
                   lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                        lad_comb$height_bin == '21-25')])
mw5

mw6 <- wilcox.test(lad_comb$lad[which(lad_comb$type == 'Burned' &
                                        lad_comb$height_bin == '26-30')],
                   lad_comb$lad[which(lad_comb$type == 'Unburned' &
                                        lad_comb$height_bin == '26-30')])
mw6

## Need to standardize effect sizes?
