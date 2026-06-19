## Looking at LAI

rm(list = ls())

# Load LAI from NEON
lai_rast <- terra::rast(x = 'data/GIS_Files/LAI_Raster-2021/Leaf_area_Index_2021.tif')

# Load polygons for plot extents
poly_dat <- sf::st_read('data/GIS_Files/Neon_Plots_Shapefile/')

# Convert to spatVector
poly_dat <- terra::vect(poly_dat)

# Change CRS
lai_rast <- terra::project(lai_rast, terra::crs(poly_dat))

# List of individual plot extents
poly_split <- terra::split(poly_dat, 'plotID')

# Clip LAI to plot extents
lai_clip_006 <- terra::crop(lai_rast, poly_split[[1]])
lai_clip_007 <- terra::crop(lai_rast, poly_split[[2]])
lai_clip_013 <- terra::crop(lai_rast, poly_split[[3]])
lai_clip_019 <- terra::crop(lai_rast, poly_split[[4]])
lai_clip_020 <- terra::crop(lai_rast, poly_split[[5]])
lai_clip_025 <- terra::crop(lai_rast, poly_split[[6]])

# Just testing to make sure it looks right
terra::plot(lai_clip_006)
terra::plot(lai_clip_007)
terra::plot(lai_clip_013)
terra::plot(lai_clip_019)
terra::plot(lai_clip_020)
terra::plot(lai_clip_025)

# Make matrix
lai_mat_006 <- terra::as.data.frame(lai_clip_006, 
                                xy = TRUE)
lai_mat_007 <- terra::as.data.frame(lai_clip_007,
                                    xy = TRUE)
lai_mat_013 <- terra::as.data.frame(lai_clip_013,
                                    xy = TRUE)
lai_mat_019 <- terra::as.data.frame(lai_clip_019,
                                    xy = TRUE)
lai_mat_020 <- terra::as.data.frame(lai_clip_020,
                                    xy = TRUE)
lai_mat_025 <- terra::as.data.frame(lai_clip_025,
                                    xy = TRUE)

lai_mat_006 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = Leaf_area_Index_2021)) +
  khroma::scale_fill_cork(name = 'LAI') + # cork
  ggplot2::ggtitle('Base plot 006\n2021, Pre-burn') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/006_lai_spatial.png',
                dpi = 300)

lai_mat_007 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = Leaf_area_Index_2021)) +
  khroma::scale_fill_cork(name = 'LAI') +
  ggplot2::ggtitle('Base plot 007\n2021, Pre-burn') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lai_mat_020 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = Leaf_area_Index_2021)) +
  khroma::scale_fill_cork(name = 'LAI') +
  ggplot2::ggtitle('Base plot 020\n2021, Pre-Burn') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lai_mat_013 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = Leaf_area_Index_2021)) +
  khroma::scale_fill_cork(name = 'LAI') +
  ggplot2::ggtitle('Base plot 013\n2021, Unburned') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/013_lai_spatial.png',
                dpi = 300)

lai_mat_019 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = Leaf_area_Index_2021)) +
  khroma::scale_fill_cork(name = 'LAI') +
  ggplot2::ggtitle('Base plot 019\n2021, Unburned') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

lai_mat_025 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                  fill = Leaf_area_Index_2021)) +
  khroma::scale_fill_cork(name = 'LAI') +
  ggplot2::ggtitle('Base plot 025\n2021, Unburned') +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1,
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Combine burned vs unburned plots regardless of location
lai_comb_006 <- lai_mat_006 |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Burned',
                plot = '006')

lai_comb_007 <- lai_mat_007 |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Burned',
                plot = '007')

lai_comb_020 <- lai_mat_020 |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Burned',
                plot = '020')

lai_comb_013 <- lai_mat_013 |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Unburned',
                plot = '013')

lai_comb_019 <- lai_mat_019 |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Unburned',
                plot = '019')

lai_comb_025 <- lai_mat_025 |>
  dplyr::select(-x, -y) |>
  dplyr::mutate(type = 'Unburned',
                plot = '025')

# Combine
lai_comb <- rbind(lai_comb_006, lai_comb_007,
                  lai_comb_020, lai_comb_013,
                  lai_comb_019, lai_comb_025)

# Plot with plots separated
lai_comb |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = Leaf_area_Index_2021,
                                    fill = plot),
                       linewidth = 1) +
  khroma::scale_fill_bright() +
  ggplot2::xlab('') + ggplot2::ylab('LAI') +
  ggplot2::ggtitle('2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lai_violin.png',
                dpi = 300)

# Plot just by burned/unburned
lai_comb |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = type,
                                    y = Leaf_area_Index_2021),
                       fill = '#00000000', linewidth = 1) +
  ggplot2::xlab('') + ggplot2::ylab('LAI') +
  ggplot2::ggtitle('2021 (Pre-burn)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Komogorov-Smirnov test
ks <- ks.test(lai_comb$Leaf_area_Index_2021[which(lai_comb$type == 'Burned')],
              lai_comb$Leaf_area_Index_2021[which(lai_comb$type == 'Unburned')])
ks

# Mann-Whitney test
mw <- wilcox.test(lai_comb$Leaf_area_Index_2021[which(lai_comb$type == 'Burned')],
                  lai_comb$Leaf_area_Index_2021[which(lai_comb$type == 'Unburned')])
mw

## Maybe next try looking at relationship between
## composition and LAI at the plot level