rm(list = ls())

# Install neonOS package for joining NEON observational data
#devtools::install_github('NEONScience/NEON-OS-data-processing/neonOS')

# Stack all plant structure data
neonUtilities::stackByTable('~/Downloads/NEON_struct-plant.zip',
                            savepath = 'data')

# Load apparent individual & mapping and flagging
app_ind <- readr::read_csv('data/stackedFiles/vst_apparentindividual.csv')
map_tag <- readr::read_csv('data/stackedFiles/vst_mappingandtagging.csv')

dat <- neonOS::joinTableNEON(table1 = app_ind,
                             table2 = map_tag,
                             name1 = 'vst_apparentindividual',
                             name2 = 'vst_mappingandtagging')

plots <- c('MLBS_006', 'MLBS_007', 'MLBS_020',
           'MLBS_013', 'MLBS_025', 'MLBS_019')

dat_proc <- dat |>
  dplyr::select(individualID, plotID, taxonID, eventID, plantStatus, canopyPosition,
                height) |>
  dplyr::filter(plotID %in% plots) |>
  dplyr::filter(plantStatus == 'Live')

## Canopy position is often missing so let's use height instead
## Histogram of height to see where canopy height might be
dat_proc |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = height),
                        linewidth = 1) +
  ggplot2::coord_flip() +
  ggplot2::ylab('Density function') +
  ggplot2::xlab('Height (m)') +
  ggplot2::theme_minimal()

dat_proc |>
  dplyr::mutate(plotID = dplyr::if_else(plotID == 'MLBS_006', '006 Burned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_007', '007 Burned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_020', '020 Burned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_013', '013 Unburned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_019', '019 Unburned', plotID)) |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = height,
                                     color = factor(plotID,
                                                    levels = c('006 Burned', '007 Burned', '020 Burned',
                                                               '013 Unburned', '019 Unburned'))),
                        linewidth = 1) +
  ggplot2::coord_flip() +
  ggplot2::ylab('Density function') +
  ggplot2::xlab('Height (m)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

## About 18 seems good here

# Filter for height >= 18
dat_canopy <- dat_proc |>
  dplyr::filter(height >= 18)

sum_by_plotyear <- dat_canopy |>
  dplyr::group_by(plotID, taxonID, eventID) |>
  dplyr::count()

tot_by_plotyear <- dat_canopy |>
  dplyr::group_by(plotID, eventID) |>
  dplyr::count() |>
  dplyr::rename(n_tot = n)

sum_by_plotyear <- sum_by_plotyear |>
  dplyr::left_join(y = tot_by_plotyear,
                   by = c('plotID', 'eventID')) |>
  dplyr::mutate(frac = n / n_tot) |>
  dplyr::mutate(burned = dplyr::if_else(plotID %in% c('MLBS_006', 'MLBS_007', 'MLBS_020'), TRUE, FALSE))

sum_by_plot <- sum_by_plotyear |>
  dplyr::group_by(plotID, taxonID) |>
  dplyr::summarize(mean_n = mean(n)) |>
  dplyr::mutate(mean_n_tot = sum(mean_n),
                mean_frac = mean_n / mean_n_tot)

sum_by_plotyear |>
  dplyr::mutate(plotID = dplyr::if_else(plotID == 'MLBS_006', '006\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_007', '007\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_020', '020\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_013', '013\nUnburned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_019', '019\nUnburned', plotID)) |>
  dplyr::mutate(eventID = dplyr::if_else(eventID == 'vst_MLBS_2018', '2018', eventID),
                eventID = dplyr::if_else(eventID == 'vst_MLBS_2019', '2019', eventID),
                eventID = dplyr::if_else(eventID == 'vst_MLBS_2024', '2024', eventID)) |>
  dplyr::mutate(taxonID = dplyr::if_else(taxonID == 'ACRU', 'Red maple', taxonID),
                taxonID = dplyr::if_else(taxonID == 'AMLA', 'Allegheny serviceberry', taxonID),
                taxonID = dplyr::if_else(taxonID == 'OXAR', 'Sourwood', taxonID),
                taxonID = dplyr::if_else(taxonID == 'PIEC2', 'Shortleaf pine', taxonID),
                taxonID = dplyr::if_else(taxonID == 'PITA', 'Loblolly pine', taxonID),
                taxonID = dplyr::if_else(taxonID == 'PRSE2', 'Black cherry', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QUAL', 'White oak', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QUCO2', 'Scarlet oak', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QURU', 'Red oak', taxonID)) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = '', y = frac, fill = taxonID),
                    stat = 'identity') +
  ggplot2::coord_polar('y', start=0) +
  ggplot2::scale_fill_discrete(palette = 'Spectral',
                               name = 'Taxon') +
  ggplot2::facet_grid(factor(plotID, levels = c('006\nBurned', '007\nBurned', '020\nBurned',
                                                '013\nUnburned', '019\nUnburned'))~eventID) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_blank())

sum_by_plot |>
  dplyr::mutate(plotID = dplyr::if_else(plotID == 'MLBS_006', '006\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_007', '007\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_020', '020\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_013', '013\nUnburned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_019', '019\nUnburned', plotID)) |>
  dplyr::mutate(taxonID = dplyr::if_else(taxonID == 'ACRU', 'Red maple', taxonID),
                taxonID = dplyr::if_else(taxonID == 'AMLA', 'Allegheny serviceberry', taxonID),
                taxonID = dplyr::if_else(taxonID == 'OXAR', 'Sourwood', taxonID),
                taxonID = dplyr::if_else(taxonID == 'PIEC2', 'Shortleaf pine', taxonID),
                taxonID = dplyr::if_else(taxonID == 'PITA', 'Loblolly pine', taxonID),
                taxonID = dplyr::if_else(taxonID == 'PRSE2', 'Black cherry', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QUAL', 'White oak', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QUCO2', 'Scarlet oak', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QURU', 'Red oak', taxonID)) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = '', y = mean_frac, fill = taxonID),
                    stat = 'identity') +
  ggplot2::coord_polar('y', start=0) +
  ggplot2::scale_fill_discrete(palette = 'Spectral',
                               name = 'Taxon') +
  ggplot2::facet_wrap(~factor(plotID, levels = c('006\nBurned', '007\nBurned', '020\nBurned',
                                                 '013\nUnburned', '019\nUnburned'))) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_blank())

# Filter for height <= 5
dat_understory <- dat_proc |>
  dplyr::filter(height <= 5) |>
  dplyr::filter(!is.na(taxonID))

sum_by_plotyear2 <- dat_understory |>
  dplyr::group_by(plotID, taxonID, eventID) |>
  dplyr::count()

tot_by_plotyear2 <- dat_understory |>
  dplyr::group_by(plotID, eventID) |>
  dplyr::count() |>
  dplyr::rename(n_tot = n)

sum_by_plotyear2 <- sum_by_plotyear2 |>
  dplyr::left_join(y = tot_by_plotyear2,
                   by = c('plotID', 'eventID')) |>
  dplyr::mutate(frac = n / n_tot) |>
  dplyr::mutate(burned = dplyr::if_else(plotID %in% c('MLBS_006', 'MLBS_007', 'MLBS_020'), TRUE, FALSE))

sum_by_plot2 <- sum_by_plotyear2 |>
  dplyr::group_by(plotID, taxonID) |>
  dplyr::summarize(mean_n = mean(n)) |>
  dplyr::mutate(mean_n_tot = sum(mean_n),
                mean_frac = mean_n / mean_n_tot)

sum_by_plotyear2 |>
  dplyr::mutate(plotID = dplyr::if_else(plotID == 'MLBS_006', '006\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_007', '007\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_020', '020\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_013', '013\nUnburned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_019', '019\nUnburned', plotID)) |>
  dplyr::mutate(eventID = dplyr::if_else(eventID == 'vst_MLBS_2018', '2018', eventID),
                eventID = dplyr::if_else(eventID == 'vst_MLBS_2019', '2019', eventID),
                eventID = dplyr::if_else(eventID == 'vst_MLBS_2024', '2024', eventID)) |>
  dplyr::mutate(taxonID = dplyr::if_else(taxonID == 'ACPE', 'Striped maple', taxonID),
                taxonID = dplyr::if_else(taxonID == 'ACRU', 'Red maple', taxonID),
                taxonID = dplyr::if_else(taxonID == 'ELUM', 'Autumn olive', taxonID),
                taxonID = dplyr::if_else(taxonID == 'HAVI4', 'American witchhazel', taxonID),
                taxonID = dplyr::if_else(taxonID == 'ILOP', 'American holly', taxonID),
                taxonID = dplyr::if_else(taxonID == 'KALA', 'Mountain laurel', taxonID),
                taxonID = dplyr::if_else(taxonID == 'OXAR', 'Sourwood', taxonID),
                taxonID = dplyr::if_else(taxonID == 'PIST', 'White pine', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QUAL', 'White oak', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QURU', 'Red oak', taxonID),
                taxonID = dplyr::if_else(taxonID == 'SAAL5', 'Sassafras', taxonID),
                taxonID = dplyr::if_else(taxonID == 'VAST', 'Deerberry', taxonID)) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = '', y = frac, fill = taxonID),
                    stat = 'identity') +
  ggplot2::coord_polar('y', start=0) +
  ggplot2::scale_fill_discrete(palette = 'Paired',
                               name = 'Taxon') +
  ggplot2::facet_grid(factor(plotID, levels = c('006\nBurned', '007\nBurned', '020\nBurned',
                                                '013\nUnburned', '019\nUnburned'))~eventID) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_blank())

sum_by_plot2 |>
  dplyr::mutate(plotID = dplyr::if_else(plotID == 'MLBS_006', '006\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_007', '007\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_020', '020\nBurned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_013', '013\nUnburned', plotID),
                plotID = dplyr::if_else(plotID == 'MLBS_019', '019\nUnburned', plotID)) |>
  dplyr::mutate(taxonID = dplyr::if_else(taxonID == 'ACPE', 'Striped maple', taxonID),
                taxonID = dplyr::if_else(taxonID == 'ACRU', 'Red maple', taxonID),
                taxonID = dplyr::if_else(taxonID == 'ELUM', 'Autumn olive', taxonID),
                taxonID = dplyr::if_else(taxonID == 'HAVI4', 'American witchhazel', taxonID),
                taxonID = dplyr::if_else(taxonID == 'ILOP', 'American holly', taxonID),
                taxonID = dplyr::if_else(taxonID == 'KALA', 'Mountain laurel', taxonID),
                taxonID = dplyr::if_else(taxonID == 'OXAR', 'Sourwood', taxonID),
                taxonID = dplyr::if_else(taxonID == 'PIST', 'White pine', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QUAL', 'White oak', taxonID),
                taxonID = dplyr::if_else(taxonID == 'QURU', 'Red oak', taxonID),
                taxonID = dplyr::if_else(taxonID == 'SAAL5', 'Sassafras', taxonID),
                taxonID = dplyr::if_else(taxonID == 'VAST', 'Deerberry', taxonID)) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = '', y = mean_frac, fill = taxonID),
                    stat = 'identity') +
  ggplot2::coord_polar('y', start=0) +
  ggplot2::scale_fill_discrete(palette = 'Paired',
                               name = 'Taxon') +
  ggplot2::facet_wrap(~factor(plotID, levels = c('006\nBurned', '007\nBurned', '020\nBurned',
                                                 '013\nUnburned', '019\nUnburned'))) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_blank())
