calc_annual_oha <- function(files_to_munge_ind, lake_crosswalk, benthic_area, out_file) {
  # code to summarize optical habitat estimates
  # down to annual values expressed as percent of total benthic area
  
  # munge scenario data to plot area vs clarity
  # find files to map over
  files_map <- read.table(files_to_munge_ind,  sep = ':')
  files_map <- files_map$V1
  
  # temporary, just go through files that have
  # completed running
  files_map <- files_map
  
  # function to read in data, collapse, bind rows
  summarize_clarity <- function(in_file){
    dat <- readr::read_csv(in_file) %>%
      mutate(year = lubridate::year(date)) %>%
      mutate(secchi_scenario = 1.7/scenario) %>%
      group_by(site_id, year, secchi_scenario) %>%
      summarize(annual_opti_hab = sum(opti_hab))
    return(dat)
  }
  
  total_benthic <- readr::read_csv(benthic_area)
  lake_meta <- readr::read_csv(lake_crosswalk)
  all <- purrr::map_dfr(files_map, summarize_clarity) %>%
    left_join(total_benthic) %>%
    mutate(perc_benthic_oha = 100*(annual_opti_hab/(365*total_benthic_area))) %>%
    ungroup()
  
  all_n <- all %>%
    left_join(select(lake_meta, site_id, lake_name)) %>%
    mutate(print_name = ifelse(is.na(lake_name), site_id, lake_name),
           print_name_unique = ifelse(is.na(lake_name), site_id, paste0(lake_name, ' (', site_id, ')')))
  
  readr::write_csv(all_n, out_file)
}

collapse_annual_oha <- function(data_in, lake_crosswalk, out_file){
  
  years_collapsed <- readr::read_csv(data_in) %>%
    group_by(print_name_unique, print_name, site_id, secchi_scenario) %>%
    summarize(avg_perc = mean(perc_benthic_oha),
              sd = sd(perc_benthic_oha),
              min = min(perc_benthic_oha),
              max = max(perc_benthic_oha)) %>%
    ungroup() %>%
    left_join(readr::read_csv(lake_crosswalk))
  
  readr::write_csv(years_collapsed, out_file)
  
}

calc_oha_metrics <- function(oha_annual, mod_clarity, out_file) {
  
  all_n <- readr::read_csv(oha_annual)
  #years_collapsed <- readr::read_csv(oha_collapsed)
  mod_clarity <- readr::read_csv(mod_clarity)
  #meta <- readr::read_csv('1_fetch/out/lake_metadata.csv')
  
  # pull 2018 clarity value
  clarity_2018 <- filter(mod_clarity, year %in% 2018) %>%
    mutate(secchi_m_2018 = 1.7/annual_mean_kd) %>%
    mutate(rsecchi_m_2018 = plyr::round_any(secchi_m_2018, 0.25))
  # calculate summary metrics
  # for each year, calculate:
  # range of values > 5%
  # slope at peak OH

  # metrics should characterize
  # 1) potential of the lake (e.g., if optimal clarity was achieved)
  # This includes range of secchi values where OH > 5% of total benthic
  # 2) Sensitivity of lake
  # this includes the slope at the optimal clarity, and slope at current secchi
  
  optimal_clarity_all <- all_n %>%
    mutate(secchi_scenario = round(secchi_scenario, 2)) %>% # some weird number issues on these scenarios
    group_by(site_id, print_name_unique, print_name, year) %>%
    mutate(optimal_clarity = perc_benthic_oha == max(perc_benthic_oha)) %>%
    left_join(select(clarity_2018, site_id, secchi_m_2018)) %>%
    mutate(scenario_equal_clarity2018 = secchi_scenario == plyr::round_any(secchi_m_2018, 0.25))
  
  
  # calculate slope at each interval
  # several ways to calculate slope
  # pp = percentage points = change in total percent of benthic habitat per scenario interval
  # area_per_interval = change in optical habitat area per scenario interval
  # p_per_interval = percent change in optical habitat area per scenario interval
  # i is increase in secchi from value to next interval (e.g., for 0.5, the slope is change from 0.5 to 0.75)
  slope <- ungroup(optimal_clarity_all) %>%
    arrange(site_id, year, secchi_scenario) %>%
    group_by(site_id, year) %>%
    mutate(
      slope_pp_per_interval_i = c(diff(perc_benthic_oha), NA), # by percentage points
      slope_area_per_interval_i = c(diff(annual_opti_hab), NA), # by area
      slope_p_per_interval_i = c(100*(diff(annual_opti_hab)/annual_opti_hab[1:(length(annual_opti_hab)-1)]), NA),
      slope_pp_per_interval_d = c(NA, diff(perc_benthic_oha)*-1), # by percentage points
      slope_area_per_interval_d = c(NA, diff(annual_opti_hab)*-1), # by area
      slope_p_per_interval_d = c(NA, -1*100*(diff(annual_opti_hab)/annual_opti_hab[1:(length(annual_opti_hab)-1)]))
      )

  # calculating per year and then taking the average
  threshold_clarity_peryear <- all_n %>%
    group_by(site_id, year) %>%
    summarize(annual_min_clarity_5 = min(secchi_scenario[perc_benthic_oha >=5]),
              annual_max_clarity_5 = max(secchi_scenario[perc_benthic_oha >=5]),
              annual_clarity_window_width_5 = length(unique(secchi_scenario[perc_benthic_oha >=5]))*0.25,
              annual_clarity_range_5 = annual_max_clarity_5 - annual_min_clarity_5)
  
  threshold_clarity_avg <- threshold_clarity_peryear %>%
    ungroup() %>%
    group_by(site_id) %>%
    summarize(min_clarity_5PBOH = mean(annual_min_clarity_5),
              max_clarity_5PBOH = mean(annual_max_clarity_5),
              clarity_window_width_5PBOH = mean(annual_clarity_window_width_5),
              clarity_range_5PBOH = mean(annual_clarity_range_5))
  
  annual_slope_metrics <- ungroup(slope) %>%
    group_by(site_id, year) %>%
    summarize(
      optimal_slope_pp_per_interval_i = slope_pp_per_interval_i[optimal_clarity],
      optimal_slope_area_per_interval_i = slope_area_per_interval_i[optimal_clarity],
      optimal_slope_p_per_interval_i = slope_p_per_interval_i[optimal_clarity],
      optimal_slope_pp_per_interval_d = slope_pp_per_interval_d[optimal_clarity],
      optimal_slope_area_per_interval_d = slope_area_per_interval_d[optimal_clarity],
      optimal_slope_p_per_interval_d = slope_p_per_interval_d[optimal_clarity],
      max_annual_oh = max(annual_opti_hab),
      max_annual_perc_oh = max(perc_benthic_oha),
      total_benthic_area = unique(total_benthic_area),
      current_annual_oh = annual_opti_hab[scenario_equal_clarity2018],
      current_annual_perc_oh = perc_benthic_oha[scenario_equal_clarity2018],
      current_annual_percofmax_oh = (current_annual_perc_oh/max_annual_perc_oh)*100,
      optimal_secchi = secchi_scenario[which.max(annual_opti_hab)],
      y2018_secchi = unique(secchi_m_2018),
      y2018_dist_optimal = round(optimal_secchi - plyr::round_any(unique(secchi_m_2018), 0.25), 2),
      y2018_slope_pp_per_interval_i = ifelse(all(is.na(scenario_equal_clarity2018)), NA, slope_pp_per_interval_i[scenario_equal_clarity2018]),
      y2018_slope_area_per_interval_i = ifelse(all(is.na(scenario_equal_clarity2018)), NA, slope_area_per_interval_i[scenario_equal_clarity2018]),
      y2018_slope_p_per_interval_i = ifelse(all(is.na(scenario_equal_clarity2018)), NA, slope_p_per_interval_i[scenario_equal_clarity2018]),
      y2018_slope_pp_per_interval_d = ifelse(all(is.na(scenario_equal_clarity2018)), NA, slope_pp_per_interval_d[scenario_equal_clarity2018]),
      y2018_slope_area_per_interval_d = ifelse(all(is.na(scenario_equal_clarity2018)), NA, slope_area_per_interval_d[scenario_equal_clarity2018]),
      y2018_slope_p_per_interval_d = ifelse(all(is.na(scenario_equal_clarity2018)), NA, slope_p_per_interval_d[scenario_equal_clarity2018])) %>%
    ungroup()
  
  # collapse year-specific estimates
  lake_slope_metrics <- annual_slope_metrics %>%
    group_by(site_id) %>%
    summarize(across(optimal_slope_pp_per_interval_i:y2018_slope_p_per_interval_d, 
              list(min = min, mean = mean, max = max), .names = "{.col}_{.fn}"),
              lake_area_km2 = unique(total_benthic_area)/1000000) %>%
    left_join(distinct(select(all_n, site_id, print_name, print_name_unique))) %>%
    left_join(threshold_clarity_avg)

  readr::write_csv(lake_slope_metrics, out_file)
  
}

test <- function() {
  lake_slope_metrics <- slope_metrics %>%
    group_by(site_id) %>%
    summarize(current_pct_max_oh = mean(perc_max_oha_y2018),
              current_pct_max_oh_min = min(perc_max_oha_y2018),
              current_pct_max_oh_max = max(perc_max_oha_y2018),
              current_slope_pp_per_interval_i = mean(y2018_slope_pp_per_interval_i),
              current_slope_pp_per_interval_d = mean(y2018_slope_pp_per_interval_d),
              optimal_slope_pp_per_interval_i  = mean(optimal_slope_pp_per_interval_i),
              optimal_slope_pp_per_interval_d = mean(optimal_slope_pp_per_interval_d),
              clarity_change_to_optimal_oh = mean(y2018_dist_optimal),
              clarity_change_to_optimal_oh_min = min(y2018_dist_optimal),
              clarity_change_to_optimal_oh_max = max(y2018_dist_optimal),
              mean_max_pct_oh = mean(max_perc_benthic_oha),
              current_pct_oh = mean(perc_benthic_oha_y2018),
              lake_area_km2 = unique(total_benthic_area)/1000000)
  
  # normalize each secchi scenario to the change from 2018
  # this will be approximate because we are rounding the 2018 value to the nearest 0.25
  # this will be better once we round to the 0.1
  change_from_2018 <- slope %>%
    mutate(change_from_2018 = round(secchi_scenario - plyr::round_any(secchi_m_2018, 0.25), 2)) %>%
    group_by(site_id, change_from_2018) %>%
    summarize(mean_perc_benthic_oha = mean(perc_benthic_oha)) %>% ungroup()
  
  library(ggridges)
  library(ggplot2)
  ggplot(filter(change_from_2018, change_from_2018 <2.25 & change_from_2018 >-2.25), aes(x = mean_perc_benthic_oha, y = factor(change_from_2018))) +
    stat_density_ridges(from = 0, to = 10, quantile_lines = 2, quantiles = 2, alpha = 0.7) +
    geom_vline(xintercept = 5, color = 'red', linetype = 2)
  
  # create a figure showing sensitivity vs possibility
  ggplot(annual_metrics, aes(x = current_pct_oh, y = current_slope_pp_per_interval_i)) +
    geom_point() +
    geom_vline(xintercept = 5) +
    geom_hline(yintercept = c(-1, 1)) +
    labs(y = "Percentage point change in OH\nper 0.25m increase in Secchi",
         x = '%OH given 2018 modeled Secchi',
         subtitle = "OH and sensitivity to change given +0.25m Secchi scenario")
  ggplot(annual_metrics, aes(x = current_pct_oh, y = current_slope_pp_per_interval_d)) +
    geom_point() +
    geom_vline(xintercept = 5) +
    geom_hline(yintercept = c(-1, 1)) +
    labs(y = "Percentage point change in OH\nper 0.25m decrease in Secchi",
         x = '%OH given 2018 modeled Secchi',
         subtitle = "OH and sensitivity to change given -0.25m Secchi scenario")
  
  # sensitivity to increase vs sensitivity to decrease
  # at current secchi
  ggplot(annual_metrics, aes(x = current_slope_pp_per_interval_d, y = current_slope_pp_per_interval_i)) +
    geom_point(aes(color = current_pct_oh)) +
    geom_abline(slope = 1, intercept = 0) +
    geom_vline(xintercept = 0, color = 'red', linetype = 2) +
    geom_hline(yintercept = 0, color = 'red', linetype = 2)
  
  # sensitivity to increase vs sensitivity to decrease in secchi
  # at optimal secchi
  ggplot(annual_metrics, aes(x = optimal_slope_pp_per_interval_d, 
                             y = optimal_slope_pp_per_interval_i)) +
    geom_point(aes(color = mean_max_pct_oh)) +
    geom_abline(slope = 1, intercept = 0) +
    scale_color_viridis_c()
  
  
  
  ggplot(annual_metrics, aes(x = current_pct_max_oh, y = clarity_change_to_optimal_oh)) +
    geom_errorbar(aes(ymin = clarity_change_to_optimal_oh_min,
                      ymax = clarity_change_to_optimal_oh_max), color = 'darkgray') +
    geom_errorbarh(aes(xmin = current_pct_max_oh_min, 
                       xmax = current_pct_max_oh_max), color = 'darkgray') +
    geom_point(aes(color = category, size = lake_area_km2), alpha = 0.7) +
    scale_size_continuous(breaks = c(10, 50, 100, 1000), range = c(0.1, 10)) +
    labs(x = "Current % of maximum OH", 
         y = "Change in secchi required\nto achive maximum OH",
         size = "Lake Area (km2)", color = '')
  
  ggplot(years_collapsed, aes(x = secchi_scenario, y = avg_perc)) +
    geom_line(aes(group = site_id, color = depth), alpha = 0.3) +
    scale_color_viridis_c(trans = 'log10', option = 'B')
  # calculate range of secchi values that leads to 
  # >5% of benthic habitat available
  
  # window width is number of secchi values * 0.25 >= 5% OH, accounts for non-monotonic changes
  # clarity range assumes all values between min and max achieve >5% OH
  
  
  
  
  
  # find distance to peak clarity
  distance_to_peak <- all_n %>%
    group_by(site_id, print_name_unique, print_name, year) %>%
    slice_max(perc_benthic_oha, n = 1) %>%
    ungroup() %>%
    left_join(select(clarity_2018, site_id, secchi_m_2018)) %>%
    mutate(distance_to_peak_m = secchi_scenario - secchi_m_2018) %>%
    group_by(site_id) %>%
    summarize(avg_2018_dist_peak_m = round(mean(distance_to_peak_m), 2)) %>%
    ungroup()
  
  # habitat at current clarity value
  clarity_join <- clarity_2018 %>%
    select(site_id, secchi_scenario = rsecchi_m_2018)%>%
    mutate(clarity2018 = TRUE)
  oh_at_2018clarity <- slope %>%
    left_join(clarity_join) %>%
    filter(!is.na(clarity2018))
  
  
  # find fraction of max 
  
  # calculate slope at peak
  # calculate 2018 distance (in secchi) from mean peak
  # calculate slope from 2018 value
  
  dist_from_optimal <- left_join(optimal_clarity, clarity_2018) %>%
    mutate(dist_from_opt = round(secchi_m_2018 - mean_optimal_clarity, 1))
}
  


plot_functions <- function() {
  library(ggplot2)
  ggplot(all, aes(x = secchi_scenario, y = perc_benthic_oha)) +
    geom_point(aes(group = year, color = factor(year))) +
    geom_line(aes(group = year, color = factor(year))) +
    geom_hline(aes(yintercept = 5)) +
    facet_wrap(~site_id) +
    theme_bw() +
    labs(x = 'Secchi scenario (m)', y = 'Annual optical habitat')
  
  ggplot(years_collapsed, aes(x = secchi_scenario, y = avg_perc)) +
    geom_ribbon(aes(ymax = max, ymin = min), alpha = 0.6, fill = "gray70") +
    geom_point() +
    geom_line() +
    geom_hline(aes(yintercept = 5), col = 'red', linetype = 2) +
    facet_wrap(~print_name) +
    theme_bw() +
    labs(x = "Clarity scenario\n(Secchi depth, m)", y = 'Percent of total benthic area')
  # test a tile plot idea
  # lake IDs on y axis, secchi depth on the x axis
  # each tile can have 3 colors
  # red = none of the years were >5 % 
  # yellow = 1-4 of the years were >5%
  # green = all 5 of the years were >5%
  # dark green = all > 5% and 1 or more >15
  # darker green = all >15%
  head(years_collapsed)
  
  tile_dat <- years_collapsed %>%
    mutate(habitat_bin = case_when(
      min >= 15 ~ 'all years >15%',
      min < 15 & max >= 15 ~ "some years >15%",
      max < 15 & min >= 5 ~ "all years 5-15%",
      max >= 5 & min <5 ~ 'some years 5-15%',
      max < 5 ~ 'all years <5%'))
  
  tile_dat$habitat_bin <- factor(tile_dat$habitat_bin, 
                                 levels = c('all years <5%', 'some years 5-15%', 'all years 5-15%', 'some years >15%', 'all years >15%'))
  
  lakes_size <- tile_dat %>%
    arrange(depth)
  
  tile_dat$print_name <- factor(tile_dat$print_name, levels = unique(lakes_size$print_name))
  p <- ggplot(tile_dat, aes(x = secchi_scenario, y = print_name)) +
    geom_tile(aes(fill = habitat_bin)) +
    scale_fill_manual(values = c('#A71D31', '#D5BF86', '#3A7D44', '#275436', '#132A1B')) +
    theme_bw() +
    labs(x = "Water Clarity Scenario\n(Secchi depth, m)",
         y = 'Lakes arranged by depth\n(deep lakes on top)',
         fill = '% Habitat\nAvailability',
         subtitle = 'Optical habitat availability given water clarity scenarios.\nColors refer to the number of years (of 5 model years)\nin different tiers of habitat availability.')
  ggsave('3_summarize/out/clarity_scenarios_tile.png', p, height = 10, width = 7)  
  
  
  }



  