# code to summarize optical habitat estimates
# down to annual values expressed as percent of total benthic area

# munge scenario data to plot area vs clarity
# find files to map over
files_map <- read.table('2_process/out/2_pgdl_lake_tasks.ind',  sep = ':')
files_map <- files_map$V1

# temporary, just go through files that have
# completed running
files_map <- files_map[1:16]

# function to read in data, collapse, bind rows
summarize_clarity <- function(in_file){
  dat <- readr::read_csv(in_file) %>%
    mutate(year = lubridate::year(date)) %>%
    mutate(secchi_scenario = 1.7/scenario) %>%
    group_by(site_id, year, secchi_scenario) %>%
    summarize(annual_opti_hab = sum(opti_hab))
  return(dat)
}
library(dplyr)
total_benthic <- readr::read_csv('3_summarize/out/total_benthic_areas.csv')
lake_meta <- readr::read_csv('1_fetch/out/lake_metadata.csv')
all <- purrr::map_dfr(files_map, summarize_clarity) %>%
  left_join(total_benthic) %>%
  mutate(perc_benthic_oha = 100*(annual_opti_hab/(365*total_benthic_area)))

years_collapsed <- all %>%
  group_by(site_id, secchi_scenario) %>%
  summarize(avg_perc = mean(perc_benthic_oha),
            sd = sd(perc_benthic_oha),
            min = min(perc_benthic_oha),
            max = max(perc_benthic_oha)) %>%
  left_join(lake_meta) %>%
  mutate(print_name = ifelse(is.na(lake_name), site_id, lake_name))
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
  
  
  
  
  