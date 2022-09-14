summarize_clarity <- function(file_out, files_in){
  
  clarity_files <- names(yaml::read_yaml(files_in))
  
  read_and_summarize <- function(in_file){
    
    annual <- readr::read_csv(in_file) %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(year) %>%
      summarize(annual_mean_kd = mean(kd)) %>%
      mutate(site_id = gsub('(.*gam_)(.*)(_clarity.csv)', '\\2', in_file, perl = TRUE))
  }
  
  dat_c <- purrr::map_df(clarity_files, read_and_summarize)
  readr::write_csv(dat_c, file_out)
}