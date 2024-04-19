box::use(
    tidyverse[...],
    fs[...],
    reactlog[...],
    janitor[...],
    stringr[...],
    readr[...],
    tidyr[...],
    dplyr[...]
)

# list_of_files <- list.files(here::here('FSST', 'data')) #when testing without running the full app
list_of_files <- list.files(fs::path('app','data')) #when running the app
current_file <- list_of_files[str_detect(list_of_files, 'app_inputs')]
financial_outcome_file <- list_of_files[str_detect(list_of_files, 'financial_outcomes')]
# current_file <- '/Users/constanzebayer/Dropbox (2° Investing)/2° Investing Team/People/Constanze/GitHub/ST-App/FSST/data'
repository_data <- readr::read_csv(path('app', 'data', current_file))
# repository_data <- read_csv(here::here('FSST', 'data', current_file))
# repository_data <- read_csv('/Users/constanzebayer/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/1in1000/03_Content/02_ST-team/05_NGFS ST App/01_Tool Repository/03_ProcessedFiles/old data/app_inputs_2021-09-24.csv')
repository_data <- repository_data %>% 
  clean_names(case='snake')

# financial_outcome <- read_csv(here('FSST', 'data', financial_outcome_file)) %>% 
#   clean_names(case = 'snake')
financial_outcome <- read_csv(path('app', 'data', financial_outcome_file)) %>% 
  clean_names(case = 'snake')

## Get the correct file for graph data 
current_file <- list_of_files[str_detect(list_of_files, 'financial_outcomes')]
# boxplot_data <- readr::read_csv(here::here('FSST', 'data', current_file)) #when testing without running the full app
boxplot_data <- readr::read_csv(path('app', 'data', current_file)) #when running the app
# boxplot_data <- read_csv("/Users/constanzebayer/Dropbox (2° Investing)/2° Investing Team/People/Constanze/GitHub/ST-App/FSST/data/financial_outcomes 2021-10-17 .csv")

# INTERIM: Preparation of the repository data

repository_data <- repository_data %>% 
  mutate(matching_indicator = if_else(provider == 'DNB' & str_detect(indicator, 'hicp'),
                                     'inflation/ consumer prices level', 
                                     matching_indicator), 
         matching_indicator = if_else(provider == 'DNB' & str_detect(indicator, 'gdp'),
                                     'GDP growth', 
                                     matching_indicator),
         matching_indicator = if_else(provider == 'BdF' & str_detect(indicator, 'GDP') & geography == 'France', 
                                     'GDP growth', matching_indicator),
         matching_indicator = if_else(provider == 'BoE' & str_detect(indicator, 'UK.real.GDP'), 
                                     'GDP growth', matching_indicator),
         converted_value = if_else(provider == 'BoE' & str_detect(matching_indicator, 'consumer'), 
                                  round(((value / lag(value, 1))-1)*100, 2), converted_value), 
         year = if_else(provider == 'DNB', case_when(
           year_count == 1 ~ 2021,
           year_count == 2 ~ 2022,
           year_count == 3 ~ 2023,
           year_count == 4 ~ 2024,
           year_count == 5 ~ 2025
         ), year), 
         matching_unit = if_else(provider == 'DNB' & str_detect(indicator, 'hicp'), 
                                '%', matching_unit)) %>% 
  group_by(scenario, indicator) %>% 
  mutate(converted_value = if_else(provider == 'BoE' &
                                    (str_detect(matching_indicator, 'consumer') | str_detect(indicator, 'UK.real.GDP')),
                                  round(((value / lag(value, 1))-1)*100, 2), 
                                  converted_value), 
         converted_value = if_else(provider == 'BoE' & is.na(converted_value) & (str_detect(matching_indicator, 'consumer') | str_detect(indicator, 'GDP')),
                                  0, 
                                  converted_value)) 

# Prepare data for building the table (interim) 

replaced_repository_data <- repository_data %>%
  mutate(indicator=str_replace(indicator, '[:space:]year[:space:][:digit:]$', ''))

replaced_repository_data_wide <- replaced_repository_data %>%
  select(-c(matching_indicator, matching_unit)) %>%
  pivot_wider(names_from = c(indicator, value_unit), values_from = converted_value)

column_names <- make_clean_names(names(replaced_repository_data), case='title')
