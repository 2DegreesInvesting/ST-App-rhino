library(readr)
library(readxl)
library(dplyr)


old <- read_csv("app/data/app_inputs_2021-09-24.csv")

new <- read_excel("scripts/Canadascenarioaddition.xlsx")



new  <- new %>%
    rename(
        provider=Provider,
        scenario=Scenario,
        year=Year,
        `year count`= `Year Count`,
        geography=Geography,
        subset=Subset,
        `matching unit` = `Matching Unit`,
        `matching indicator` = `Matching Indicator`,
        `converted_value` = `Converted Value` ,    
    )

bind_rows(new, old) %>% write_csv("app/data/app_inputs_2021-09-24.csv")
