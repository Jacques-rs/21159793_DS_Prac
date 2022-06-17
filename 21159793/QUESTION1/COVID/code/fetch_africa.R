

fetch_africa <- function(df){


    df <- df %>% .[.[,"continent"] == "Africa",] %>%
        .[rowSums(is.na(.)) != ncol(.),] %>%
        mutate(date = as.Date(date)) %>% group_by(date) %>%
        select(-iso_code) %>%
        select(total_cases, total_deaths, icu_patients,
               hosp_patients, total_tests, total_vaccinations,
               stringency_index, population_density,
               excess_mortality, reproduction_rate) %>%
        summarise(across(c(total_cases,
                           total_deaths,
                           icu_patients,
                           hosp_patients,
                           total_tests,
                           total_vaccinations,
                           stringency_index, population_density,
                           excess_mortality, reproduction_rate),
                         list(mean = mean, sum = sum))) %>%
        select(date, total_cases_sum, total_deaths_sum, icu_patients_sum,
               hosp_patients_sum, hosp_patients_sum, total_tests_sum,
               total_vaccinations_sum, stringency_index_mean, population_density_mean,
               excess_mortality_mean, reproduction_rate_mean) %>%
        ungroup()








    return(df)
}











