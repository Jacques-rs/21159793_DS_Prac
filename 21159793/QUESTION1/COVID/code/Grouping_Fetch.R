


grouping_fetch <- function(df){


    df <- covid_df %>% select(date, location, ends_with("smokers"), extreme_poverty,
                              human_development_index, handwashing_facilities,
                              aged_65_older, median_age, total_cases_per_million,
                              total_deaths_per_million, reproduction_rate,
                              icu_patients_per_million, hosp_patients_per_million) %>%
        group_by(date, location) %>%
        replace_na(list(female_smokers = 0, male_smokers = 0)) %>%
        mutate(smokers = mean(female_smokers, male_smokers, na.rm = F)) %>%
        select(-c(female_smokers, male_smokers))

    return(df)
}










