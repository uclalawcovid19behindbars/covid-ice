rm(list=ls())
library(tidyverse)
library(tidycensus)
library(rvest)
library(xml2)
library(lubridate)

ice_pop_df <- "https://raw.githubusercontent.com/vera-institute/" %>%
    str_c(
        "ice-detention-covid/master/data_daily/",
        "national_population_daily.csv") %>%
    read_csv() %>%
    select(Date = page_downloaded_day,Population = population_current) %>%
    summarize(Population = mean(Population))

ice_covid_count <- "https://raw.githubusercontent.com/vera-institute/" %>%
    str_c("ice-detention-covid/master/data_daily/national_cases_daily.csv") %>%
    read_csv() %>%
    filter(page_downloaded_day == max(page_downloaded_day)) %>%
    pull(cases_cumulative)

pop_df <- "https://www2.census.gov/programs-surveys/demo/tables/" %>%
    str_c("age-and-sex/2019/age-sex-composition/2019gender_table1.csv") %>%
    {suppressWarnings(read_csv(., skip = 4, col_types = cols()))} %>%
    .[3:20,] %>%
    select(Age, Male, Female) %>%
    mutate(Male = as.numeric(str_remove_all(Male, ","))) %>%
    mutate(Female = as.numeric(str_remove_all(Female, ","))) %>%
    mutate(Agen = 1:n()) %>%
    mutate(Age = case_when(
        Agen == 1 ~ "0-4",
        Agen == 2 ~ "5-9",
        Agen == 3 ~ "10-14",
        Agen == 4 ~ "15-19",
        Agen == 5 ~ "20-24",
        Agen == 6 ~ "25-29",
        Agen %in% 7:8 ~ "30-39",
        Agen %in% 9:10 ~ "40-49",
        Agen %in% 11:12 ~ "50-59",
        Agen %in% 13:14 ~ "60-69",
        Agen >= 15 ~ "70+",
        TRUE ~ NA_character_)) %>%
    select(-Agen) %>%
    group_by(Age) %>%
    summarize_all(sum) %>%
    pivot_longer(-Age) %>%
    rename(Sex = name, USpop = value) %>%
    arrange(desc(Sex), Age)

cdc_raw_df <- "https://data.cdc.gov/api/views/vsak-wrfu/rows.csv" %>%
    read_csv(col_types = cols()) %>%
    mutate(Date = mdy(`End Week`)) %>%
    select(Date, Age = "Age Group", Deaths = "COVID-19 Deaths", Sex) %>%
    filter(Age != "All Ages" & Sex != "All Sex") %>%
    group_by(Age, Sex) %>%
    summarize(Deaths = sum(Deaths))

young_vec <- c("Under 1 year", "1-4 Years", "5-14 Years", "5-24 Years")

covid_us_df <- cdc_raw_df %>%
    filter(`Age Group` != "All Ages", Sex != "All Sex") %>%
    select(
        Date = `End Week`, Sex, `Age Group`,
        covid_19_deaths = `COVID-19 Deaths`) %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    arrange(Sex, `Age Group`, Date) %>%
    group_by(Sex, `Age Group`) %>%
    mutate(cum_deaths = cumsum(covid_19_deaths)) %>%
    ungroup() %>%
    select(Sex, `Age Group`, cum_deaths, Date) %>%
    mutate(Age = case_when(
        `Age Group` %in% young_vec ~ "<=24",
        `Age Group` %in% c("25-34 Years") ~ "25-34",
        `Age Group` %in% c("35-44 Years") ~ "35-44",
        `Age Group` %in% c("45-54 Years") ~ "45-54",
        `Age Group` %in% c("55-64 Years") ~ "55-64",
        TRUE ~ "65+")) %>%
    group_by(Sex, Age, Date) %>%
    summarize(USCovidDeaths = sum(cum_deaths), .groups = "drop") %>%
    arrange(desc(Sex), Age, Date) %>%
    group_by(Sex, Age) %>%
    mutate(NewUSDeaths = USCovidDeaths - lag(USCovidDeaths, 1, 0)) %>%
    ungroup() %>%
    mutate(Date = Date - 6)

read_html("https://trac.syr.edu/immigration/reports/350/") %>%
    html_nodes("table") %>%
    .[[3]] %>%
    html_table() %>%
    .[,c(1,3)] %>%
    filter(!str_starts(Age, "A")) %>%
    mutate(Number = as.numeric(str_remove_all(Number, ","))) %>%
    mutate(P = Number/sum(Number)) %>%
    mutate(Male = P*.94, Female = P*.06) %>%
    select(Age, Male, Female) %>%
    pivot_longer(-Age, names_to = "Sex", values_to = "P") %>%
    mutate(Population = P * ice_pop_df$Population) %>%
    select(Age, Sex, Population)
