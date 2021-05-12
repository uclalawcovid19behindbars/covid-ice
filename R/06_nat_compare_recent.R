rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(tigris)
library(sf)
library(lubridate)
library(plotly)

gen_df <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/" %>%
    str_c("us.csv") %>%
    read_csv(col_types = cols()) %>%
    select(Date = date, Cases = cases, Deaths = deaths) %>%
    arrange(Date) %>%
    # not necessary but a good sanity check
    distinct(Date, .keep_all = TRUE) %>%
    mutate(New_Cases = diff_roll_sum(Cases, Date)) %>%
    mutate(Population = 328200000) %>%
    mutate(Name = "United States\nPopulation")

pop_df <- "https://raw.githubusercontent.com/vera-institute/ice-" %>%
    str_c("detention-covid/master/data_daily/national_population_daily.csv") %>%
    read_csv(col_types = cols()) %>%
    select(Date = ice_date_as_of_pop, Population = population_current) %>%
    distinct(Date, .keep_all = TRUE) %>%
    full_join(
        "https://raw.githubusercontent.com/vera-institute/ice-detention" %>%
            str_c("-covid/master/data_daily/national_cases_daily.csv") %>%
            read_csv(col_types = cols()) %>%
            select(
                Date = ice_date_updated,
                Cases = cases_cumulative,
                Active = cases_current) %>%
            mutate(Date = as_date(Date)) %>%
            group_by(Date) %>%
            summarize(
                Cases = max(Cases),
                Active = max(Active),
                .groups = "drop"
            ),
        by = "Date"
    ) %>%
    full_join(
        "https://raw.githubusercontent.com/vera-institute/ice-detention-" %>%
            str_c("covid/master/data_daily/national_tests_daily.csv") %>%
            read_csv(col_types = cols()) %>%
            select(Date = ice_date_as_of_test, Tests = tests_cumulative) %>%
            mutate(Date = as_date(Date)) %>%
            # bad data here
            filter(Date != ymd("2020-12-04")) %>%
            group_by(Date) %>%
            filter(Tests == max(Tests)) %>%
            filter(1:n() == 1) %>%
            ungroup() %>%
            arrange(Date) %>%
            mutate(New_Tests = diff_roll_sum(Tests, Date)),
        by = "Date"
    ) %>%
    arrange(Date) %>%
    mutate(New_Cases = diff_roll_sum(Cases, Date)) %>%
    mutate(TPR = New_Cases/New_Tests) %>%
    mutate(Name = "ICE Detainee\nPopulation")

pop_df %>%
    filter(Date >= "2020-10-16") %>%
    filter(!is.na(TPR)) %>%
    mutate(name = "") %>%
    ggplot(aes(x = Date, y=TPR, color = name)) +
    geom_line(size=1.25) +
    geom_point(size=2.5) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(labels=scales::percent, limits = c(0,NA)) +
    labs(y="Test Positivity Percent") +
    theme(legend.position = "none")

pop_df %>%
    filter(Date >= "2020-10-16") %>%
    mutate(AR = Active / Population) %>%
    filter(!is.na(AR)) %>%
    mutate(name = "") %>%
    ggplot(aes(x = Date, y=AR, color = name)) +
    geom_line(size=1.25) +
    geom_point(size=2.5) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(labels=scales::percent, limits = c(0,NA)) +
    labs(y="Percent Actively Infected") +
    theme(legend.position = "none")

pop_df %>%
    filter(!is.na(New_Tests)) %>%
    mutate(name = "") %>%
    filter(Date >= "2020-10-16") %>%
    ggplot(aes(x = Date, y=New_Tests, color = name)) +
    geom_line(size=1.25) +
    geom_point(size=2.5) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(y="New Tests in Last 14 Weeks") +
    theme(legend.position = "none") +
    ylim(c(0,NA))

comp_plot <- pop_df %>%
    filter(!is.na(Population) & !is.na(New_Cases)) %>%
    bind_rows(gen_df) %>%
    mutate(NCR = New_Cases / Population * 10000) %>%
    filter(Date >= "2020-02-16") %>%
    ggplot(aes(x = Date, y=NCR, color = Name)) +
    geom_line(size=2) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(y="New Cases Per\n10,000 People", color = "") +
    scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
    ylim(c(0,NA))

ggplotly(comp_plot)


NCR_df <- pop_df %>%
    filter(!is.na(Population) & !is.na(New_Cases)) %>%
    bind_rows(gen_df) %>%
    mutate(NCR = New_Cases / Population * 10000)

pop_df %>%
    filter(!is.na(Population) & !is.na(New_Cases)) %>%
    bind_rows(gen_df) %>%
    mutate(NCR = New_Cases / Population * 10000) %>%
    filter(Date == "2021-04-16") %>%
    pull(NCR) %>%
    {first(.)/last(.)}

`/`(
    NCR_df %>%
        filter(Name == "ICE Detainee\nPopulation") %>%
        filter(Date == "2021-04-16") %>%
        pull(NCR),

    NCR_df %>%
        filter(Name != "ICE Detainee\nPopulation") %>%
        pull(NCR) %>%
        max(na.rm = TRUE))


`/`(
    NCR_df %>%
        filter(Name != "ICE Detainee\nPopulation") %>%
        filter(Date == "2021-04-16") %>%
        pull(NCR),

    NCR_df %>%
        filter(Name != "ICE Detainee\nPopulation") %>%
        pull(NCR) %>%
        max(na.rm = TRUE))
