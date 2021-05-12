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
    select(Date = date, Confirmed = cases, Deaths = deaths) %>%
    arrange(Date) %>%
    # not necessary but a good sanity check
    distinct(Date, .keep_all = TRUE) %>%
    mutate(Active = diff_roll_sum(Confirmed, Date)) %>%
    mutate(Population = 328200000) %>%
    mutate(Name = "United States\nPopulation")

last_highest <- function(x){
    z <- x
    for(i in 2:length(x)){
        if(z[i] < z[i-1]){
            z[i] <- max(z[1:i], na.rm = TRUE)
        }
    }
    z
}

pop_df <- "https://raw.githubusercontent.com/vera-institute/ice-" %>%
    str_c("detention-covid/master/data_daily/national_population_daily.csv") %>%
    read_csv() %>%
    select(Date = page_downloaded_day, Population = population_current) %>%
    arrange(Date) %>%
    distinct(Date, .keep_all = TRUE)

pop_df %>%
    ggplot(aes(x=Date, y = Population)) +
    geom_line()

hist_df <- read_scrape_data(TRUE)

hist_imm <- hist_df %>%
    filter(Jurisdiction == "immigration")

hist_imm %>%
    select(Date, Residents.Confirmed) %>%
    group_by(Date) %>%
    summarise_all(sum_na_rm) %>%
    mutate(Residents.Confirmed = last_highest(Residents.Confirmed)) %>%
    ggplot(aes(x = Date, y = Residents.Confirmed)) +
    geom_line(size = 2, color = "#D7790F") +
    geom_area(fill = "#D7790F", alpha = .5) +
    theme_behindbars() +
    ylab("Cumulative Cases")

hist_imm %>%
    select(Date, Residents.Confirmed) %>%
    group_by(Date) %>%
    summarise_all(sum_na_rm) %>%
    mutate(Residents.Confirmed = last_highest(Residents.Confirmed)) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    ggplot(aes(x = Date, y = Residents.Active)) +
    geom_line(size = 2, color = "#D7790F") +
    geom_area(fill = "#D7790F", alpha = .5) +
    theme_behindbars() +
    ylab("Active Cases")

hist_imm %>%
    select(Date, Residents.Confirmed) %>%
    group_by(Date) %>%
    summarise_all(sum_na_rm) %>%
    mutate(Confirmed = last_highest(Residents.Confirmed)) %>%
    mutate(Active = diff_roll_sum(Confirmed, Date)) %>%
    left_join(pop_df, by = "Date") %>%
    mutate(Active.P = Active / Population) %>%
    ggplot(aes(x = Date, y = Active.P)) +
    geom_line(size = 2, color = "#D7790F") +
    geom_area(fill = "#D7790F", alpha = .5) +
    theme_behindbars() +
    ylab("Active Cases")

hist_imm %>%
    select(Date, Residents.Confirmed) %>%
    group_by(Date) %>%
    summarise_all(sum_na_rm) %>%
    mutate(Confirmed = last_highest(Residents.Confirmed)) %>%
    mutate(Active = diff_roll_sum(Confirmed, Date)) %>%
    mutate(Name = "ICE Detainee\nPopulation") %>%
    left_join(pop_df, by = "Date") %>%
    bind_rows(gen_df) %>%
    mutate(Active.P = Active / Population * 10000) %>%
    ggplot(aes(x = Date, y = Active.P, color = Name)) +
    geom_line(size = 2) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(y ="Active Case Rate\nPer 10,000", color = "")

hist_imm %>%
    select(Date, Residents.Confirmed) %>%
    group_by(Date) %>%
    summarise_all(sum_na_rm) %>%
    mutate(Confirmed = last_highest(Residents.Confirmed)) %>%
    mutate(Active = diff_roll_sum(Confirmed, Date)) %>%
    mutate(Name = "ICE Detainee\nPopulation") %>%
    left_join(pop_df, by = "Date") %>%
    bind_rows(gen_df) %>%
    mutate(Active.P = Active / Population * 10000) %>%
    select(Date, Name, Active.P) %>%
    pivot_wider(names_from = "Name", values_from = "Active.P") %>%
    mutate(Ratio = `ICE Detainee\nPopulation`/`United States\nPopulation`) %>%
    filter(!is.na(Ratio)) %>%
    ggplot(aes(x = Date, y = Ratio)) +
    geom_line(size = 2, color = "#D7790F") +
    theme_behindbars() +
    labs(y="Risk Ratio") +
    ggtitle("ICE Detainee Excess COVID Infection Risk")


hist_imm %>%
    filter(State != "Arizona") %>%
    plot_recent_fac_increases(plot_days = 20, metric = "Residents.Active") +
    labs(color="", y = "Active COVID-19 Infections")

