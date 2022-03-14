rm(list = ls())
library(googlesheets4)
library(tidyverse)
library()
gs4_deauth()
gs4_user()
flight_paths_url <- "1mmcB4pl8-RS0KczOSUz2HdpkqBo4AJQHjp-f4PgcvDk"

covid_df <- "https://raw.githubusercontent.com/vera-institute/ice-detention" %>%
    str_c("-covid/master/data_daily/national_cases_daily.csv") %>%
    read_csv(col_types = cols()) %>%
    select(
        Date = ice_date_updated,
        Cases = cases_cumulative,
        Active = cases_current) %>%
    mutate(Date = as_date(Date)) %>%
    mutate(Active = Active/ max(Active, na.rm = TRUE)*140) %>%
    mutate(Name = "Active\nCases")

as_date(covid_df$Date)

fix_dates <- function(D){
    unlist(lapply(D, function(x){
        if(is.null(x)){
            return(NA_character_)
        }else{
            return(as.character(x))
        }})) %>%
        str_remove("Tuesday, ") %>%
        parse_date_time(orders = c("ymd", "mdy")) %>%
        as_date()
}

fix_numeric <- function(X){
    unlist(lapply(X, function(x){
        if(is.null(x)){
            return(NA_real_)
        }else{
            return(as.numeric(unlist(x)[1]))
        }}))
}

flights_df <- read_sheet(flight_paths_url, skip = 2) %>%
    mutate(Date = fix_dates(Date)) %>%
    filter(!is.na(Date))

flights_df %>%
    mutate(`From Plane - Boarded Bus to NWDC...10` = fix_numeric(
        `From Plane - Boarded Bus to NWDC...10`
    )) %>%
    group_by(Date) %>%
    summarise(Arrivals = sum(
        `From Plane - Boarded Bus to NWDC...10`, na.rm = T)) %>%
    ggplot(aes(x=Date, y=Arrivals)) +
    geom_col() +
    geom_line(aes(x=Date, y = Active, color = Name), data = covid_df) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(color = "") +
    ggtitle(
        "Active Cases & New Arrivals",
        str_c(
            "A Comparison of Outbreaks in ICE detention\ncenters ",
            "& new arrivals from Tacoma Airport"))

flights_df %>%
    mutate(`From Plane - Boarded Bus to NWDC...10` = fix_numeric(
        `From Plane - Boarded Bus to NWDC...10`
    )) %>%
    group_by(Date) %>%
    summarise(Arrivals = sum(
        `From Plane - Boarded Bus to NWDC...10`, na.rm = T)) %>%
    ggplot(aes(x=Date, y=Arrivals)) +
    geom_col() +
    #geom_line(aes(x=Date, y = Active, color = Name), data = covid_df) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(color = "") +
    ggtitle("New Arrivals from Tacoma Airport")

flights_df %>%
    mutate(`From Plane - Boarded Bus to NWDC...10` = fix_numeric(
        `From Plane - Boarded Bus to NWDC...10`
    )) %>%
    ggplot(aes(x=Date, y=`From Plane - Boarded Bus to NWDC...10`)) +
    geom_col() +
    facet_wrap(~`Previous Airport`)


