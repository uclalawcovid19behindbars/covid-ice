rm(list = ls())
library(googlesheets4)
library(tidyverse)
library(lubridate)
# install using
# remotes::install_github("uclalawcovid19behindbars/behindbarstools")
library(behindbarstools)
gs4_deauth()
gs4_user()
# url of the tacoma flight data
flight_paths_url <- "1mmcB4pl8-RS0KczOSUz2HdpkqBo4AJQHjp-f4PgcvDk"

# vera institute ice wide data
covid_df <- "https://raw.githubusercontent.com/vera-institute/ice-detention" %>%
    str_c("-covid/master/data_daily/national_cases_daily.csv") %>%
    read_csv(col_types = cols()) %>%
    select(
        Date = ice_date_updated,
        Cases = cases_cumulative,
        Active = cases_current) %>%
    mutate(Date = as_date(Date)) %>%
    # scale factor to match scale of arrivals
    mutate(Active = Active/ max(Active, na.rm = TRUE)*140) %>%
    mutate(Name = "Active\nCases")

# ucla covid behind bars facility data
hist_imm <- read_scrape_data(TRUE) %>%
    filter(Jurisdiction == "immigration")

# only look at tacoma nwdc data data
tacoma_df <- hist_imm %>%
    # this is the ID for NWDC
    filter(Facility.ID == 1844) %>%
    mutate(ColName="Active\nCases")

# function for fixing strange dates in google sheet
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

# function for fixing weird number issues in google sheet
fix_numeric <- function(X){
    unlist(lapply(X, function(x){
        if(is.ntull(x)){
            return(NA_real_)
        }else{
            return(as.numeric(unlist(x)[1]))
        }}))
}

# read the flights data from the google sheet
flights_df <- read_sheet(flight_paths_url, skip = 2) %>%
    mutate(Date = fix_dates(Date)) %>%
    filter(!is.na(Date))

# plot flight data arrivals against scaled all ice active cases
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
    xlim(ymd(c("2021-01-01", NA))) +
    ggtitle(
        "Active Cases & New Arrivals",
        str_c(
            "A Comparison of Outbreaks in ICE detention\ncenters ",
            "& new arrivals from Tacoma Airport"))

# plot flight data arrivals against cases in specifically NWDC
flights_df %>%
    mutate(`From Plane - Boarded Bus to NWDC...10` = fix_numeric(
        `From Plane - Boarded Bus to NWDC...10`
    )) %>%
    group_by(Date) %>%
    summarise(Arrivals = sum(
        `From Plane - Boarded Bus to NWDC...10`, na.rm = T)) %>%
    ggplot(aes(x=Date, y=Arrivals)) +
    geom_col() +
    geom_line(
        aes(x=Date, y = Residents.Active, color = ColName), data = tacoma_df) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(color = "") +
    xlim(ymd(c("2021-01-01", NA))) +
    ggtitle(
        "Active Cases & New Arrivals",
        str_c(
            "A Comparison of Outbreaks in NWDC\n ",
            "& new arrivals from Tacoma Airport"))


