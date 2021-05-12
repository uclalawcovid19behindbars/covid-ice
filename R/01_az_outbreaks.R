rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(tigris)
library(sf)
library(lubridate)
library(plotly)

hist_df <- read_scrape_data(TRUE)

hist_imm <- hist_df %>%
    filter(Jurisdiction == "immigration")

hist_az_df <- hist_df %>%
    filter(State == "Arizona" & Jurisdiction != "county")

test <- hist_az_df %>%
    filter(Date >= ymd("2020-11-15") & !is.na(Facility.ID)) %>%
    arrange(Name, Date) %>%
    group_by(Name) %>%
    mutate(Res.Act.Est = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Res.Act.Est = ifelse(Res.Act.Est < 0, 0, Res.Act.Est)) %>%
    filter(Date >= ymd("2021-01-01")) %>%
    filter(!is.na(Population.Feb20)) %>%
    select(Date, Res.Act.Est, Name, Population.Feb20) %>%
    mutate(Percent.Active = Res.Act.Est/Population.Feb20) %>%
    filter(any(Percent.Active > .055, na.rm = TRUE)) %>%
    ggplot(aes(x=Date, y=Percent.Active, color = Name)) +
    geom_line()


hist_az_df %>%
    filter(Date >= ymd("2020-11-15") & !is.na(Facility.ID)) %>%
    arrange(Name, Date) %>%
    group_by(Name) %>%
    mutate(Res.Act.Est = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Res.Act.Est = ifelse(Res.Act.Est < 0, 0, Res.Act.Est)) %>%
    #filter(Date >= ymd("2021-01-01")) %>%
    filter(Name %in%
               c("ICE CCA FLORENCE CORRECTIONAL CENTER", "DOUGLAS STATE PRISON")) %>%
    filter(!is.na(Population.Feb20)) %>%
    select(Date, Res.Act.Est, Name, Population.Feb20) %>%
    mutate(Percent.Active = Res.Act.Est/Population.Feb20) %>%
    filter(any(Percent.Active > .055, na.rm = TRUE)) %>%
    ggplot(aes(x=Date, y=Percent.Active, color = Name)) +
    geom_line()

ggplotly(test)
