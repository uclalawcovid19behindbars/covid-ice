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

hist_imm %>%
    select(Name, Residents.Confirmed, Date) %>%
    filter(!is.na(Residents.Confirmed)) %>%
    arrange(Name, Date) %>%
    group_by(Name) %>%
    mutate(New_Deaths = Residents.Confirmed - lag(Residents.Confirmed, default=0)) %>%
    filter(New_Deaths < 0)


Act_df <- hist_imm %>%
    select(Name, Residents.Confirmed, Date) %>%
    filter(!is.na(Residents.Confirmed)) %>%
    arrange(Name, Date) %>%
    group_by(Name) %>%
    mutate(Act = diff_roll_sum(Residents.Confirmed, Date)) %>%
    ungroup() %>%
    mutate(Act = ifelse(Act < 0, 0, Act)) %>%
    group_by(Date) %>%
    summarise(Act = sum(Act, na.rm=T))

Act_df %>%
    mutate(Name = "") %>%
    ggplot(aes(x=Date, y=Act, color = Name)) +
    geom_line(size = 2) +
    ylab("Active COVID\nInfections") +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    theme(legend.position = "none")

# Pearsall 1866
# San Diego 1846
# Laredo == City
# El Paso 1798
