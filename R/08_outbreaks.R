rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(lubridate)

hist_df <- read_scrape_data(TRUE)

hist_imm <- hist_df %>%
    filter(Jurisdiction == "immigration")

focus <- c(
    "La Palma Correctional" = "ICE LA PALMA CORRECTIONAL CENTER",
    "Richwood Correctional" = "ICE RICHWOOD CORRECTIONAL CENTER",
    "Adams Detention" = "ICE ADAMS COUNTY DETENTION CENTER")

end_vals <- hist_imm %>%
    filter(Name %in% c(focus)) %>%
    group_by(Name) %>%
    filter(Date == max(Date)) %>%
    pull(Residents.Active)

ob_plot <- hist_imm %>%
    filter(Name %in% c(focus)) %>%
    filter(Date >= ymd("2020-12-01")) %>%
    mutate(Name = names(focus)[sapply(Name, function(x) which(x == focus))]) %>%
    mutate(Name = str_c(Name, "\n", City, ", ", State)) %>%
    ggplot(aes(x = Date, y = Residents.Active, color = Name)) +
    geom_line(size = 2, alpha = .85) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = end_vals)) +
    labs(x = "Date", y = "Detainees With Active Infections", color = "") +
    theme_behindbars() +
    scale_color_bbdiscrete()

ggsave("~/Downloads/ice_outbreaks.svg", ob_plot, width = 14, height = 8)
