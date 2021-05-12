rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(tigris)
library(sf)
library(lubridate)
library(plotly)

county_source <- "https://raw.githubusercontent.com/nytimes/" %>%
    paste0("covid-19-data/master/us-counties.csv")

county_df <- readr::read_csv(county_source, col_types = readr::cols()) %>%
    rename(Date = "date", County = "county", State = "state",
           FIPS = "fips", General.Confirmed = "cases",
           General.Deaths = "deaths")

covid_df <- filter(county_df, FIPS == 48163) %>%
    mutate(Population = 20306) %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ Population)

hist_df <- read_scrape_data(TRUE)

hist_imm <- hist_df %>%
    filter(Jurisdiction == "immigration")

pearsall_df <- hist_imm %>%
    filter(Facility.ID == 1866) %>%
    arrange(Date) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

pearsall_plot <- pearsall_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(covid_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE South Texas\nImmigration Detainees",
        "Frio County"
        )) %>%
    mutate(Group = factor(
        Group,c("ICE South Texas\nImmigration Detainees", "Frio County"))) %>%
    filter(Date > ymd("2020-05-23")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")


sd_df <- hist_imm %>%
    filter(Facility.ID == 1846) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    #mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    #mutate(Residents.Active = ifelse(Resid))
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

sd_gen_df <- get_genpop_covid("San Diego", "CA") %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ General.Population2018)

sd_plot <- sd_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(sd_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE Otay Mesa\nImmigration Detainees",
        "San Diego County"
    )) %>%
    mutate(Group = factor(
        Group,c("ICE Otay Mesa\nImmigration Detainees", "San Diego County"))) %>%
    filter(Date > ymd("2020-05-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")

ep_df <- hist_imm %>%
    filter(Facility.ID == 1798) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    #mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    #mutate(Residents.Active = ifelse(Resid))
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

ep_gen_df <- get_genpop_covid("El Paso", "TX") %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ General.Population2018)

ep_plot <- ep_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(ep_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE El Paso Processing\nImmigration Detainees",
        "El Paso County"
    )) %>%
    mutate(Group = factor(
        Group,c("ICE El Paso Processing\nImmigration Detainees", "El Paso County"))) %>%
    filter(Date > ymd("2020-05-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")

wb_df <- hist_imm %>%
    filter(Facility.ID == 1856) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Residents.Active = ifelse(Residents.Active <0, 0, Residents.Active)) %>%
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

wb_gen_df <- get_genpop_covid("Webb", "TX") %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ General.Population2018)

wb_plot <- wb_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(wb_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE Rio Grande\nImmigration Detainees",
        "Webb County"
    )) %>%
    mutate(Group = factor(
        Group,c("ICE Rio Grande\nImmigration Detainees", "Webb County"))) %>%
    filter(Date > ymd("2020-09-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")

st_df <- hist_imm %>%
    filter(Facility.ID == 1867) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Residents.Active = ifelse(Residents.Active <0, 0, Residents.Active)) %>%
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

st_gen_df <- filter(county_df, FIPS == 13259) %>%
    mutate(Population = 6621) %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ Population)

st_plot <- st_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(st_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE Stewart Detention\nImmigration Detainees",
        "Stewart County"
    )) %>%
    mutate(Group = factor(
        Group,
        c("ICE Stewart Detention\nImmigration Detainees", "Stewart County"))) %>%
    filter(Date > ymd("2020-04-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")

sb_df <- hist_imm %>%
    filter(Facility.ID == 1773) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Residents.Active = ifelse(Residents.Active <0, 0, Residents.Active)) %>%
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

sb_gen_df <- get_genpop_covid("San Bernardino", "CA") %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ General.Population2018)

sb_plot <- sb_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(sb_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "Adelanto ICE Processing\nImmigration Detainees",
        "San Beranrdino County"
    )) %>%
    filter(Date > ymd("2020-04-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")

## LA PALMA

lp_df <- hist_imm %>%
    filter(Facility.ID == 1825) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Residents.Active = ifelse(Residents.Active <0, 0, Residents.Active)) %>%
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

lp_gen_df <- get_genpop_covid(lp_df$County[1], lp_df$State[1]) %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ General.Population2018)

lp_plot <- lp_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(lp_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE La Palma\nImmigration Detainees",
        "Pinal County"
    )) %>%
    # mutate(Group = factor(
    #     Group,
    #     c("ICE Stewart Detention\nImmigration Detainees", "Stewart County"))) %>%
    filter(Date > ymd("2020-04-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")


## ELOY

el_df <- hist_imm %>%
    filter(Facility.ID == 1801) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Residents.Active = ifelse(Residents.Active <0, 0, Residents.Active)) %>%
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

el_gen_df <- get_genpop_covid(el_df$County[1], el_df$State[1]) %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ General.Population2018)

el_plot <- el_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(el_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE Eloy\nImmigration Detainees",
        "Pinal County"
    )) %>%
    # mutate(Group = factor(
    #     Group,
    #     c("ICE Stewart Detention\nImmigration Detainees", "Stewart County"))) %>%
    filter(Date > ymd("2020-04-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")

## Winn

wn_df <- hist_imm %>%
    filter(Facility.ID == 1874) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Residents.Active = ifelse(Residents.Active <0, 0, Residents.Active)) %>%
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

wn_gen_df <- filter(county_df, FIPS == 22127) %>%
    mutate(Population = 14313) %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ Population)

wn_plot <- wn_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(wn_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE Winn Correctional\nImmigration Detainees",
        "Winn Parish"
    )) %>%
    # mutate(Group = factor(
    #     Group,
    #     c("ICE Stewart Detention\nImmigration Detainees", "Stewart County"))) %>%
    filter(Date > ymd("2020-04-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")

## broward

br_df <- hist_imm %>%
    filter(Facility.ID == 1781) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Residents.Active = ifelse(Residents.Active <0, 0, Residents.Active)) %>%
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

br_gen_df <- get_genpop_covid(br_df$County[1], br_df$State[1]) %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ General.Population2018)

br_plot <- br_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(br_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE Broward\nImmigration Detainees",
        "Broward County"
    )) %>%
    mutate(Group = factor(
        Group,
        c("ICE Broward\nImmigration Detainees", "Broward County"))) %>%
    filter(Date > ymd("2020-04-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")

## otero

ot_df <- hist_imm %>%
    filter(Facility.ID == 1847) %>%
    arrange(Date) %>%
    filter(!(Date %in% ymd(c("2020-05-05", "2020-05-06")))) %>%
    mutate(Residents.Active = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(Residents.Active = ifelse(Residents.Active <0, 0, Residents.Active)) %>%
    mutate(Residents.Active.P = Residents.Active / Population.Feb20)

ot_gen_df <- get_genpop_covid(ot_df$County[1], ot_df$State[1]) %>%
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>%
    mutate(General.Active.P = General.Active/ General.Population2018)

ot_plot <- ot_df %>%
    select(Date, Residents.Active.P) %>%
    left_join(
        select(ot_gen_df, Date, General.Active.P),
        by = "Date"
    ) %>%
    mutate(Residents.Active.P = ifelse(
        is.na(Residents.Active.P), 0, Residents.Active.P)) %>%
    pivot_longer(-Date) %>%
    mutate(Group = ifelse(
        str_detect(name, "Res"),
        "ICE Otero\nImmigration Detainees",
        "Otero County"
    )) %>%
    filter(Date > ymd("2020-04-01")) %>%
    ggplot(aes(x=Date, y=value, color=Group)) +
    geom_line(size=1.5, alpha=.8) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(
        breaks= scales::pretty_breaks(),
        labels = scales::percent_format(accuracy = 1L)) +
    labs(y="Estimated Percent\n Population Infected", color = "")

## save files

sb_full_df <- bind_rows(
    sb_gen_df %>%
        select(
            Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "San Bernardino County") %>%
        filter(!is.na(Percent.Active)),

    sb_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "Adelanto ICE Processing") %>%
        filter(!is.na(Percent.Active)))

write_csv(sb_full_df, "results/sanbernardino.csv")

frio_full_df <- bind_rows(
    covid_df %>%
        select(Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "Frio County") %>%
        filter(!is.na(Percent.Active)),

    pearsall_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "ICE South Texas Immigration") %>%
        mutate(Percent.Active = ifelse(is.na(Percent.Active), 0, Percent.Active)))

write_csv(frio_full_df, "results/frio.csv")

ep_full_df <- bind_rows(
    ep_gen_df %>%
        select(
            Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "El Paso County") %>%
        filter(!is.na(Percent.Active)),

    ep_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "El Paso ICE Processing") %>%
        mutate(Percent.Active = ifelse(is.na(Percent.Active), 0, Percent.Active)))

write_csv(ep_full_df, "results/elpaso.csv")

st_full_df <- bind_rows(
    st_gen_df %>%
        select(
            Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "Stewart County") %>%
        filter(!is.na(Percent.Active)),

    st_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "ICE Stewart Detention") %>%
        mutate(Percent.Active = ifelse(is.na(Percent.Active), 0, Percent.Active)))

write_csv(st_full_df, "results/stewart.csv")

lp_full_df <- bind_rows(
    lp_gen_df %>%
        select(
            Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "Pinal County") %>%
        filter(!is.na(Percent.Active)),

    lp_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "ICE La Palma") %>%
        filter(!is.na(Percent.Active)))

ggsave("results/lapalma.png", lp_plot, width = 12, height = 8)
write_csv(lp_full_df, "results/lapalma.csv")

sd_full_df <- bind_rows(
    sd_gen_df %>%
        select(
            Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "San Diego County") %>%
        filter(!is.na(Percent.Active)),

    sd_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "Otay Mesa ICE") %>%
        filter(!is.na(Percent.Active)))

ggsave("results/otaymesa.png", sd_plot, width = 12, height = 8)
write_csv(sd_full_df, "results/otaymesa.csv")

el_full_df <- bind_rows(
    el_gen_df %>%
        select(
            Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "Pinal County") %>%
        filter(!is.na(Percent.Active)),

    el_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "ICE Eloy") %>%
        filter(!is.na(Percent.Active)))

ggsave("results/eloy.png", el_plot, width = 12, height = 8)
write_csv(el_full_df, "results/eloy.csv")

wn_full_df <- bind_rows(
    wn_gen_df %>%
        select(
            Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "Winn Parish") %>%
        filter(!is.na(Percent.Active)),

    wn_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "ICE Winn") %>%
        filter(!is.na(Percent.Active)))

ggsave("results/winn.png", wn_plot, width = 12, height = 8)
write_csv(wn_full_df, "results/winn.csv")

br_full_df <- bind_rows(
    br_gen_df %>%
        select(
            Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "Broward County") %>%
        filter(!is.na(Percent.Active)),

    br_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "ICE Broward") %>%
        filter(!is.na(Percent.Active)))

ggsave("results/broward.png", br_plot, width = 12, height = 8)
write_csv(br_full_df, "results/broward.csv")

ot_full_df <- bind_rows(
    ot_gen_df %>%
        select(
            Date, Percent.Active = General.Active.P) %>%
        mutate(Name = "Otero County") %>%
        filter(!is.na(Percent.Active)),

    ot_df %>%
        select(
            Date, Percent.Active = Residents.Active.P) %>%
        mutate(Name = "ICE Otero") %>%
        filter(!is.na(Percent.Active)))

ggsave("results/otero.png", ot_plot, width = 12, height = 8)
write_csv(ot_full_df, "results/otero.csv")
