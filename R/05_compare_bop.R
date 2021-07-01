rm(list = ls())
library(tidyverse)
library(behindbarstools)
library(lubridate)

last_highest <- function(x){
    z <- x
    for(i in 2:length(x)){
        if(z[i] < z[i-1]){
            z[i] <- max(z[1:i], na.rm = TRUE)
        }
    }
    z
}

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

vera_df <- "https://raw.githubusercontent.com/vera-institute/ice-" %>%
    str_c("detention-covid/master/data_daily/national_population_daily.csv") %>%
    read_csv(col_types = cols()) %>%
    select(
        Date = page_downloaded_day,
        Population = population_current) %>%
    arrange(Date) %>%
    distinct(Date, .keep_all = TRUE) %>%
    left_join(
        "https://raw.githubusercontent.com/vera-institute/ice-detention-" %>%
            str_c("covid/master/data_daily/national_cases_daily.csv") %>%
            read_csv(col_types = cols()) %>%
            select(
                Cases = cases_cumulative,
                Date = page_downloaded_day,
                Active = cases_current) %>%
            arrange(Date) %>%
            distinct(Date, .keep_all = TRUE),
        by = "Date"
    )

vera_df %>%
    select(-Active) %>%
    na.omit() %>%
    mutate(cum_rate = Cases/(cumsum(Population)/ 1:n())) %>%
    tail()

gen_df %>%
    tail() %>%
    mutate(Confirmed / Population)

fed_json_files <- list_remote_data("raw_files", scraper_name = "federal")

all_covid_df <- read_scrape_data(all_dates = TRUE)

hist_imm <- all_covid_df %>%
    filter(Jurisdiction == "immigration")

# get population data from the json files directly
fed_pop_df <- bind_rows(lapply(1:length(fed_json_files), function(i){

    fed_json <- jsonlite::read_json(fed_json_files[i], simplifyVector = TRUE)

    c(
        fed_json$final$other$federalInmatePopulationTotal,
        fed_json$final$other$ccmFacilityPopulationTotal,
        fed_json$final$other$privateFacilityPopulationTotal) %>%
        str_remove_all(",") %>%
        as.numeric() %>%
        sum() %>%
        {tibble(
            Date = ymd(str_extract(fed_json_files[i], "\\d{4}-\\d{2}-\\d{2}")),
            Population = .
        )}})) %>%
    mutate(Population = as.numeric(str_remove_all(Population, ",")))

comp_plot <- bind_rows(
    all_covid_df %>%
        filter(Jurisdiction == "federal") %>%
        group_by(Date) %>%
        summarize(Active = sum_na_rm(Residents.Active)) %>%
        filter(!is.na(Active)) %>%
        left_join(fed_pop_df) %>%
        mutate(Name = "BOP\nPopulation"),

    vera_df %>%
        mutate(Name = "ICE\nPopulation"),

    gen_df) %>%
    mutate(Active.P = Active / Population * 10000) %>%
    filter(Date >= ymd("2020-10-15")) %>%
    ggplot(aes(x = Date, y = Active.P)) +
    geom_line(aes(color = Name), size = 2) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(color = "", y = "Active COVID Infections\nPer 10,000 Individuals")

ggsave("ice_bop.svg", comp_plot, width = 10.7, height = 6.7)
ggsave("ice_bop.png", comp_plot, width = 10.7, height = 6.7)


gen_plot <- bind_rows(
    vera_df %>%
        mutate(Name = "ICE\nPopulation"),

    gen_df) %>%
    mutate(Active.P = Active / Population * 10000) %>%
    filter(Date >= ymd("2020-10-15")) %>%
    ggplot(aes(x = Date, y = Active.P)) +
    geom_line(aes(color = Name), size = 2) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(color = "", y = "Active COVID Infections\nPer 10,000 Individuals")

ggsave("ice_gen.svg", gen_plot, width = 10.7, height = 6.7)
ggsave("ice_gen.png", gen_plot, width = 10.7, height = 6.7)

bind_rows(
    all_covid_df %>%
        filter(Jurisdiction == "federal") %>%
        group_by(Date) %>%
        summarize(Residents.Active = sum_na_rm(Residents.Active)) %>%
        filter(!is.na(Residents.Active)) %>%
        left_join(fed_pop_df) %>%
        rename(Residents.Population = Population) %>%
        mutate(Name = "BOP"),

    hist_imm %>%
        select(Date, Residents.Confirmed) %>%
        group_by(Date) %>%
        summarise_all(sum_na_rm) %>%
        mutate(Confirmed = last_highest(Residents.Confirmed)) %>%
        mutate(Residents.Active = diff_roll_sum(Confirmed, Date)) %>%
        select(Date, Residents.Active) %>%
        left_join(pop_df) %>%
        mutate(Name = "ICE")) %>%
    mutate(Active.P = Residents.Active / Residents.Population * 10000) %>%
    ggplot(aes(x = Date, y = Active.P)) +
    geom_line(aes(color = Name), size = 2) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    geom_vline(xintercept = ymd("2020-12-22"), linetype = 3, size = 2) +
    labs(color = "", y = "Active COVID Infections\nPer 10,000 Detainees")

bind_rows(
    all_covid_df %>%
        filter(Jurisdiction == "federal") %>%
        group_by(Date) %>%
        summarize(Residents.Active = sum_na_rm(Residents.Active)) %>%
        filter(!is.na(Residents.Active)) %>%
        left_join(fed_pop_df) %>%
        rename(Residents.Population = Population) %>%
        mutate(Name = "BOP"),

    hist_imm %>%
        select(Date, Residents.Active) %>%
        group_by(Date) %>%
        summarise_all(sum_na_rm) %>%
        select(Date, Residents.Active) %>%
        left_join(pop_df) %>%
        mutate(Name = "ICE")) %>%
    mutate(Active.P = Residents.Active / Residents.Population * 10000) %>%
    ggplot(aes(x = Date, y = Active.P)) +
    geom_line(aes(color = Name), size = 2) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    geom_vline(xintercept = ymd("2020-12-22"), linetype = 3, size = 2) +
    labs(color = "", y = "Active COVID Infections\nPer 10,000 Detainees")

facs <- c(
    "ICE KARNES COUNTY RESIDENTIAL CENTER",
    "ICE SOUTH TEXAS FAMILY RESIDENTIAL CENTER",
    "ICE SOUTH TEXAS DETENTION COMPLEX"
)

hist_imm %>%
    filter(Date == max(Date)) %>%
    filter(State == "Texas") %>%
    select(Name, Residents.Active) %>%
    arrange(-Residents.Active) %>%
    pull(Name) %>%
    head(n=3) %>%
    {filter(hist_imm, Name %in% .)}

hist_imm %>%
    filter(Name %in% facs) %>%
    mutate(Name = case_when(
        Name == "ICE KARNES COUNTY RESIDENTIAL CENTER" ~ "Karnes County\nResidential Center",
        Name == "ICE SOUTH TEXAS FAMILY RESIDENTIAL CENTER" ~ "South Texas Family\nResidential Center",
        Name == "ICE SOUTH TEXAS DETENTION COMPLEX" ~ "South Texas\nDetention Complex"
    )) %>%
    filter(Date >= ymd("2021-01-01")) %>%
    ggplot(aes(x = Date, y = Residents.Active)) +
    geom_line(aes(color=Name), size = 2) +
    geom_vline(xintercept = ymd("2021-03-02"), linetype=2) +
    geom_text(
        aes(label = txt),
        size = 6,
        data = tibble(
            Date = ymd("2021-02-01"),
            Residents.Active = 140,
            txt="March 2, 2021\n TX Mask Mandate Lifted"
        )) +
    theme_behindbars() +
    labs(y = "Active COVID Cases", color = "") +
    scale_color_bbdiscrete()

fac_ts_plot <- hist_imm %>%
    filter(Date <= ymd("2021-04-11")) %>%
    mutate(Name = case_when(
        Name == "ICE ADAMS COUNTY DETENTION CENTER" ~ "ICE Adams\nDetention Center",
        Name == "ICE KARNES COUNTY RESIDENTIAL CENTER" ~ "ICE Karnes\nResidential Center",
        Name == "ICE SOUTH TEXAS DETENTION COMPLEX" ~ "ICE South Texas\nDetention Complex",
        Name == "ICE SOUTH TEXAS FAMILY RESIDENTIAL CENTER" ~ "ICE South Texas\nResidential Center",
        Name == "ICE WINN CORRECTIONAL CENTER" ~ "ICE Winn\nCorrectional Center"
    )) %>%
    plot_recent_fac_increases(plot_days = 20, metric = "Residents.Active") +
    labs(color="", y = "Active COVID-19 Infections")

ggsave("fac_ts.svg", fac_ts_plot, width = 10.7, height = 6.7)
ggsave("fac_ts.png", fac_ts_plot, width = 10.7, height = 6.7)
