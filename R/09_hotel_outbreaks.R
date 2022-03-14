library(tidyverse)
library(behindbarstools)

hist_imm <- read_scrape_data(TRUE) %>%
    filter(Jurisdiction == "immigration")

sub_hist <- hist_imm %>%
    filter(Facility.ID %in% c(2730, 2458, 2326)) %>%
    mutate(Name = case_when(
        Facility.ID == 2730 ~ "Wingate\nYuma,AZ",
        Facility.ID == 2458 ~ "La Quinta\nPearsall, TX ",
        Facility.ID == 2326 ~ "Best Western\nEl Paso, TX"
    ))

end_vals <- sub_hist %>%
    group_by(Name) %>%
    filter(Date == max(Date)) %>%
    pull(Residents.Active)



sub_hist %>%
    ggplot(aes(x = Date, y = Residents.Active, color = Name)) +
    geom_line(size = 2, alpha = .85) +
    labs(x = "Date", y = "Detainees With Active\nCOVID-19 Infections", color = "") +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = end_vals))
