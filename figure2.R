library(tidyverse)
library(readxl)
library(eurostat)
library(scales)
library(ggrepel)

load("colors.RData")
load("geo.RData")

## Load Eurostat datasets ------

datasets_eurostat <- c("nama_10_pc")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, stringsAsFactors = F, cache = F) |>
           rename(date = TIME_PERIOD)
  )
}


nama_10_pc %>%
  filter(unit == "PC_EU27_2020_HAB_MEUR_CP",
         na_item == "B1GQ",
         !(geo %in% c("EA12", "EA19", "EA20","EA", "LU", "LI", "NO", "CH", "IE"))) %>%
  select_if(~ n_distinct(.) > 1) %>%
  group_by(date) %>%
  left_join(geo, by  = "geo") %>%
  mutate(Geo = ifelse(geo == "EU27_2020", "Europe", Geo)) %>%
  left_join(colors, by = c("Geo" = "country")) %>%
  filter(geo != "EU27_2020") %>%
  mutate(color = ifelse(!(geo %in% c("EL", "FR", "IT", "PL")), "gray", color)) %>%
  ggplot(.) + theme_minimal() + scale_color_identity() +
  geom_line(aes(x = date, y = values, color = color, group = geo)) + 
  geom_line(data = . %>% filter(geo == "EL"), aes(x = date, y = values, color = color), size = 1.5) + 
  geom_line(data = . %>% filter(geo == "FR"), aes(x = date, y = values, color = color), size = 1.5) + 
  geom_line(data = . %>% filter(geo == "IT"), aes(x = date, y = values, color = color), size = 1.5) + 
  geom_line(data = . %>% filter(geo == "PL"), aes(x = date, y = values, color = color), size = 1.5) + 
  xlab("") + ylab("MEUR") +
  scale_x_date(breaks = seq(1995, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = seq(10, 400, 10)) +
  geom_text_repel(data = . %>% group_by(geo) %>% filter(date %in% c(max(date), min(date))),
                  aes(x = date, y = values, color = color, label = Geo)) +
  theme_minimal()

ggsave("figure2.png", width = 1.25*6, height = 1.25*3.375, bg = "white", dpi = 150)
ggsave("figure2.pdf", width = 1.25*6, height = 1.25*3.375, dpi = 150)

